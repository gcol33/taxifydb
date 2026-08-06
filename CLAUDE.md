# taxifydb — Build Pipeline (`taxifydb` package)

## What This Is

R package `taxifydb`: the build pipeline for the `taxify` runtime. Downloads
raw source data, normalizes to the unified Darwin Core-like schema, and writes
the pre-compiled `.vtr` files that `taxify` consumes at runtime.

Two-repo split:

- `taxify` (runtime) — matching engine, S3 dispatch, enrichment join glue.
  Lean. No build deps. Does NOT need `taxifydb` to function with pre-built
  `.vtr` downloads.
- `taxifydb` (this repo) — every download/parse/normalize/index step. Has
  heavy build deps (curl, openxlsx2, RSQLite, rfishbase, ...). Only required
  when a user wants to build `.vtr` files themselves.

## Architecture

```
R/normalize.R              — normalize_backbone(), resolve_hierarchy()
R/precompute.R             — precompute_keys(), embed_accepted(),
                              precompute_backbone()
R/build_vtr.R              — build_vtr(), sha256()
R/build_enrichment_vtr.R   — build_enrichment_vtr()
R/diff.R                   — has_xdelta3(), create_delta(), apply_delta()
R/resolve_names.R          — resolve_enrichment_names() (cross-backbone)
R/build_name_lookup.R      — build_name_lookup(),
                              build_all_name_lookups()
R/check_versions.R         — upstream version check helpers
R/publish.R                — publish_release(), update_manifest(),
                              update_enrichment_manifest()

R/backend-<name>.R         — per-backend download / read / build_<name>()
R/build_backend.R          — build_backend(name, ...) dispatcher,
                              list_backends()
R/backend-wgsrpd.R         — reference geometry (not a backbone): WGSRPD Level 3
                              botanical regions -> wgsrpd.vtr (plant range polygons)
R/backend-meow.R           — reference geometry (not a backbone): MEOW marine
                              ecoregions -> meow.vtr (marine range polygons, #21)
R/register.R               — genus extractors, kingdom normalization,
                              resolve_kingdom_via_gbif(), build_genus_register(),
                              build_backend_coverage(), build_register() (#23)
R/life-form.R              — family -> taxon_group / kingdom_group lookup table,
                              assign_life_form()

R/enrichment-helpers.R     — shared download helpers
R/enrichment-parsers*.R    — 24 parse_<name>() functions
R/enrichment-registry.R    — .enrichment_build_registry
R/build_enrichment.R       — build_enrichment(name, ...) dispatcher,
                              list_enrichments(),
                              enrichment_emergency_fallback()
R/tsita-vocab.R            — T-SITA controlled-vocabulary crosswalk: tsita_vocab(),
                              tsita_crosswalk(), .tsita_enrichment_meta() (#42)
R/betsi-recovery.R         — BETSI recovery: published BETSI-derived matrices ->
                              per-taxon enrichment assets. gen_spe(),
                              parse_betsi_recovery(), build_betsi_recovery(),
                              list_betsi_recovery(); frozen matrices in
                              inst/extdata/betsi/, data-raw/betsi_recovery.R (#42)
```

## Backends

15 backends. All built via the same `build_backend(name)` entrypoint.

| Backend | Format | Notes |
|---------|--------|-------|
| wfo | Zenodo ZIP / classification.txt | WFO 2024-12 snapshot |
| col | DwC-A TSV | Catalogue of Life |
| gbif | simple.txt.gz | GBIF backbone, denormalized hierarchy |
| itis | SQLite | parent_tsn walk, needs RSQLite |
| ncbi | pipe-delimited .dmp | aggressive noise filter |
| ott | TSV (Open Tree) | NCBI+GBIF+WoRMS+IRMNG synthesis |
| worms | DwC-A (ChecklistBank) | marine taxa, denormalized; ChecklistBank double-quotes TSV fields, so `read_worms` reads with `read.delim(quote = "\"")` not `quote = ""` (see WoRMS quote note below) |
| euromed | CDM REST snapshot (NDJSON) | Euro+Med PlantBase, harvested live from the EDIT CyberTaxonomy CDM API (`api.cybertaxonomy.org/euromed`) by `inst/py/crawlers/crawl_euromed.py` into a frozen `euromed.jsonl` + `nodes.tsv` snapshot (accepted taxa with nested synonyms; genus->family from node `treeIndex`). Replaces the frozen 2020 v1.2 flat file that could not refresh (#7). No working bulk export exists (the CDM `/dwca` and `/checklist/export` endpoints time out through the public proxy), so the per-taxon portal API is crawled |
| fungorum | (depends) | Index Fungorum |
| algaebase | ChecklistBank /nameusage/search | paginated API; /archive disabled (CC BY-NC) |
| fishbase | rfishbase `load_taxa()` + `synonyms()` | fishes; shared reader `.read_rfishbase_backbone()`; needs rfishbase |
| sealifebase | rfishbase (server = sealifebase) | non-fish aquatic; same shared reader |
| reptiledb | taxa.csv + synonyms.xlsx + checklist.xlsx | reptiles; CC-BY; synonyms from 2023-04 snapshot, order via family->order map; needs openxlsx2 |
| wcvp | pipe-delimited `wcvp_names.csv` (Kew) | vascular plants; CC BY; `taxon_name` is the rendered canonical (hybrids + infraspecific markers); acceptance derived from `accepted_plant_name_id` (self=accepted, other=synonym, empty=unplaced), not the nine `taxon_status` spellings. Kew does NOT field-wrap, and 0.06% of names carry genuine embedded `"` (e.g. `f. "A"`), so `read_wcvp` reads with `quote = ""` (opposite of WoRMS); optional data.table fread |
| lcvp | `tab_lcvp.rda` (idiv-biodiversity/LCVP) | vascular plants; MIT; loaded via base `load()` (no LCVP pkg dep); canonical assembled from `Input.Genus`/`Input.Epitheton`/`Rank`/`Input.Subspecies.Epitheton` (`nil` = species; `forma`->`f.`); synonym->accepted via `globalId.of.Output.Taxon`; `unresolved` kept as own accepted concept; no hybrids in input columns |

**WoRMS quote note (#2, fixed in `worms-2026.07`).** ChecklistBank double-quotes its `Taxon.tsv` / `SpeciesProfile.tsv` string fields. `read_worms` (and the SpeciesProfile reader) must use `read.delim(quote = "\"")`, NOT `quote = ""` — with `quote = ""` the field-wrapping quotes are kept as literal characters, so 90.2% of `canonical_name` / `key_ci` / `key_normalized` / `authorship` came out wrapped in `"` (e.g. `"Aglaophamus malmgreni"`) and some quoted-field rows were mis-split (bibliographic text leaking into `taxon_id`). Runtime effect: exact match fails on the quotes, falls to fuzzy, and the quoted `accepted_name` then breaks every marine enrichment join. Using `quote = "\""` parses the double-quotes as quotes and strips them; only `"` is a quote (not `'`), so apostrophes in authorship (`d'Orbigny`, `O'Brien`) stay intact. After the fix: `canonical_name` quotes 1,406,915 -> 11 (the 11 genuine embedded quotes, e.g. `Gyrodactylus barbatuli f. "A"`), rows 1,559,455 -> 1,557,860. This was WoRMS-only: every other backbone and all enrichments have <=0.03% quoted, all genuine embedded quotes in informal/provisional names, so none needs the change. When rebuilding another delimited backbone/enrichment whose source wraps fields, prefer `quote = "\""` over `quote = ""`.

## Enrichments

113 enrichments registered in `.enrichment_build_registry` (includes `fishbase`,
`sealifebase`, `groot`, `marine_distribution`, and the BETSI-recovery assets).
Ecoflora and FloraWeb are built into `.vtr` files
from frozen per-species scrape snapshots. Only 1 on-demand source remains
(Pignatti, copyrighted), catalogued in `.enrichment_scrape_only`
(`list_scrape_only_enrichments()`) and accessed by taxify's `add_pignatti()` via
the TR8 package. Licence-blocked candidate sources whose live terms permit
citation/scientific use only, not third-party redistribution, are recorded in
`.enrichment_build_only` (`list_build_only_enrichments()`) with each source's
licence provenance, so taxifydb builds no `.vtr`, writes no manifest entry, and
adds nothing to the cross-source trait registry for them: freshwaterecology.info
(#33; non-commercial, registration-gated, (c) BOKU -- read off the live
conditions.php) and NEMAPLEX (#31; UC Davis, licence-unstated: the live site
publishes no terms page, default copyright reserves it). A source leaves
that catalog for `.enrichment_build_registry` only if its live licence is
relicensed to permit redistribution. BETSI (#31, Collembola/soil inverts) was
catalogued here but reclassified OUT (#42): the decision is to serve it as
**informed-risk redistribution** -- licence unconfirmed, scientific reuse
intended, redistribution risk accepted -- not as a refuse-to-redistribute
source. It is not built yet only because there is no data on hand: the portal's
"Trait data request" page exports per-query CSV subsets but is currently
intermittently unreachable, and a known 2021 snapshot (`BETSI_220221.csv`, used
by the SLIME project) is not publicly committed. The serve decision, attribution
statement, provenance (Joimel et al. 2021 descriptor, Pey et al. 2014
thesaurus), and acquisition path (SLIME/Le Guillarme, Selenium harvest, or the
BETSI admins) are tracked in #42. Every built
enrichment goes through cross-backbone name resolution before its `.vtr` is
written:

1. `parse_<name>()` cleans the source to `canonical_name` + trait columns
2. `resolve_enrichment_names()` expands each name across the 7 backbones
3. `build_enrichment_vtr()` writes the indexed `.vtr` + `meta.json` sidecar

Group-based enrichments (GRIIS, WCVP, common_names, marine_distribution) pass
`group_cols` so deduplication respects the grouping column.

**Enrichment granularity: one source, many traits = columns; one trait, many
sources = separate enrichments.** Multiple traits from a single source are
columns in one enrichment (`pantheria`, `betsi_collembola_traits` with 9 trait
columns). The same trait recovered from several sources stays as separate
enrichments, never merged into a consensus column: body mass rides `pantheria`,
`elton_traits`, `animaltraits`, `amniote`, `phylacine`, `combine` and
`combine_imputed` as its own entry each ("shipped as its own enrichment, not as
a replacement for PanTHERIA or EltonTraits"), and the Collembola body-length
trio (`betsi_collembola_body_length`, `plazi_collembola_body_length`,
`monograph_collembola_body_length`) is the same pattern -- source-prefixed
columns (`betsi_body_length_mm` / `plazi_body_length_mm` /
`monograph_body_length_mm`) a consumer adds side by side, each carrying its own
licence, citation and `_n` / `_sources`. Merging multiple sources into ONE
enrichment is reserved for interchangeable labels with no per-source citation
obligation or cross-source comparison value: only `common_names` (GBIF + NCBI +
OTT) and `marine_distribution` (WoRMS + MEOW) do it. Keeping same-trait sources
apart is exactly what lets them be cross-validated against each other (Plazi vs
BETSI Pearson r = 0.906; Ellers vs the BETSI body-length floor r = 0.96).

**T-SITA trait vocabulary (#42).** The soil-fauna enrichments (Ellers,
ecomorphosis, the mined monographs, the BETSI body-length floor, and the
planned INRAE / monograph-payload multi-trait assets) each name their traits
their own way. `R/tsita-vocab.R` gives them one shared controlled vocabulary:
T-SITA (Pey et al. 2014, CC BY -- the thesaurus BETSI itself is built on, 71
traits + 24 ecological preferences with stable ARK URIs). Two `inst/extdata`
assets back it: `tsita_vocab.csv` (the frozen thesaurus, one row per concept,
regenerated by `data-raw/tsita_vocab.R`) and `tsita_crosswalk.csv` (enrichment
column, and categorical value, -> T-SITA prefLabel; labels only, so a label can
never drift from its URI, which is resolved from the vocab). `tsita_vocab.csv`
is pulled from the **live** thesaurus: the T-SITA portal (`t-sita.cesab.org`) is
dead -- it 301-redirects to the same squatter that swallowed `betsi.cesab.org`
-- but the SKOS is served, keyless, by the Opentheso instance behind the
CEFE/CNRS ARK `ark:/66666/th558` (`opentheso.huma-num.fr`), and mirrored on
AgroPortal (slug `T-SITA`). `build_enrichment_vtr()` attaches a `tsita` block to
each built `meta.json` (per column: `trait_label` / `trait_uri`, plus a
`values` map for categorical traits) via `.tsita_enrichment_meta(name)`, keyed
on the enrichment name in the crosswalk -- O(1) per new enrichment, no
per-parser code. A trait axis with no faithful T-SITA concept (Ellers'
biogeographic temperature-zone class and thermal-niche breadth; pigmentation;
pseudocelli) is left out of the crosswalk rather than forced onto a near-miss:
the column keeps its own name and carries no T-SITA identifier. taxify reads
`meta$tsita` to expose canonical trait names; an enrichment with no crosswalk
row simply writes no `tsita` field.

**BETSI recovery (#42).** BETSI's live portal is offline and no complete export
is recoverable, so `R/betsi-recovery.R` rebuilds the trait matrices individual
studies downloaded from BETSI and printed or deposited, into per-taxon assets.
Each source has a descriptor in `.betsi_recovery_sources`; `parse_betsi_recovery()`
dispatches on its `shape`. A `"fuzzy"` matrix (per-species % affinities across a
trait's modality bins) is validated (trait set, modalities, and the sum-to-100
invariant, all hard errors) and pivoted to one numeric column per
`<trait>__<modality>` bin (0-100) -- the full fuzzy vectors are kept, not
collapsed. A `"hard"` matrix (one scalar/categorical value per trait) is read and
type-checked into one column per trait. Provenance is per column, not per row: a
source can carry BETSI-derived columns beside its own primary ones, so each
column's tier (`betsi_export` / `betsi_derived` / `literature_reconstruction` /
`source_study`) is written to `meta.json`'s `provenance` block by
`.betsi_recovery_provenance()` (via `build_enrichment_vtr(provenance = )`), never
flattened onto rows it does not describe. The frozen matrices live in
`inst/extdata/betsi/` (committed; regenerated from the local
`dev_notes/betsi_recovery/` sources by `data-raw/betsi_recovery.R`), and each
recovery asset is a normal `.enrichment_build_registry` entry (with a
`provenance_fn`) built through the shared pipeline. `build_betsi_recovery()` /
`list_betsi_recovery()` scope the recovery subset; `gen_spe()` builds the
6-letter `GEN_SPE` code the code-keyed matrices key on; `resolve_betsi_codes()`
decodes the legend-less INRAE codes against a Collembola reference pool, with
`inrae_genus_dict()` supplying the bespoke genus abbreviations and the unresolved
dropped, never guessed. Assets: `betsi_earthworm_traits`
(Pelosi 2014, 11 earthworm species x 7 fuzzy traits, gap G2), `betsi_collembola_traits`
(Lu 2025, 26 Collembola species, hard-value; 6 BETSI-derived traits + 3 the study
measured itself, gap G1 -- pigment/ocelli/furca), and `inrae_collembola_traits`
(the two Data INRAE deposits UU2FQT/UCYSLH decoded and merged, 135 Collembola
species x 7 shared fuzzy traits -- ocelli, furca, post-antennal organ,
pigmentation, body shape, scales, reproduction, gap G1). The INRAE source is
`sparse`: it records different traits for different species (post-antennal organ
for a minority), so the fuzzy parser keeps a wholly-absent trait block as NA but
rejects a partial one, rather than dropping the species to force a complete
rectangle. Body length (incompatible bin schemes across the two deposits, covered
by four dedicated length assets), trichobothria (one deposit only) and
ecomorphosis (its own asset) are not carried.

`marine_distribution` is the marine analogue of the WCVP range table (#21): a
`canonical_name` + `region_code` + `native_status` asset so taxify's
`region=`/`coords=` filter can constrain animal/marine matches, not just plants.
WoRMS distributions are not in the ChecklistBank export, so they are harvested
per taxon from the WoRMS REST API (`inst/py/crawlers/crawl_worms_distributions.py`,
a multi-day throttled crawl like euromed) keyed on Marine Regions localities
(MRGID). `inst/py/crawlers/crosswalk_mrgid_meow.py` rolls each MRGID up to the
MEOW ecoregion(s) it falls in (point-in-polygon against the frozen MEOW
GeoJSON), and `parse_marine_distribution()` joins the two frozen snapshots. Its
`region_code` is the MEOW ECO_CODE, the same key `backend-meow.R`'s `meow.vtr`
geometry is indexed on, so the runtime coords->region path is a drop-in beside
`wgsrpd.vtr`. Both the WoRMS snapshot and the MEOW GeoJSON are frozen as
`marine-snapshots-*` release assets (the WoRMS full copy is request-gated and
the MEOW download is a form-gated shapefile with no stable URL).

`gidias` carries two grains in one `.vtr`, keyed on `affected_taxon`: `"Any"`
(every record for the species) plus one row per affected native taxon (`Plant`,
`Invertebrate`, `Vertebrate`, `Microbe`, `Fungi`). The `"Any"` row is not the
union of the others and cannot be dropped — it is the only one carrying SEICAT
and the negative records with no affected taxon recorded, so a door reading
gidias without a group must select `affected_taxon == "Any"`.

## Genus Register

`genus_register.vtr` (one row per genus: classification + `kingdom_group` /
`taxon_group` / `life_form`) and `backend_coverage.vtr` (long format, one row
per genus x backend) are the cross-backbone index `taxify()`'s
`kingdom_group`/`taxon_group`/`life_form` output and `inspect()` read (#23).
Both are built here, not on the taxify runtime side, so every user downloads
the same published register regardless of which backbones they have
installed locally — taxify's own `build_genus_register()` used to assemble it
from whatever the caller happened to have on disk, so two users running
identical code could get different `kingdom_group`/`taxon_group` output.

`build_register()` (or `build_genus_register()` / `build_backend_coverage()`
individually) unions the fixed 13-backbone set `register_backbones()` returns
(every backbone except Fungorum and AlgaeBase) via the extractor registry in
`R/register.R`. Each backbone's `.vtr` resolves, in order: an explicit
`backbone_paths` override, a local `output/<name>/<name>.vtr` build, or the
version published in `manifest/manifest.json` (downloaded into
`<output_dir>/_cache/`). Classification conflicts resolve by backbone
priority (WoRMS > COL > WCVP > Reptile DB > GBIF > Euro+Med > LCVP > ITIS >
NCBI > OTT > WFO > FishBase > SeaLifeBase); `kingdom_group`/`taxon_group`/
`life_form` come from `R/life-form.R`'s family lookup table, with a GBIF
parent-key hierarchy walk (`resolve_kingdom_via_gbif()`) as a second pass for
genera the family table cannot place. Each `.vtr` publishes under its own
release tag like a backbone (`genus_register-<version>`,
`backend_coverage-<version>`), and both are recorded in `manifest.json`'s
`backends` block (alongside `wgsrpd`/`meow`, the other non-taxonomic
reference assets built there).

## Building locally

```bash
# Install the package
Rscript -e "devtools::install_local('.', upgrade = 'never')"

# Build one backend
Rscript build_all.R itis output/itis

# Build all backends
Rscript build_all.R all output

# Build one enrichment
Rscript build_enrichments.R woodiness

# Build all enrichments
Rscript build_enrichments.R all

# Publish (after build)
Rscript build_all.R publish itis 2026.05
```

Direct package API (equivalent):

```r
library(taxifydb)
build_backend("itis", output_dir = "output/itis")
build_enrichment("woodiness", output_dir = "output/enrichment/woodiness")
update_manifest("manifest/manifest.json", "itis", "2026.05",
                "output/itis/itis.vtr")
```

## CI

Three workflows under `.github/workflows/`:

- `build-light.yml` — Ubuntu, the 4 light backends (ITIS, NCBI, OTT, WoRMS),
  twice a year + manual dispatch
- `build-heavy.yml` — Windows self-hosted, the 6 heavy backends (WFO, COL,
  GBIF, Euro+Med, Fungorum, AlgaeBase), same cadence
- `check-enrichment-versions.yml` — weekly cron, opens/updates a GitHub
  issue labeled `enrichment-outdated` when upstream versions advance

All workflows install `taxifydb` from the repo's source (`devtools::install_local(".")`)
and call the package API directly.

## Dependencies

- R >= 4.1
- Imports: vectra, curl, digest, jsonlite, utils
- Suggests: DBI, RSQLite (ITIS only); openxlsx2 (xlsx enrichments);
  rfishbase (fishbase only); taxify (cross-backbone name resolution)
- System: xdelta3 (for binary diffs); gh CLI (publishing)
