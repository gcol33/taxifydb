# taxifydb 0.1.21

## A rebuild that changed nothing no longer cuts a release

* Builds stamp `date +%Y.%m`, which records when a build ran rather than what
  it read. Several backbones read a pinned source and rebuild to identical
  bytes every time -- Euro+Med from a frozen snapshot release, WFO from a fixed
  Zenodo record, COL from the pinned annual archive -- so releasing them again
  minted a version whose only difference was its name. taxify's runtime treats
  a fresh version as reason to refetch, so every user downloaded a file they
  already held: 740 MB of it for WFO.

* New `vtr_changed()` compares the built `.vtr` against the `full_sha256` the
  manifest already records, and both workflows gate the delta, release,
  manifest commit and runtime sync behind it. It fails open -- no manifest, no
  entry, no recorded hash, a first-ever build all count as changed -- so an
  undecidable case still publishes. A skipped release is reported as a run
  notice rather than passing silently.

## Six backbones moved off a runner that does not exist

* `build-heavy.yml` targets `self-hosted` and no runner is registered on the
  repository, so no job of its has ever been picked up. WFO, WCVP, LCVP,
  Fungorum, AlgaeBase and Euro+Med all sit below OTT (3.7M rows) and NCBI
  (2.8M), which build on the hosted runner today, and need no dependency the
  light workflow lacks. They move to `build-light.yml`, leaving COL, COL XR and
  GBIF. Euro+Med built there in 2m41s.

* Both `build-heavy.yml` crons are held while it has no runner. A schedule that
  queues a job nothing can pick up produces a stuck run a month, which the COL
  XR monthly cron had been doing since it was added.

## A release without sidecars no longer deletes the ones on record

* `update_manifest()` cleared the `extras` block whenever no sidecar was
  passed, by analogy with the stale delta it drops on the line above. The two
  are not alike: a delta URL names the release being written, so an old one
  would 404, while a sidecar records the tag it was published under and stays
  reachable across releases that do not carry one. The manifest is the only
  record of that URL, so clearing it is how the runtime stops downloading the
  file. Passing no extras now leaves the block alone; `character(0)` removes
  it.

* Both build workflows upload whatever sidecar the build wrote beside the
  backbone, and record it. Neither had, which is why
  `worms_species_profile.vtr` sat at `worms-2026.05` while its own backbone
  reached 2026.08 -- and 14,227 rows short, since it is read by the same fixed
  reader. Republished at 1,562,065 rows, one per taxon.

* The light build, the heavy build and the taxify runtime sync each rebuilt the
  release's artifact paths in their own embedded R one-liner, which is how they
  came to disagree about what a release carries. They now share
  `scripts/update_manifest_entry.R`.

## WoRMS reads short, and the streaming feed learns why (#43)

* `read_worms()` no longer uses `utils::read.delim()`, which cannot read the
  WoRMS taxon core at all. `Taxon.tsv` carries 4,626 newlines and 59 lone
  carriage returns inside quoted fields; R reads a lone `CR` as the end of a
  line, and in text mode loses bytes doing it, so the file's 15,419,038 quote
  characters come back as 15,418,989. The quoting stops balancing, `scan()`
  reaches a record it cannot close, and it returns 1,363,240 of 1,562,065 rows
  -- 198,825 marine taxa missing, the last few hundred filled with fragments of
  the citation that broke the parse -- reporting it only as a warning. The
  reader is now `data.table::fread()` followed by `unescape_quotes()`, since
  `fread()` leaves RFC 4180's doubled quote as it found it
  (Rdatatable/data.table#1109) where `read.delim()` collapses it.

  Published as `worms-2026.08`: 1,562,065 rows, against 1,557,860 in
  `worms-2026.07`.

* `assert_worms_taxon_core()` fails the build when a `taxonID` is missing or
  repeated. A reader stopping partway through leaves the rows it did return
  well formed, so the identifiers are what show that a record was cut in two.

* `build_worms()` streams. The taxon core is over a gigabyte unpacked and was
  the largest backbone still assembled in memory.

* `delim_block_feed()` no longer parses with `read.delim()`, and the choice of
  reader no longer decides what a value ends up as. Blocks are parsed with
  `fread()` and the RFC 4180 unescaping is done in `unescape_quotes()`, so it
  holds whichever source is read. `delim_lf_reader()` splits blocks on `LF` or
  `CRLF` and never on a bare `CR`, and a block reaches the parser as a file
  rather than as text, since `fread(text=)` splits its input into lines itself
  and so cannot carry a record that spans them.

  Verified against both real sources: WFO is identical to its previous read
  across all 1,638,552 rows and all 20 columns, and WoRMS streams to the same
  1,562,065 rows and 14 columns as the whole-file read, building to 115,741
  genera in 115,741 contiguous runs with all 857,427 synonyms resolved.

## BETSI recovery: earthworm and Collembola trait matrices (#42)

* `R/betsi-recovery.R` rebuilds published BETSI-derived trait matrices into
  per-taxon enrichment assets, since BETSI's live portal is offline and no
  complete export is recoverable. `build_betsi_recovery()` /
  `list_betsi_recovery()` scope the recovery subset; `parse_betsi_recovery()`
  dispatches on each source's matrix shape; `gen_spe()` builds the six-letter
  species code the code-keyed matrices use.

* `betsi_earthworm_traits` (gap G2): Pelosi et al. (2014) Appendix 1, 11
  earthworm species x 7 fuzzy-coded traits. Each (species, trait) affinity block
  is checked to sum to 100 on ingest, and the full fuzzy vectors are kept as one
  numeric column per `<trait>__<modality>` bin.

* `betsi_collembola_traits` (gap G1): Lu et al. (2025) Appendix S1, 26 Collembola
  species. Fills pigmentation, number of ocelli and furcula, which had no prior
  Collembola coverage. Six traits are BETSI-derived; vertical distribution,
  trophic guild and life form were measured by the source study.

* Provenance is recorded per column in each asset's `meta.json` `provenance`
  block (`betsi_export` / `betsi_derived` / `literature_reconstruction` /
  `source_study`), never flattened onto rows a tier does not describe.
  `build_enrichment_vtr()` gains a `provenance` argument that the registry
  supplies through a per-enrichment `provenance_fn`.

# taxifydb 0.1.20

## Enrichment name resolution spans all fifteen backbones

* `resolve_enrichment_names()`, `resolve_name_map()`, `.resolve_species_names()`
  and `build_all_name_lookups()` now default to every taxify backbone
  (`list_backends()`), up from the seven broadest. An enrichment `.vtr` built
  after this change carries the accepted-name variants from the domain-specific
  backbones too (Euro+Med, Species Fungorum, AlgaeBase, FishBase, SeaLifeBase,
  Reptile Database, LCVP, WCVP), so an enrichment join through one of those
  backbones no longer falls through.

* `resolve_name_map()` warns when a requested backbone lacks a
  `name_lookup.vtr`, so a production build cannot silently resolve against a
  narrower backbone set than intended.

## Build-only enrichments are never published

* `build_enrichments.R` no longer publishes or writes a manifest entry for a
  build-only enrichment (ccdb, gmpd, plantatt, bryoatt, clopla): `publish all`
  and `all` skip them, and `publish <name>` refuses a build-only name. These
  sources carry a citation-only or unstated licence and are built locally only.

# taxifydb 0.1.19

## `build_register()` reaches installations that resolve by version

* `build_register()` was added to 0.1.18 after 0.1.18 had already been built
  and cached, so anything resolving taxifydb by version string kept serving the
  earlier code and `taxifydb::build_register` was missing from it. taxify's
  register fallback calls that function, so the call failed wherever the older
  0.1.18 was installed. The version now identifies the code that carries it.

# taxifydb 0.1.18

## Marine distribution asset (issue #21)

* New `marine_distribution` enrichment: the marine analogue of the WCVP range
  table, so taxify's `region=`/`coords=` filter can constrain animal and marine
  matches, not just vascular plants. It keys `canonical_name` on a MEOW
  ecoregion `region_code` with a native/introduced status.

* WoRMS distributions are not in the ChecklistBank/GBIF export (the per-taxon
  distribution endpoint returns nothing there), so they are harvested per taxon
  from the WoRMS REST API by `inst/py/crawlers/crawl_worms_distributions.py`,
  keyed on Marine Regions localities (MRGID).
  `inst/py/crawlers/crosswalk_mrgid_meow.py` rolls each MRGID up to the Marine
  Ecoregions of the World (Spalding et al. 2007) it falls in by point-in-polygon
  against the frozen MEOW GeoJSON, and `parse_marine_distribution()` joins the
  two frozen snapshots, dropping records that report non-presence or that WoRMS
  flags as doubtful or inaccurate.

* New reference-geometry backend `build_meow()` writes `meow.vtr` (MEOW
  ecoregion boundary polygons), the marine counterpart of `build_wgsrpd()`,
  indexed on the same ECO_CODE the enrichment's `region_code` uses so the
  runtime coordinate-to-region path is a drop-in.

* `build_meow_geojson.py` reads each ecoregion's province and realm from the
  Marine Regions gazetteer hierarchy instead of inferring them by point-in-
  polygon on the ecoregion centroid. MEOW nests an ecoregion in exactly one
  province and a province in exactly one realm, but assigning the two
  independently produced combinations that cannot co-occur (a Cold Temperate
  Northwest Pacific province under a Central Indo-Pacific realm) and misplaced
  any ecoregion whose centroid falls outside its own polygon or spans the
  antimeridian -- the Aleutian Islands were filed under the Lusitanian province
  of the Temperate Northern Atlantic, and the Chukchi and Beaufort Seas under
  Tropical Eastern Pacific. 39 of the 232 ecoregions carried a wrong province
  or realm. Reading the stated hierarchy yields exactly MEOW's published 232
  ecoregions / 62 provinces / 12 realms, with every province resolving to a
  single realm. The geometry is unchanged, so `meow.vtr` is unaffected.

* `parse_marine_distribution()` counts every WoRMS `establishmentMeans` variant
  that states nativeness as native. WoRMS qualifies the value where it can, so
  matching `"Native"` exactly scored the 16,663 `"Native - Endemic"` and 4,375
  `"Native - Non-endemic"` records as unknown origin and left `range_mode =
  "native"` missing a sixth of its evidence.

* `crosswalk_mrgid_meow.py` handles ecoregions that cross the antimeridian.
  Thirteen of the 232 run to both -180 and +180, so in the raw longitude framing
  they measure 360 degrees wide and their centre lands at longitude 0: the
  Aleutian Islands centred on the English Channel, Kamchatka and the Eastern
  Bering Sea on the North Sea and Norway. Every coarse bounding box over Europe
  therefore swept them in, attaching Pacific and Arctic ecoregions to European
  records -- and "European Marine Waters" alone carries 19,763 of them. Such a
  region is now held in 0..360 space, where it is contiguous, and query points
  are shifted to match. An MRGID bounding box that crosses the antimeridian is
  read as the union of its two halves rather than one interval.

* `crosswalk_mrgid_meow.py` splits into a `fetch` phase that caches each MRGID's
  gazetteer record and an `assign` phase that recomputes the crosswalk from that
  cache. Revising how a locality maps to an ecoregion is now seconds of local
  work instead of another throttled pass over 7,112 gazetteer records.

* `crosswalk_mrgid_meow.py` discards a locality that covers every ecoregion.
  "World", "World Oceans" and "High Seas" all roll up to the complete set of
  232, which records a species as present everywhere and constrains nothing
  while costing a row per ecoregion per species; "High Seas" alone accounted for
  49,184 of them. This is the degenerate case rather than a threshold, so
  regions that cover much of the globe without covering all of it are kept: the
  hemispheres at 119 and 113 of 232, the ocean basins, and "European Marine
  Waters" still carry information, and their breadth is a fact about the record.

* `parse_marine_distribution()` also drops the occurrence values that report
  non-presence rather than only `"Absent"`: a record retracted as `"Recorded in
  error"`, a population that is `"Extirpated"` or `"Eradicated"`, and one held
  only `"In captivity/cultivated"` are not evidence that a species lives in a
  region. Borderline values (`"Uncertain"`, `"Sometimes present"`) still count
  as presence, since the filter is a soft disambiguation aid.

# taxifydb 0.1.17

## Documentation

* Added a package README covering the build API, the two-repo split with
  `taxify`, data hosting, requirements, and how to add a backbone or enrichment.

* The DESCRIPTION `Description` field now names all 15 backbones; it had listed
  only the original 12 and omitted FishBase, SeaLifeBase, and the Reptile
  Database.

# taxifydb 0.1.16

## Cloudflare-gated builds find a curl_cffi Python without a manual override

* `.cf_python()` no longer relies on PATH alone to locate an interpreter with
  `curl_cffi`. Under `Rscript`, Rtools prepends its own `usr/bin/python` (which
  lacks `curl_cffi`) to PATH, so the previous PATH-only probe picked the wrong
  interpreter and errored even when a suitable python was installed -- forcing a
  manual `TAXIFYDB_PYTHON`. Discovery now also scans pyenv-managed versions
  (newest first, honouring `PYENV_ROOT`, both `pyenv-win` and unix layouts) and
  interpreters registered with the Windows `py` launcher, then falls back to
  PATH. The first candidate that can import `curl_cffi` wins, and the error
  lists everything it tried. Building `hosts`, `usda_fungus_host` and `clopla`
  now works out of the box on a pyenv or `py`-launcher setup.

# taxifydb 0.1.15

## Enrichment runtime manifest fields populated from the build

* A new enrichment's runtime manifest entry is now complete straight from the
  build, with no manual curation step. `build_enrichment_vtr()` records
  `trait_cols` (the built `.vtr` columns, minus the join key and group column),
  `static`, and `source_format` in `meta.json`, and
  `update_enrichment_manifest(runtime = TRUE)` populates `trait_cols`,
  `species_col`, `static`, `source_format`, and `citation` (from the registry
  attribution) on the runtime entry.

* Runtime fields are filled only when absent, so a hand-curated value in an
  existing entry is preserved; only the build-side manifest is written without
  them (`runtime = FALSE`, the default), keeping it lean. The enrichment publish
  path and the `taxify-enrichment-manifest-sync` action pass `runtime = TRUE`
  when writing taxify's `inst/manifest.json`.

* `build_enrichment_vtr()` gains `species_col`, `static`, and `source_format`
  arguments; the registry may set them per enrichment (defaults: species-grain,
  frozen snapshot).

# taxifydb 0.1.14

## Automated enrichment publishing

* New `publish_enrichment_release()` uploads enrichment `.vtr` files to the
  shared, rolling `enrichment-<version>` release. The release is created only
  when absent and never deleted; assets upload with `--clobber`, replacing only
  the same-named file so other enrichments' assets stay in place.

* New `build_enrichments.R publish <name|all> <version>` action builds the
  enrichment(s), uploads to the release, and updates *both* manifests in one
  step -- this repo's build-side `manifest/manifest.json` and, when `../taxify`
  is checked out alongside, taxify's runtime `inst/manifest.json`. This closes
  the root cause behind #20: previously the publish path had no single step that
  moved both manifests together, so they could drift.

* New `publish-enrichment.yml` workflow (dispatch) does the same in CI for
  headlessly-buildable enrichments: build, upload, commit the build-side
  manifest, and sync taxify's runtime manifest via a PR (new
  `taxify-enrichment-manifest-sync` action, mirroring the backbone sync).

# taxifydb 0.1.13

## Manifest fixes

* `update_enrichment_manifest()` now records the rolling release version in
  `latest` (matching the `enrichment-<latest>` download tag) and the dataset's
  own version in a new `source_version` field. Previously `latest` held the
  dataset version, so the build-side manifest disagreed with taxify's runtime
  manifest on every entry whose dataset version differs from the release.

* Regenerated `manifest/manifest.json` so all 89 enrichment entries match the
  current `enrichment-2026.07` release (they had lagged `enrichment-2026.05`;
  #20). The enrichment publish path had updated taxify's runtime manifest but
  not this build-side copy, leaving it a stale second source of truth.

* `scripts/check_manifest_coverage.R` (the weekly coverage cron) now also
  compares the two manifests' enrichment entries directly, so a publish that
  updates one manifest but not the other surfaces the same day instead of
  lying dormant. Enrichments share one rolling tag, so there is no per-entry
  release tag to compare against as with backbones.

# taxifydb 0.1.12

## New enrichment

* GIDIAS (Bacher et al. 2025), the IPBES invasive-species assessment's global
  impact compilation, is added as a per-species enrichment (`gidias`, CC BY 4.0).
  Only derived aggregates are distributed, not the raw impact records:
  `parse_gidias()` reduces each species' impact records to its IUCN EICAT
  environmental-impact category and SEICAT socio-economic-impact category (each
  the most severe magnitude among the species' negative impacts; the per-record
  global-extinction flag splits EICAT magnitude 3 into Major and Massive), plus
  the driving mechanism, affected well-being constituents, realms, and
  record/source counts. Names are resolved to the accepted grain inside
  `parse_gidias()` (the InvaCost / GloBI rollup pattern), so a species whose
  records are split across a synonym and its accepted name keeps its full
  evidence (#10).

# taxifydb 0.1.11

## New enrichment

* InvaCost (Diagne et al. 2020), the economic costs of biological invasions, is
  added as a per-species enrichment (`invacost`, CC BY 4.0). Only derived
  aggregates are distributed, not the raw estimate rows: `cost_total_usd` (each
  estimate's standardised annual 2017-USD cost expanded across its documented
  impact period and summed, overlapping estimates not de-duplicated), `cost_n`,
  and the dominant `cost_type`. Names are resolved to the accepted grain inside
  `parse_invacost()` and the cost is summed there (the GloBI rollup pattern), so
  a species whose costs are split across a synonym and its accepted name keeps
  its full total (#9).

## PHYLACINE mass provenance

* `parse_phylacine()` keeps the source `Mass.Method` flag as `mass_method` and
  adds a coarse `mass_method_class` (`reported` / `estimated` / `imputed`), so a
  phylogenetically imputed or allometrically estimated body mass is not served as
  a measurement where PHYLACINE is a species' only source (#11).

## Publishing

* `update_enrichment_manifest()` now records the `.vtr` `content_id` (its md5),
  matching `update_manifest()` for backbones, so a same-tag enrichment republish
  refreshes an otherwise version-locked runtime cache.

# taxifydb 0.1.10

## Backbone fixes

* The Euro+Med backbone is repointed from the frozen germansl 2020 v1.2 flat
  file (which could not refresh) to the live EDIT CyberTaxonomy CDM API that
  backs europlusmed.org. `inst/py/crawlers/crawl_euromed.py` harvests the full
  classification through the per-taxon portal API (the CDM bulk-export endpoints
  time out through the public proxy and no bulk Euro+Med exists on
  GBIF/COL/ChecklistBank) and freezes it as an `euromed.jsonl` + `nodes.tsv`
  snapshot, the same pattern as the ecoflora/floraweb scrape snapshots.
  `read_euromed()` builds the unified backbone from the snapshot: accepted taxa
  with nested synonyms resolved to their accepted concept, authorship and
  epithets parsed from the rendered name, and family mapped to each genus from
  the node `treeIndex` materialised path. Taxa the portal does not publish
  (`publish = false`) are skipped. The version pin moves from `2020.1` to
  `2026.07`.

## Manifest

* The LCVP (`lcvp-3.0.1`, 1,337,891 names), WCVP (`wcvp-2026.06`, 1,448,984
  names), and Reptile Database (`reptiledb-2026.06`, 50,043 names) backbones
  are now in `manifest/manifest.json`. LCVP and WCVP had no published `.vtr`
  release at all, so they could previously only be built from source; both are
  now built and released, and all three entries carry the full metadata
  (`nrow`, `full_size`, `full_sha256`, `content_id`, `source_url`, citation,
  license). The manifest now lists every one of the fifteen backbones.

# taxifydb 0.1.9

## Manifest fixes

* `update_manifest()` now fills a backbone's `source_url` from the `url` field
  of the `.meta` sidecar that `build_vtr()` writes next to every `.vtr`, so the
  manifest URL always matches what the build downloaded from (each backend's
  single `.<name>_url` constant). Previously `source_url` was an optional
  argument that no build path passed, so it was hand-curated and present for
  only some backbones. The `euromed`, `fungorum`, and `algaebase` entries gain
  their `source_url`, and `ncbi`'s stale URL (`.../new_taxdump.tar.gz`, which
  the server does not serve) is corrected to the path the builder actually
  downloads (`.../new_taxdump/new_taxdump.tar.gz`). Adds an internal
  `read_meta()` reader for the sidecar; this mirrors the enrichment path, which
  already derives `source_url` from the built `meta.json`.

# taxifydb 0.1.8

## New backbones

* `build_wcvp()` builds the World Checklist of Vascular Plants backbone
  (`wcvp`) from Kew's pipe-delimited `wcvp_names.csv` (CC BY). The canonical
  name is taken straight from `taxon_name` (hybrid signs and infraspecific
  markers already rendered), and acceptance is derived from the
  `accepted_plant_name_id` link (a name pointing at another is a synonym; a
  self-pointing or unplaced name is its own accepted concept) rather than the
  nine `taxon_status` spellings. `read_wcvp()` reads with `quote = ""` so the
  0.06% of names carrying a genuine embedded double-quote (informal epithets
  like `f. "A"`) stay literal; it uses `data.table::fread()` when available and
  falls back to `utils::read.delim()`.
* `build_lcvp()` builds the Leipzig Catalogue of Vascular Plants backbone
  (`lcvp`) from the `idiv-biodiversity/LCVP` `tab_lcvp.rda` (MIT), loaded with
  base `load()` so no LCVP package dependency is needed. The canonical name is
  assembled from `Input.Genus` / `Input.Epitheton` / `Rank` /
  `Input.Subspecies.Epitheton` (`nil` marks species rank; `forma` renders as
  `f.`); the synonym to accepted link comes from `globalId.of.Output.Taxon`,
  and `unresolved` names are kept as their own accepted concept.
* Both are wired into `build_backend()`, `list_backends()`, and the heavy
  build workflow, bringing the backbone count to 15, and carry
  `check_wcvp_version()` / `check_lcvp_version()` upstream-version checks. Adds
  `data.table` to Imports.

## Backbone fixes

* `read_worms()` (and the SpeciesProfile reader) now parse ChecklistBank's
  double-quoted `Taxon.tsv` / `SpeciesProfile.tsv` with `read.delim(quote =
  "\"")` instead of `quote = ""`. Previously the field-wrapping quotes were
  kept as literal characters, so 90.2% of `canonical_name`, `key_ci`,
  `key_normalized`, and `authorship` came out wrapped in `"` and some
  quoted-field rows were mis-split, which broke exact matching (falling to
  fuzzy) and every marine enrichment join. After the fix, wrapped
  `canonical_name` quotes drop from 1,406,915 to 11 (the genuine embedded
  ones), and only `"` is treated as a quote so authorship apostrophes
  (`d'Orbigny`) stay intact. WoRMS rebuilt under `worms-2026.07`.
* `publish` now stamps a `content_id` (md5 of the built `.vtr`) for each
  backbone in the manifest, mirroring the enrichment `content_id`, so taxify's
  runtime can detect a same-tag republish and refresh a version-locked static
  cache instead of serving stale data.

## New enrichments

* A further wave of trait and interaction recipes brings the enrichment
  registry to 91: `bacdive`, `globi`, `italic` (lichens), `hosts`,
  `usda_fungus_host`, `edwards_phyto`, `kew_sid`, `thermofresh`, `ramond`,
  `fw_insects_conus`, `eurobat`, `copepod_traits`, `fishtraits`,
  `cefas_btrait`, `kew_cvalues`, `epa_freshwater`, `ccdb`, `gmpd`, `plantatt`,
  `bryoatt`, and `clopla`. Run `taxifydb::list_enrichments()` for the full set;
  pre-built `.vtr` artifacts are published under the `enrichment-2026.07` tag.
  The GloBI, BacDive, and ITALIC builds use resume-safe crawlers.

## New features

* `parse_elton_traits()` now derives a `diet_guild` column from the ten
  EltonTraits diet-fraction columns: fractions are summed within each guild
  (the four vertebrate/fish columns are all carnivory), the dominant guild
  wins when it reaches 50 percent, otherwise the species is omnivore. The
  derived label agrees 93% with EltonTraits' own `diet_5cat` and 83% with
  AVONET's independent `trophic_niche` on shared species. This lets the taxify
  runtime coalesce EltonTraits (birds and mammals) into the cross-source
  `diet_guild` trait alongside AVONET and ReptTraits. Requires rebuilding
  `elton_traits.vtr` (`enrichment-2026.07`).
* `parse_birdbase()` now derives a `clutch_mean` column as the NA-safe mean of
  the reported clutch min and max, so the taxify runtime can coalesce Birdbase
  bird clutch sizes into the cross-source `clutch_litter_size` trait (calibrated
  1:1 against the Amniote database on 6781 shared species). Requires rebuilding
  `birdbase.vtr` (`enrichment-2026.07`).

## Bug fixes

* `build_enrichment_vtr()` now records a `content_id` (md5 of the built `.vtr`)
  in each enrichment's `meta.json`, and `sync_manifest.R` propagates it into
  taxify's manifest. This lets taxify's runtime detect a same-tag republish (a
  rebuilt asset re-uploaded under an unchanged release tag) and refresh an
  otherwise version-locked static cache offline, instead of serving stale data
  forever. Every rebuilt enrichment carries the id from this release onward.
* `parse_groot()` now repairs GRooT's `specific_root_area` column. GRooT's
  species aggregate carries three source papers (Quanquan 2011, Mokany & Ash
  2008, Chanteloup & Bonis 2013) at ~1000x below GRooT's documented cm2 g-1
  standard (data paper Table 1/S1, median 385.8) -- physically impossible for
  fine-root SRA -- which corrupts the aggregate mean for any co-measured species.
  This is a compilation unit error, not GRooT's conversion (AusTraits carries the
  identical Mokany 2008 data equally low). The x1000 correction is grounded in the
  primary literature: Mokany & Ash 2008's own SRA-SLA regression (Fig 1B) puts the
  real magnitude at ~10 m2/kg = ~100 cm2/g, and the stored values x1000 land in
  that range. The build now also fetches the full per-record version
  (`download_groot()`) and recomputes specific root area from it with the three
  papers rescaled x1000, standardized sources winning per species -- a species
  with any standardized record keeps its clean median (Mokany's paper itself
  cautions its pot-grown values differ from the field), and the rescaled papers
  fill only species no standardized source covers. The repaired column has median
  386 cm2 g-1 (matching the data paper), full species coverage, and no physically
  impossible sub-1 values. Every other GRooT trait is unchanged.

## New features

* New `gift` enrichment: `parse_gift()` fetches GIFT's (Global Inventory of
  Floras and Traits; Weigelt et al. 2020) species-level plant traits from the
  live API at build time -- batched over all trait IDs -- and writes them to a
  `.vtr` so the taxify runtime joins them offline. Only the redistributable
  subset the API returns is included (CC BY 4.0; restricted references are
  excluded by the default call). Registered in the enrichment build registry;
  requires the `GIFT` package to build from source.

## Fixes

* Corrected the FishBase and SeaLifeBase enrichment license to CC BY-NC 4.0
  (was mislabelled CC BY-NC 3.0).

# taxifydb 0.1.6

## New features

* `build_reptiledb()` builds the Reptile Database backbone (`reptiledb`): the
  reptarium taxa export plus the synonym snapshot and checklist, normalized to
  the unified backbone schema (~12.6k accepted reptile species + ~34k
  synonyms, stamped Animalia/Chordata/Reptilia). Registered in
  `build_backend()` and the `build-light` workflow. License CC-BY 4.0.
* The ReptTraits enrichment is renamed `lizard_traits` -> `repttraits` to match
  its source (it covers all reptiles, not lizards), with the citation corrected
  to Oskyrko et al. (2024). `parse_repttraits()` maps source headers explicitly
  and adds the distribution block (biogeographic realm, microhabitat, habitat
  type, elevation range, mean annual temperature, insular/endemic) that the
  previous morphology-only parser dropped.

# taxifydb 0.1.5

## New features

* Aggregate markers are normalized to one canonical `aggr.` form at build time,
  so taxify recognizes species aggregates uniformly across every backbone and
  enrichment. `precompute_backbone()` folds backbone aggregate names and
  aggregate-rank rows (via `taxify::normalize_aggregate_name()`);
  `resolve_enrichment_names()` keeps aggregate source rows out of cross-backbone
  expansion (which would otherwise leak an aggregate's traits onto the binomial
  species key); `build_enrichment_vtr()` normalizes the enrichment join key.
  Requires taxify (>= 0.3.0).

# taxifydb 0.1.4

## New features

* `parse_fungalroot()` builds a genus-level mycorrhizal type enrichment from the
  FungalRoot database (Soudzilovskaia et al. 2020), published on GBIF as a
  Darwin Core Archive (doi:10.15468/a7ujmj, CC BY-NC 4.0). It reads the
  occurrence core plus its MeasurementOrFact extension, standardizes the
  per-observation `Mycorrhiza type` labels to `AM` / `EcM` / `ErM` / `OM` / `NM`
  (plus dual types, `Other`, `uncertain`), and reduces them to one row per plant
  genus by majority consensus (`mycorrhizal_type`, `mycorrhizal_status`,
  `mycorrhizal_records`). Registered in `.enrichment_build_registry` as a
  genus-keyed enrichment (`name_col = "genus"`).

# taxifydb 0.1.3

## New features

* `parse_ecoflora()` and `parse_floraweb()` build the Ecoflora (British Isles)
  and FloraWeb (German flora) plant-trait enrichments from frozen scrape
  snapshots. Both are registered in `.enrichment_build_registry` and produce
  bundled `.vtr` files (Ecoflora 18 `_uk` columns; FloraWeb 59 `_de` columns
  spanning morphology, reproductive biology, the nine Ellenberg indicator
  values, ploidy and chromosome number, and chorological distribution).
  Ecoflora is redistributed under CC BY-NC-SA 4.0; FloraWeb carries the
  BiolFlor data (Klotz, Kuehn & Durka 2002), usable with acknowledgement and
  citation per the BioFresh metadata statement. The raw per-species snapshots
  are hosted in the `scrape-snapshots-2026.06` release; the access date is the
  dataset version.

## Changes

* `resolve_enrichment_names()` now keeps the best-populated source record when
  several taxa resolve to the same accepted name (subspecies and synonyms
  collapsing onto a species) rather than an arbitrary first row. This recovers
  trait data that could previously be discarded for widely-circumscribed
  species.

* Ecoflora and BiolFlor are removed from `.enrichment_scrape_only`; only
  Pignatti (copyrighted, not redistributable) remains there.


# taxifydb 0.1.2

## Scrape-only sources

* Added a catalog of trait sources that taxifydb does not build into `.vtr`
  files: Pignatti (copyrighted) cannot be redistributed; BiolFlor is usable
  with acknowledgement + citation (BioFresh metadata statement) but has no
  obtainable bulk copy while the UFZ site is offline; Ecoflora's CC BY-NC-SA
  licence would allow it but ecoflora.org.uk has no bulk download.
  `list_scrape_only_enrichments()` lists them. taxify accesses these on demand
  via the TR8 package (`add_ecoflora()`, `add_biolflor()`, `add_pignatti()`);
  nothing is redistributed by taxify.

# taxifydb 0.1.1

## New enrichments

* `baseflor` builder added (`parse_baseflor()`, registry entry). Reads Julve's
  Baseflor spreadsheet (Programme Catminat; ODbL 1.0 / CC BY-SA 2.0) and emits
  flowering months, pollination vector, dispersal mode, breeding system, flower
  colour, fruit type, woody growth form, and the continentality and salinity
  indicator-value axes for ~7,000 vascular plant taxa of France and
  neighbouring regions. Source is the archived 2023-10 snapshot of the file
  (the Orange personal host has since been retired). Built `.vtr` published in
  the `enrichment-2026.06` release; manifest updated.
