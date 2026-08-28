# taxifydb 0.1.22

## Enrichment assets reach the accepted names only one backbone keeps (#44)

* The cross-backbone expansion resolved each source name forward through every
  backbone and kept the union, which is not the set the runtime joins on. A
  backbone that keeps a name no other backbone accepts is reachable only by
  entering the concept through a name the source never lists, and a forward
  image never visits it. WCVP's asset carried no `Minuartia hybrida` key
  because its own names are all `Sabulina ...`, so a WFO user's `add_wcvp()`
  joined on a key that did not exist and got back what a name the source
  genuinely does not cover gets back.

* The closure adds one reverse hop and keeps two things from it: a name some
  backbone holds ACCEPTED while others synonymise it onto the forward image,
  and a re-routing two or more backbones agree on. It does not take the
  reverse-discovered names' own forward image, which chains concepts through a
  single bad synonym record -- GBIF alone maps `sabulina hybrida` to
  *Saponaria officinalis* where six backbones keep the name inside the concept,
  and following it pulled soapwort into a set of 48 accepted names. Those three
  refused pairs are what corroboration buys.

* All 108 redistributable enrichments were rebuilt and uploaded to the rolling
  `enrichment-2026.08` release. Measured on the rebuilt zanne asset, 200
  sampled names against every installed backbone: unmatched backbone-name pairs
  fall 41 -> 17 of 1,474, and names missing from at least one backbone 10.5% ->
  7.5%. Assets grow 1.96x (270 MB -> 529 MB over the 100 with a shipped
  counterpart); wcvp, the worst affected at a 22.5% gap, goes 69 -> 141 MB.

* Every entry now records `resolved_backbones`, naming the backbones the
  expansion reached. Its absence is what made a partially expanded asset
  indistinguishable from a complete one: a missing `name_lookup.vtr` only
  warned. The publishing paths resolve strictly now and a missing lookup stops
  the build, while `build_enrichment()` stays lenient so taxify's
  build-from-source fallback still works against whatever a user has installed.
  `publish-enrichment.yml` moves to the self-hosted runner, since a hosted one
  has no taxify data dir and resolved against nothing at all.

* Three `trait_cols` lists moved with the republish, none of them from this
  change. griis loses `recordid`, which the GBIF-backed source stopped
  returning between builds and which was a per-record id being median-
  aggregated across records. avonet loses `avibase_id1` and glonaf
  `presence_uncertain` + `quest_native`: both `.vtr` schemas are unchanged and
  those entries had been advertising columns the built file never carried.

* Two defects found while fixing the above. Name lookups gain an
  `accepted_name` index, added in place on existing files, without which the
  reverse direction is a full scan. And the schema probe collected each whole
  lookup to read one column name (`head()` after `collect()`, not before),
  which had the audit process at 20 GB resident.

## GBIF infraspecific names carry their rank marker (#45)

* GBIF renders an infraspecific canonical without its rank connecting term
  (`Erica tenella tenella` for `Erica tenella var. tenella`), a rendering no
  other backbone produces, so anything keyed on the name misses and the dropped
  marker rides `accepted_name` into every `add_data()` and `reconcile()` join.

* `gbif_render_infraspecific()` reads the marker back from the full
  `scientific_name`, anchored on the epithet so a `f.` forma marker stays apart
  from an `L. f.` filius author, and inserts it into the canonical. Reading the
  marker rather than mapping `rank` leaves zoological trinomials
  (*Panthera leo persica*, marker-less by code) unchanged. It runs in
  `normalize_gbif` before `precompute_keys`, so the keys, the embedded
  `accepted_name` and the genus register all pick up the corrected canonical.

* `gbif-2026.08` is re-cut from a build carrying it: same source snapshot and
  row count (6,404,001), with sha, size and content id moving, which is what
  signals a runtime to refetch.

## GloNAF was joined on the wrong key

* Parsers name the column they want as a list of spellings, most specific
  first, so a source that renames a field between releases still parses. The
  lookup was `intersect(names(df), candidates)`, and `intersect()` returns its
  matches in the order of its FIRST argument: the answer was whichever
  candidate sat leftmost in the file, not the preferred one. `.first_col()`
  reads the list as the preference order it was written as, and all 34 lookups
  across the two parser files go through it. Three genuine set intersections
  (LepTraits' month columns, EltonTraits' diet columns, FloraWeb's labels) are
  unchanged.

* GloNAF is where that bit. Its flora table carries its own row `id` before
  `taxon_wcvp_id`, the foreign key into the taxon table, and both are integers
  over overlapping ranges, so joining on the row id matched part of the range
  instead of failing: `glonaf.vtr` held 10,238 rows over 84 regions against the
  256,454 over 1,343 the source holds, and the rows it did hold paired a name
  with another taxon's family (`Campanula latiloba` filed under Bignoniaceae).

* Reading the candidate lists in order then exposed a second wrong key: the
  region list put `OBJIDsic`, the region polygon's GIS object id, ahead of
  `id`. Those ranges overlap too, so it resolved 825 of 1,343 regions.
  `OBJIDsic` is now last, reached only when the region table names its primary
  key nothing else.

* `parse_glonaf()` fails the build when either join key does not resolve for
  every flora row. Both bugs returned a plausible table rather than an error,
  which is what a partly-matching integer key looks like from the inside.

* Every candidate list converted was checked against the live source header for
  the other thirteen parsers that use one (EltonTraits, AVONET, PanTHERIA,
  AmphiBIO, FishMorph, LEDA, Díaz, GRIIS, FUNGuild, LepTraits, AnimalTraits, NW
  European arthropod traits, AnAge). All pick the same column as before. The
  built `meta.json` cannot answer this -- `.is_bookkeeping_col()` strips exactly
  the id and name columns at issue -- so the check reads the sources.

## GloNAF rebuilt from the 2025 release

* The registry was pinned to Zenodo record `13235357` (2024-08-05). The current
  release is record `17105725` (2025-09-12), GloNAF v2.02: 16,429 taxa over
  1,343 regions from 336 data sources, matched against WCVP v12. Its files moved
  from `.xlsx` to `.csv`, so building the enrichment no longer needs openxlsx2.

* Rebuilt and republished to `enrichment-2026.08`: 300,094 rows over all 1,343
  regions, against 10,238 rows over 84 regions, resolved against all 19
  backbones. Both manifests record the new record, row count and content id.

## A new deposit of the same work keeps its curated citation

* A runtime `citation` is rewritten from the build when the entry moves to a
  different `source_url` or `source_doi`, so it cannot go on naming a source
  nobody read. A versioned deposit moves its URL on every release while the
  work being cited stays put, and GloNAF's runtime entry -- a structured block
  carrying authors, journal and DOI -- was flattened to the registry's one-line
  attribution on the record bump. A stored citation that already names the
  entry's `source_doi` is now kept.

## A resumed publish checks what it is about to upload

* `TAXIFYDB_PUBLISH_RESUME=1` reuses whatever `.vtr` sits in an enrichment's
  output directory rather than rebuilding it, which is what makes an
  interrupted publish cheap to finish. A `.vtr` left from an earlier source
  release is indistinguishable from a current one on disk, so the reuse
  uploaded stale bytes and then wrote a manifest entry naming the record that
  build never read. `assert_built_matches_registry()` compares the sidecar's
  `source_url` and `version` against the registry and refuses the reuse when
  they disagree, or when there is no sidecar to compare.

## Alien first records rebuilt from the v4.0 release

* Seebens et al. replaced the single freedata xlsx with a relational Darwin
  Core-style release: a semicolon-delimited dataset table beside location,
  taxonomy and source tables, every column renamed. The old parser reads an
  xlsx sheet by its v3.1 headers, so it failed on the new file rather than
  mis-reading it, and nothing already published was wrong.

* The location mapping is what needed care. Locations are keyed on wording and
  v4.0 renames 24 of them, so a straight port drops 13,104 records without a
  word ("United States" to "United States of America" alone is 8,050). The
  lookup folds accents, case and punctuation before matching, which absorbs the
  cosmetic respellings (Reunion, Curacao, Timor-Leste); a build asserts no two
  wordings fold together onto different countries, and a non-empty location the
  map does not hold is a hard error naming it.

* The file is UTF-8 except for 679 latin1 lines, which no single `fileEncoding`
  reads -- UTF-8 errors on those, latin1 mojibakes the 2,739 real sequences
  elsewhere -- so `.read_delim_utf8()` decides per line.

* Where a species has several records for one country the earliest year still
  wins, but a record asserting presence now outranks one recording absence, so
  the retained row does not date an occurrence from a record denying it. That
  record's own status rides beside its year.

* Verified against an independent reimplementation of the parse: 81,747 pairs,
  identical years, no difference either way. Against v3.1: 72,631 -> 81,731
  pairs, 242 -> 245 countries, 23,677 -> 27,251 names, with 6,016 pairs
  dropping out upstream, mostly ants and consistent with taxonomic cleanup.
  Published as 93,350 rows over 245 countries, byte-identical to the local
  build.

* Two shared helpers widened for it. Darwin Core writes identifiers in camel
  case, so `taxonID` sanitizes to `taxonid` with no separator for
  `.is_bookkeeping_col()`'s `.*_id` rule and the ids were being published as
  traits with min/max/n spreads; the DwC spellings are named explicitly rather
  than caught by a general `.*id`, since trait values end the same way (hybrid,
  diploid). `fold_accents()` goes from French lowercase to the whole Latin-1
  letter block.

## ThermoFresh reads the peer-reviewed release

* `thermofresh` was pinned to Zenodo record `14056760`, the version deposited
  before peer review. The published release is record `16959762` (ThermoFresh
  v1.0), described in Bayat et al. (2025), Scientific Data 12,
  doi:10.1038/s41597-025-05832-w. The enrichment carried 5,115 tolerance tests
  over 828 taxonomy rows against the 6,825 and 931 the published data holds;
  `parse_thermofresh()` now yields 768 species, up from 673.

* The v1.0 archive keeps the pre-review submission verbatim under
  `data/initial_submission/` and puts the peer-reviewed tables beside it as
  `data/*_final.csv`, so bumping the URL alone would have kept reading the old
  data under a new version number. The parser matches the `_final` names, and
  its file lookup stops when a pattern matches anything other than exactly one
  file rather than taking the first of several.

* Rebuilt and republished to `enrichment-2026.08`: 860 rows against the 750 the
  release carried, resolved against all 19 backbones. Both manifests record the
  new record, the new row count and the new content id, so a runtime holding the
  old file refetches on the content id rather than waiting for a version bump.

* The runtime entry went on citing record `14056760` after the build had moved
  to the published one. `update_enrichment_manifest(runtime = TRUE)` fills a
  runtime field only when absent, so hand-curated text survives a release --
  except `citation` when the build's `source_url` or `source_doi` has changed,
  which is the one case where keeping it means citing data nobody read.

## The enrichment version check can see a new upstream version

* `check_zenodo_version()` queried the pinned record. A Zenodo record is
  immutable, so it reported the day that record was published no matter how
  many newer versions its concept had gained, and no Zenodo-sourced enrichment
  could ever be reported as outdated -- which is how the ThermoFresh release
  above went unnoticed for a year. It resolves `versions/latest` and compares
  record IDs. It also reads the legacy `/record/` path, which `animaltraits`
  uses and the pattern did not match.

* `check_enrichment_source_version()` picked its checker with
  `switch(entry$source_format, ...)`. That field is absent from 97 of the 108
  manifest entries, so the call errored and every one of them was reported as
  "could not determine upstream version" -- an error the weekly workflow
  records and does not open an issue for. The checker is chosen from the host
  the source URL names, which every entry carries.

* `check_figshare_version()` read `versions[[1]]` as the newest. Figshare lists
  versions ascending, so it reported version 1 for every article however many
  versions had been published. It takes the highest. It also accepts an article
  URL, not only a file URL.

* Freshness a checker cannot settle is now reported as unknown with a note.
  The recorded version is this package's own release string and the upstream
  version is whatever the host counts in, so comparing the two answered a
  different question than the one asked; on Figshare-sourced entries it read as
  outdated every week.

## A build records what the source host called the version it read

* `build_enrichment()` asks the source host for its identity for the version
  being downloaded -- a Zenodo record number, a Figshare or Dryad version
  counter, a `Last-Modified` stamp -- and `build_enrichment_vtr()` writes it to
  `meta.json` as `upstream_id`, which `update_enrichment_manifest()` carries
  into the manifest entry. The weekly check compares that against what the host
  offers now, so both sides of the comparison are the host's own counting.
  Zenodo entries were already answerable from the record number in the URL;
  this is what makes the Figshare, Dryad and GBIF entries answerable too.

* An entry built before `upstream_id` existed records none, and reports unknown
  rather than a guess until its next rebuild. Filling the field in from what
  upstream offers today would assert every enrichment is current, which is the
  claim the check exists to test.

* The probes report identity and nothing else; `check_enrichment_source_version()`
  decides freshness in one place from what they report. Which probe answers for
  a source is a table keyed on the host, so covering a new one is a row.

* The first sweep over all 108 entries reports six behind upstream, not one:
  `thermofresh`, `glonaf`, `alien_first_records`, `gwdd`, `pottier` and
  `zooplankton`, each pinned to a Zenodo record whose concept has since gained
  a newer one. Twenty-five report current. The remaining 77 report unknown --
  12 awaiting a rebuild to record an identity, 15 naming a URL the host cannot
  answer for, and 50 on hosts no probe covers.

* A host with no probe is now reported apart from a host whose probe came back
  empty. Reading both as "no version check for source host" is how a source URL
  that no longer resolves passes for a source nobody checks -- `amphibio` and
  `elton_traits` name `ndownloader.figshare.com` download ids the Figshare API
  answers 404 for, and they read as unchecked rather than unreadable.

## A manifest written on Windows keeps its line endings

* `jsonlite::write_json()` writes through a text connection, so publishing from
  Windows ended every line of `manifest.json` CRLF where CI ends it LF: the one
  entry a release changed arrived inside a rewrite of all 5,800 lines. The
  backbones too large for the hosted runner are published by hand from Windows,
  so that was every COL, COL XR and GBIF release. `write_json_lf()` serializes
  to a string and writes it through a binary connection; both manifest writers,
  the `meta.json` sidecar and `sync_manifest.R` go through it.

## A runtime trait_cols the build no longer produces is dropped

* Runtime-only manifest fields are written only when absent, so hand-curated
  values survive a republish. `trait_cols` is not merely accompanying text
  though: it names the columns the `.vtr` carries, and preserving it through a
  source that renames its fields leaves the runtime advertising columns that
  are not there. alien_first_records is where that showed -- the republished
  entry went on naming thirteen columns the built file no longer had while
  hiding the nine it did.

* A stored list is kept whole while every column it names is still built, so a
  curated subset or ordering is untouched; once one is gone the list describes
  a file that does not exist and is taken from the build. Whether the group
  column belongs in the list is a per-entry choice -- five of the seven
  group-keyed entries carry it, two do not -- so that choice is preserved
  across the rewrite rather than settled here. This is the same shape as the
  citation rule beside it, and for the same reason: both describe the source
  rather than merely accompany it, so both go stale when it moves.

# taxifydb 0.1.21

## The coverage audit reads the backbone set instead of carrying a copy

* `check_manifest_coverage.R` walked a `BACKENDS` vector kept by hand, so a
  backbone wired into the build but not added there was audited by nothing and
  reported as clean. COL XR had been in that position since it was wired. The
  vector is gone: the set is parsed out of `.register_extractors` in
  `R/register.R`, and the script stops with an explanation rather than auditing a
  guessed set when it cannot read that file.

* The audit also checks what the published register was built from. Both
  register artifacts already record the backbones they unioned in their `.meta`
  sidecar and nothing compared it, so a register going stale as backbones were
  added had no signal at all -- no version moves and no bytes change in anything
  that is checked. `register_incomplete` and `register_unreadable` join the drift
  states the weekly workflow opens an issue for.

* Reading a release asset over the API works with a token present.
  `curl::handle_setheaders()` replaces the header set rather than adding to it,
  so setting `Authorization` in a second call dropped the
  `Accept: application/octet-stream` that asks for the bytes, and the API
  answered with the asset's JSON description instead. The same defect had been
  dropping `User-Agent` from every authenticated request. One builder now
  assembles the whole header set in a single call.

## Reference geometry builds through the same door as a backbone

* `build_backend("wgsrpd")` and `build_backend("meow")` work, and both build in
  `build-light.yml` alongside the backbones. The boundary geometry behind
  `region =` and the coordinate lookup had no entry in any builder registry, so
  it could only be rebuilt by calling the internal function directly.

* They are registered in `.geometry_builders`, not `.backend_builders`, and
  `list_geometry()` names them. `list_backends()` is what
  `build_all_name_lookups()` and `resolve_name_map()` iterate to build the
  per-backbone accepted-name lookups that every enrichment is resolved through;
  a vertex table carries no taxonomic names, so a geometry asset listed there
  would yield an empty lookup and quietly cost the enrichments resolved against
  it their matches.

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
