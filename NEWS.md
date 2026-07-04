# taxifydb (development version)

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
