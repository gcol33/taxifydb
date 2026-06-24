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
