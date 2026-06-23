# taxifydb 0.1.2

## Scrape-only sources

* Added a catalog of trait sources that taxifydb does not build into `.vtr`
  files, for two reasons: BiolFlor (permission-gated) and Pignatti
  (copyrighted) cannot be redistributed; Ecoflora's CC BY-NC-SA licence would
  allow it but ecoflora.org.uk has no bulk download (per-species access only).
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
