# taxifydb 0.1.2

## Scrape-only sources

* Added a catalog of trait sources that cannot be redistributed under an open
  license and are therefore not built into `.vtr` files: Ecoflora
  (CC BY-NC-SA, no bulk download), BiolFlor (permission-gated), and Pignatti
  (copyrighted). `list_scrape_only_enrichments()` lists them. taxify fetches
  these on demand via the TR8 package (`add_ecoflora()`, `add_biolflor()`,
  `add_pignatti()`); nothing is redistributed.

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
