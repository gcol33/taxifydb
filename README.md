
# taxifydb

*building taxonomic backbones and trait tables from their sources*

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/gcol33/taxifydb/graph/badge.svg)](https://app.codecov.io/gh/gcol33/taxifydb)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

taxifydb builds the files that
[taxify](https://gillescolling.com/taxify/) matches species names
against: taxonomic backbones such as the Catalogue of Life or World
Flora Online, trait and status datasets, and region polygons.

Each builder downloads a source from its provider, converts it to the
column layout that every taxify backbone shares, and writes a `.vtr`
file, the on-disk table format of
[vectra](https://gillescolling.com/vectra/). taxify downloads these
files ready-built, so name matching never needs taxifydb. You install
taxifydb when you want to build a file yourself, from a newer snapshot
or offline, add a backbone or dataset, or republish the hosted files.
taxifydb supports:

- **Taxonomic backbones.** 19 backbones, from global checklists to the
  authorities of single groups, are built through `build_backend()` and
  share one layout: the name, its identifier, its taxonomic status, the
  family and genus, and the link to the accepted name.
- **Trait and status datasets.** 114 trait, ecology, conservation and
  invasion datasets, called enrichments in the function names, are
  parsed to one row per species or genus and resolved to accepted names,
  so they join the output of any backbone.
- **Region polygons.** Botanical regions and marine ecoregions are built
  and versioned like a backbone; taxify reads them to restrict matching
  to a region.
- **Publishing releases.** `publish_release()` and
  `publish_enrichment_release()` upload built files as GitHub release
  assets, and the manifest that taxify reads records the version,
  address and content identifier of each.

## Installation

``` r
# install.packages("pak")
pak::pak("gcol33/taxifydb")
```

## Usage

1.  List what can be built with `list_backends()`, `list_geometry()` and
    `list_enrichments()`.
2.  Build a backbone with `build_backend()` or a dataset with
    `build_enrichment()`.
3.  Rebuild the tables that resolve dataset names across backbones with
    `build_all_name_lookups()`.

``` r
library(taxifydb)

list_backends()
#>  [1] "itis"        "ncbi"        "ott"         "worms"       "wfo"        
#>  [6] "col"         "colxr"       "gbif"        "euromed"     "fungorum"   
#> [11] "algaebase"   "fishbase"    "sealifebase" "reptiledb"   "wcvp"       
#> [16] "lcvp"        "mdd"         "avilist"     "lpsn"
```

Each build downloads the full source from its provider:

``` r
gbif_vtr <- build_backend("gbif")
clopla_vtr <- build_enrichment("clopla")
build_all_name_lookups()
```

Each builder returns the path of the `.vtr` file it wrote. taxify finds
the files in its data directory, and builds through taxifydb when a
ready-built download is not available.

## Backbones

| Backbone         | Scope                                                  |
|------------------|--------------------------------------------------------|
| WFO              | World Flora Online (plants)                            |
| COL              | Catalogue of Life (all kingdoms)                       |
| COL XR           | Catalogue of Life Extended Release (all kingdoms)      |
| GBIF             | GBIF Backbone Taxonomy (all kingdoms)                  |
| ITIS             | Integrated Taxonomic Information System                |
| NCBI             | NCBI Taxonomy                                          |
| OTT              | Open Tree of Life synthesis                            |
| WoRMS            | World Register of Marine Species                       |
| Euro+Med         | Euro+Med PlantBase (European and Mediterranean plants) |
| Index Fungorum   | Fungi                                                  |
| AlgaeBase        | Algae                                                  |
| FishBase         | Fishes                                                 |
| SeaLifeBase      | Aquatic species other than fishes                      |
| Reptile Database | Reptiles                                               |
| WCVP             | World Checklist of Vascular Plants (Kew)               |
| LCVP             | Leipzig Catalogue of Vascular Plants                   |
| MDD              | Mammal Diversity Database                              |
| AviList          | Birds                                                  |
| LPSN             | Prokaryotes (Bacteria and Archaea)                     |

The region polygons are the TDWG Level 3 botanical regions (WGSRPD) and
the Marine Ecoregions of the World (MEOW). They hold polygon vertices
instead of names, so `list_geometry()` lists them apart from the
backbones. [The backbones and
datasets](https://gillescolling.com/taxify/articles/databases.html) in
the taxify documentation describes each backbone and dataset with its
version, licence and reference.

## Hosting the built files

    providers  --download-->  taxifydb   --.vtr-->  GitHub Releases
    (WFO, GBIF,               parse,                 hosted assets +
     GRIIS, GIFT,             normalize,             manifest.json
     ...)                     index                        |
                                                           | download
                                                           v
                                                     taxify (runtime)
                                                     match / enrich, offline

Built files are published as GitHub release assets: one tag per backbone
version, such as `gbif-2026.06`, and one rolling `enrichment-<version>`
tag for all datasets. `manifest/manifest.json` records each asset’s
version, address and content identifier, and taxify reads it to find its
downloads. `update_manifest()` and `update_enrichment_manifest()` keep
the build-side and runtime manifests in step.

## Sources that need extra software

Some builders need an optional package: RSQLite for ITIS, openxlsx2 for
Excel sources, rfishbase for FishBase and SeaLifeBase, GIFT, and others.
A builder that needs one stops with a message naming the package to
install.

A few sources sit behind Cloudflare or a publisher’s bot check and are
fetched with a bundled Python helper that uses `curl_cffi`. taxifydb
looks for a suitable interpreter in pyenv, the Windows `py` launcher and
then the `PATH`; the environment variable `TAXIFYDB_PYTHON` sets one
directly.

## Adding a backbone or dataset

A backbone needs a `build_<name>()` function in `R/backend-<name>.R`,
registered in `build_backend()`. A dataset needs a `parse_<name>()`
function and an entry in `.enrichment_build_registry`. `CLAUDE.md` in
the repository lists every step, the column layout each file must follow
and the step that resolves dataset names across backbones.

## Documentation

Every exported function has a help page; `help(package = "taxifydb")`
lists them.

- [The backbones and
  datasets](https://gillescolling.com/taxify/articles/databases.html):
  each backbone and dataset with its version, licence and reference, in
  the taxify documentation.
- [Choosing and combining
  backbones](https://gillescolling.com/taxify/articles/backbones.html):
  how taxify downloads, chains and pins the files built here.

## Getting help

Questions and bug reports go to the [GitHub
issues](https://github.com/gcol33/taxifydb/issues). A bug report is
easiest to act on with a small reproducible example, for instance made
with [reprex](https://reprex.tidyverse.org/).

## Support

> “Software is like sex: it’s better when it’s free.” — Linus Torvalds

I’m a PhD student who builds R packages in my free time because I
believe good tools should be free and open. I started these projects for
my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to
say thanks. It helps with my coffee addiction.

[![Buy Me A
Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

The code of taxifydb is licensed under MIT. Each source dataset keeps
its own licence. Only openly licensed data is redistributed through
taxify’s releases; sources that require citation only or carry no
licence are built locally and never redistributed. When you use a
dataset’s data, cite the original dataset.

## Citation

``` text
Colling G (2026). _taxifydb: Build Backbones and Enrichments for the
'taxify' Package_. R package version 0.1.23.9000,
<https://github.com/gcol33/taxifydb>.
```

BibTeX:

``` bibtex
@Manual{colling2026taxifydb,
  title = {taxifydb: Build Backbones and Enrichments for the 'taxify' Package},
  author = {Gilles Colling},
  year = {2026},
  note = {R package version 0.1.23.9000},
  url = {https://github.com/gcol33/taxifydb},
}
```
