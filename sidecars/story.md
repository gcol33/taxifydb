# Story: taxifydb

## Claim
taxifydb builds the backbone, dataset and region files that taxify reads, from each provider's own download, in one shared column layout.

## Audience
Maintainers of taxify's hosted data, and taxify users who want a file built from a newer snapshot, offline, or for a source taxify does not host. They know taxify and what a backbone is; vectra's `.vtr` format is introduced where it appears.

## Relation
- to: taxify (the runtime that reads the files)
- phrase: builds the files that taxify matches species names against
- same: the column layout every taxify backbone shares; the manifest taxify reads
- differs: taxifydb holds every download, parse, normalisation and indexing step; taxify holds matching and joins and needs taxifydb only to build from source
- status: companion, not a replacement; distributed from GitHub
- home: README ("Hosting the built files"); taxify's backbones article

## Arcs

### A1 Build a backbone from source
- question: Can I rebuild a backbone myself, from a newer snapshot or offline?
- answer: `build_backend(name)` downloads, normalises and writes the `.vtr` file and returns its path.
- payoff: taxify picks the file up from its data directory.
- evidence: R/build_backend.R; tests/testthat
- limits: builds download the full source and can take hours
- home: ?build_backend
- also: README

### A2 Build trait and status datasets
- question: How do datasets become joinable on accepted names?
- answer: `build_enrichment(name)` parses a dataset to one row per species or genus and resolves the names across backbones.
- payoff: the dataset joins any backbone's output in taxify.
- evidence: R/build_enrichment.R; R/enrichment-registry.R
- limits: citation-only and unlicensed sources are built locally and never redistributed
- home: ?build_enrichment
- also: README

### A3 Publish and pin releases
- question: How do built files reach taxify users?
- answer: release assets per backbone version plus a rolling enrichment tag, recorded in a manifest.
- payoff: taxify downloads and pins versions without building.
- evidence: R/publish.R; manifest/manifest.json
- limits: GitHub Releases is the only host
- home: README ("Hosting the built files")
- also: ?publish_release

### A4 Add a source
- question: How do I add a backbone or dataset?
- answer: a builder or parser plus a registry entry, following the checklist in CLAUDE.md.
- payoff: the new source is built and published like the others.
- evidence: CLAUDE.md
- limits: no article yet
- home: CLAUDE.md
- also: README

## Non-goals
- Name matching and joins (taxify does them).
- A CRAN release.

## Documents
- DESCRIPTION: claim, relation to taxify, the three kinds of file built
- README: claim, relation, install, first listing call, one section per arc, routing
- help pages: reference for each builder, checker and publisher

## Open
- No pkgdown site and no articles; the README routes to help pages and taxify's articles.
- No R-CMD-check workflow, so no R-CMD-check badge.
