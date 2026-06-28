# Enrichment build registry.
#
# Each list entry describes one enrichment: source URL, attribution metadata,
# how to download the raw source (download_fn), and how to parse it
# (parse_fn). The dispatcher build_enrichment() reads this registry.
#
# Ported from taxify::R/enrichment-build.R. Two extra entries (fishbase,
# funguild) are present here so that taxifydb is the single source of truth
# for the build pipeline.


#' Internal registry of enrichment builders
#' @noRd
.enrichment_build_registry <- list(

  woodiness = list(
    source_url  = "https://raw.githubusercontent.com/ejedwards/reanalysis_zanne2014/master/dryad/GlobalWoodinessDatabase.csv",
    source_doi  = "10.5061/dryad.63q27",
    version     = "2014.1",
    license     = "CC0",
    attribution = "Zanne AE et al. (2014) Three keys to the radiation of angiosperms into freezing environments. Nature 506:89-92. (Dryad CSV mirrored unaltered in github.com/ejedwards/reanalysis_zanne2014.)",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "GlobalWoodinessDatabase.csv")
    },
    parse_fn    = function(path) parse_woodiness(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  eive = list(
    source_url  = "https://zenodo.org/records/7534792/files/EIVE_Paper_1.0_SM_08.xlsx?download=1",
    source_doi  = "10.3897/VCS.98324",
    version     = "1.0",
    license     = "CC BY 4.0",
    attribution = "Dengler J et al. (2023) EIVE 1.0 -- a standardized set of Ecological Indicator Values for Europe. Vegetation Classification and Survey 4:7-29.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "EIVE_1.0.xlsx")
    },
    parse_fn    = function(path) parse_eive(path),
    group_col   = NULL,
    requires    = "openxlsx2"
  ),

  elton_traits = list(
    source_url  = "https://ndownloader.figshare.com/files/5631081",
    source_doi  = "10.6084/m9.figshare.c.3306933.v1",
    version     = "1.0",
    license     = "CC0",
    attribution = "Wilman H et al. (2014) EltonTraits 1.0: Species-level foraging attributes of the world's birds and mammals. Ecology 95:2027.",
    download_fn = function(url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      download_curl_file(
        "https://ndownloader.figshare.com/files/5631081",
        dest, "BirdFuncDat.txt"
      )
      download_curl_file(
        "https://ndownloader.figshare.com/files/5631084",
        dest, "MamFuncDat.txt"
      )
      dest
    },
    parse_fn    = function(path) {
      parse_elton_traits(
        file.path(path, "BirdFuncDat.txt"),
        file.path(path, "MamFuncDat.txt")
      )
    },
    group_col   = NULL,
    requires    = character(0)
  ),

  avonet = list(
    source_url  = "https://ndownloader.figshare.com/files/34480856",
    source_doi  = "10.6084/m9.figshare.16586228.v5",
    version     = "1.0",
    license     = "CC BY 4.0",
    attribution = "Tobias JA et al. (2022) AVONET: morphological, ecological and geographical data for all birds. Ecology Letters 25:581-597.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "AVONET_BirdLife.xlsx")
    },
    parse_fn    = function(path) parse_avonet(path),
    group_col   = NULL,
    requires    = "openxlsx2"
  ),

  pantheria = list(
    source_url  = "https://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt",
    source_doi  = "10.1890/08-1494.1",
    version     = "1.0",
    license     = "CC0",
    attribution = "Jones KE et al. (2009) PanTHERIA: a species-level database of life history, ecology, and geography of extant and recently extinct mammals. Ecology 90:2648.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "PanTHERIA.txt")
    },
    parse_fn    = function(path) parse_pantheria(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  amphibio = list(
    source_url  = "https://ndownloader.figshare.com/files/8828578",
    source_doi  = "10.6084/m9.figshare.4644424.v5",
    version     = "1.0",
    license     = "CC BY 4.0",
    attribution = "Oliveira BF et al. (2017) AmphiBIO, a global database for amphibian ecological traits. Scientific Data 4:170123.",
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, "\\.csv$")
    },
    parse_fn    = function(path) parse_amphibio(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  leda = list(
    source_url  = "https://uol.de/f/5/inst/biologie/ag/landeco/download/LEDA/Data_files/",
    source_doi  = "10.1111/j.1365-2745.2008.01430.x",
    version     = "2008.1",
    license     = "Free for academic use",
    attribution = "Kleyer M et al. (2008) The LEDA Traitbase: a database of life-history traits of the Northwest European flora. J Ecol 96:1266-1274.",
    download_fn = function(url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      leda_base <- "https://uol.de/f/5/inst/biologie/ag/landeco/download/LEDA/Data_files/"
      trait_files <- c(
        "life_form.txt"         = "plant_growth_form.txt",
        "dispersal_type.txt"    = "dispersal_type.txt",
        "TV.txt"                = "TV_2016.txt",
        "seed_mass.txt"         = "seed_mass.txt",
        "canopy_height.txt"     = "canopy_height.txt",
        "leaf_mass.txt"         = "leaf_mass.txt",
        "SLA.txt"               = "SLA_und_geo_neu2.txt",
        "clonal_growth.txt"     = "CGO.txt",
        "buoyancy.txt"          = "buoyancy_2016.txt"
      )
      for (out_name in names(trait_files)) {
        upstream <- trait_files[[out_name]]
        tryCatch(
          download_curl_file(paste0(leda_base, upstream), dest, out_name),
          error = function(e) {
            message(sprintf("  Warning: failed to download LEDA %s (%s): %s",
                            out_name, upstream, conditionMessage(e)))
          }
        )
      }
      dest
    },
    parse_fn    = function(path) parse_leda(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  diaz_traits = list(
    source_url  = "https://raw.githubusercontent.com/kydahl/biodiv-hotspots/main/data/raw/Trait_data_TRY_Diaz_2022/Dataset/Species_mean_traits.xlsx",
    source_doi  = "10.1038/s41597-022-01774-9",
    version     = "2022.1",
    license     = "CC BY 4.0",
    attribution = "Diaz S et al. (2022) The global spectrum of plant form and function: enhanced species-level trait dataset. Scientific Data 9:755.",
    download_fn = function(url, dest) {
      download_curl_file(
        url, dest, "Species_mean_traits.xlsx",
        user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Firefox/120.0"
      )
    },
    parse_fn    = function(path) parse_diaz_traits(path),
    group_col   = NULL,
    requires    = "openxlsx2"
  ),

  griis = list(
    source_url  = "https://zenodo.org/records/6348164/files/GRIIS%20-%20Country%20Compendium%20V1_0.csv?download=1",
    source_doi  = "10.15468/6jcu0q",
    version     = "1.0",
    license     = "CC BY 4.0",
    attribution = "Pagad S et al. GRIIS - Global Register of Introduced and Invasive Species.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "GRIIS_Country_Compendium_V1_0.csv")
    },
    parse_fn    = function(path) parse_griis(path),
    group_col   = "country_code",
    requires    = character(0)
  ),

  alien_first_records = list(
    source_url  = "https://zenodo.org/records/10039630/files/GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx",
    source_doi  = "10.5281/zenodo.10039630",
    version     = "3.1",
    license     = "CC BY 4.0",
    attribution = "Seebens H et al. (2017) No saturation in the accumulation of alien species worldwide. Nature Communications 8, 14435. Zenodo release v3.1.",
    download_fn = function(url, dest) {
      download_curl_file(
        url, dest,
        "GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx",
        user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Firefox/120.0"
      )
    },
    parse_fn    = function(path) parse_alien_first_records(path),
    group_col   = "country_code",
    requires    = "openxlsx2"
  ),

  conservation_status = list(
    source_url  = "https://hosted-datasets.gbif.org/datasets/iucn/iucn-latest.zip",
    source_doi  = NULL,
    version     = format(Sys.Date(), "%Y.%m"),
    license     = "CC BY 4.0",
    attribution = "IUCN Red List of Threatened Species, published on GBIF as a Darwin Core Archive (dataset 19491596-35ae-4a91-9a98-85cf505f1bd3).",
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, pattern = NULL)
    },
    parse_fn    = function(path) parse_conservation_status(path),
    group_col   = NULL,
    requires    = "data.table"
  ),

  wcvp = list(
    source_url  = "https://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip",
    source_doi  = "10.1038/s41597-021-00997-6",
    version     = "2024.1",
    license     = "CC BY",
    attribution = "WCVP (2024) World Checklist of Vascular Plants. Royal Botanic Gardens, Kew.",
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, pattern = NULL)
    },
    parse_fn    = function(path) parse_wcvp(path),
    group_col   = "tdwg_code",
    requires    = character(0)
  ),

  common_names = list(
    source_url  = paste(
      "https://hosted-datasets.gbif.org/datasets/backbone/current/backbone.zip",
      "https://ftp.ncbi.nlm.nih.gov/pub/taxonomy/new_taxdump/new_taxdump.tar.gz",
      "https://files.opentreeoflife.org/ott/ott3.7.3/ott3.7.3.tgz",
      sep = " ; "
    ),
    source_doi  = NULL,
    version     = format(Sys.Date(), "%Y.%m"),
    license     = "CC0 (GBIF, OTT) / public domain (NCBI)",
    attribution = paste(
      "GBIF Secretariat. GBIF Backbone Taxonomy vernacular names.",
      "NCBI Taxonomy (common names from names.dmp).",
      "Open Tree of Life Taxonomy (common names from synonyms.tsv).",
      sep = " "
    ),
    download_fn = function(url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      urls <- strsplit(url, " ; ", fixed = TRUE)[[1L]]
      gbif_url <- urls[1L]
      ncbi_url <- urls[2L]
      ott_url  <- urls[3L]

      gbif_dir <- file.path(dest, "gbif")
      if (!dir.exists(gbif_dir)) {
        dir.create(gbif_dir, recursive = TRUE)
        zip_path <- file.path(dest, "backbone.zip")
        if (!file.exists(zip_path)) {
          h <- curl::new_handle()
          curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L)
          curl::handle_setheaders(h, "User-Agent" = "R/4.5 taxifydb")
          curl::curl_download(gbif_url, zip_path, handle = h)
        }
        zip_contents <- utils::unzip(zip_path, list = TRUE)
        vn_file <- zip_contents$Name[grepl("VernacularName",
                                           zip_contents$Name)]
        taxon_file <- zip_contents$Name[grepl("^Taxon\\.tsv$",
                                              zip_contents$Name)]
        utils::unzip(zip_path, files = c(vn_file, taxon_file),
                     exdir = gbif_dir, junkpaths = TRUE)
      }

      ncbi_dir <- file.path(dest, "ncbi")
      if (!dir.exists(ncbi_dir)) {
        dir.create(ncbi_dir, recursive = TRUE)
        tar_path <- file.path(dest, "taxdump.tar.gz")
        if (!file.exists(tar_path)) {
          utils::download.file(ncbi_url, tar_path, mode = "wb", quiet = TRUE)
        }
        utils::untar(tar_path, files = "names.dmp", exdir = ncbi_dir)
      }

      ott_dir <- file.path(dest, "ott")
      if (!dir.exists(ott_dir)) {
        tryCatch({
          dir.create(ott_dir, recursive = TRUE)
          tgz_path <- file.path(dest, "ott.tgz")
          if (!file.exists(tgz_path)) {
            utils::download.file(ott_url, tgz_path, mode = "wb", quiet = TRUE)
          }
          utils::untar(tgz_path, exdir = dest)
          ott_extracted <- list.dirs(dest, recursive = FALSE,
                                     full.names = TRUE)
          ott_extracted <- ott_extracted[grepl("^ott",
                                               basename(ott_extracted))][1L]
          if (!is.na(ott_extracted)) {
            file.copy(file.path(ott_extracted, "taxonomy.tsv"), ott_dir)
            file.copy(file.path(ott_extracted, "synonyms.tsv"), ott_dir)
            unlink(ott_extracted, recursive = TRUE)
          }
        }, error = function(e) {
          message(sprintf(
            "  Warning: OTT common names skipped (%s). ",
            conditionMessage(e)
          ))
          unlink(ott_dir, recursive = TRUE)
        })
      }

      dest
    },
    parse_fn    = function(path) parse_common_names(path),
    group_col   = "lang",
    requires    = character(0)
  ),

  funguild = list(
    source_url  = "http://www.stbates.org/funguild_db_2.php",
    source_doi  = "10.1016/j.funeco.2015.06.006",
    version     = "2024.1",
    license     = "CC BY 4.0",
    attribution = "Nguyen NH et al. (2016) FUNGuild: An open annotation tool for parsing fungal community datasets by ecological guild. Fungal Ecology 20:241-248.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "funguild_db.html")
    },
    parse_fn    = function(path) parse_funguild(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  fishbase = list(
    source_url  = "https://fishbase.ropensci.org",
    source_doi  = NULL,
    version     = format(Sys.Date(), "%Y.%m"),
    license     = "CC BY-NC 3.0",
    attribution = "Froese R, Pauly D (eds.) (2024) FishBase. World Wide Web electronic publication, https://www.fishbase.org.",
    download_fn = function(url, dest) {
      # rfishbase fetches data directly; dest exists only so the interface
      # is uniform with file-based parsers.
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      dest
    },
    parse_fn    = function(path) parse_fishbase(path),
    group_col   = NULL,
    requires    = "rfishbase"
  ),

  fungal_traits = list(
    source_url  = "https://static-content.springer.com/esm/art%3A10.1007%2Fs13225-020-00466-2/MediaObjects/13225_2020_466_MOESM4_ESM.xlsx",
    source_doi  = "10.1007/s13225-020-00466-2",
    version     = "2020.1",
    license     = "CC BY 4.0",
    attribution = "Polme S et al. (2020) FungalTraits: a user-friendly traits database of fungi and fungus-like stramenopiles. Fungal Diversity 105:1-16.",
    download_fn = function(url, dest) {
      download_curl_file(
        url, dest, "FungalTraits.xlsx",
        referer = "https://link.springer.com/article/10.1007/s13225-020-00466-2"
      )
    },
    parse_fn    = function(path) parse_fungal_traits(path),
    group_col   = NULL,
    name_col    = "genus",
    requires    = "openxlsx2"
  ),

  fungalroot = list(
    source_url  = paste0("https://orphans.gbif.org/EE/",
                         "744edc21-8dd2-474e-8a0b-b8c3d56a3c2d.232.zip"),
    source_doi  = "10.15468/a7ujmj",
    version     = "2020.1",
    license     = "CC BY-NC 4.0",
    attribution = paste0(
      "Soudzilovskaia NA et al. (2020) FungalRoot: global online database of ",
      "plant mycorrhizal associations. New Phytologist 227:955-966. Taxon ",
      "occurrence data published on GBIF as a Darwin Core Archive ",
      "(doi:10.15468/a7ujmj). Genus-level mycorrhizal type is a majority ",
      "consensus computed by taxifydb from the per-observation labels."
    ),
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, pattern = NULL)
    },
    parse_fn    = function(path) parse_fungalroot(path),
    group_col   = NULL,
    name_col    = "genus",
    requires    = character(0)
  ),

  algae_traits = list(
    source_url  = "https://mda.vliz.be/download.php?file=VLIZ_00000308_62bf06138859e409561556",
    source_doi  = "10.14284/574",
    version     = "2022.06",
    license     = "CC BY 4.0",
    attribution = "Vranken S et al. (2023) AlgaeTraits: a trait database for (European) seaweeds. Earth System Science Data 15:2711-2754.",
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, pattern = NULL)
    },
    parse_fn    = function(path) parse_algae_traits(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  fish_traits = list(
    source_url  = "https://ndownloader.figshare.com/files/28672242",
    source_doi  = "10.6084/m9.figshare.14891412",
    version     = "1.0",
    license     = "CC BY 4.0",
    attribution = "Brosse S et al. (2021) FISHMORPH: A global database on morphological traits of freshwater fishes. Global Ecology and Biogeography 30:2330-2336.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "FISHMORPH_Database.csv")
    },
    parse_fn    = function(path) parse_fish_traits(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  repttraits = list(
    source_url  = "https://ndownloader.figshare.com/files/45408133",
    source_doi  = "10.6084/m9.figshare.24572683",
    version     = "1.2",
    license     = "CC BY 4.0",
    attribution = "Oskyrko O, Mi C, Meiri S, Du W (2024) ReptTraits: a comprehensive dataset of ecological traits in reptiles. Scientific Data 11:243.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "ReptTraits_v1-2.xlsx")
    },
    parse_fn    = function(path) parse_repttraits(path),
    group_col   = NULL,
    requires    = "openxlsx2"
  ),

  anage = list(
    source_url  = "https://genomics.senescence.info/species/dataset.zip",
    source_doi  = "10.1111/j.1420-9101.2009.01783.x",
    version     = "15.0",
    license     = "CC BY",
    attribution = "Tacutu R et al. (2018) Human Ageing Genomic Resources: new and updated databases. Nucleic Acids Research 46:D1083-D1090.",
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, "(?i)anage.*\\.txt$")
    },
    parse_fn    = function(path) parse_anage(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  glonaf = list(
    source_url  = "https://zenodo.org/api/records/13235357",
    source_doi  = "10.1002/ecy.2542",
    version     = "2024.1",
    license     = "CC BY 4.0",
    attribution = "van Kleunen M et al. (2019) The Global Naturalized Alien Flora (GloNAF) database. Ecology 100:e02542.",
    download_fn = function(url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      base <- "https://zenodo.org/records/13235357/files/"
      files <- c("glonaf_flora2.xlsx", "glonaf_taxon_wcvp.xlsx",
                 "glonaf_region.xlsx")
      for (f in files) {
        tryCatch(
          download_curl_file(paste0(base, f, "?download=1"), dest, f),
          error = function(e) {
            message(sprintf("  Warning: failed to download GloNAF %s: %s",
                            f, conditionMessage(e)))
          }
        )
      }
      dest
    },
    parse_fn    = function(path) parse_glonaf(path),
    group_col   = "region_id",
    requires    = "openxlsx2"
  ),

  leptraits = list(
    source_url  = "https://raw.githubusercontent.com/RiesLabGU/LepTraits/main/consensus/consensus.csv",
    source_doi  = "10.1038/s41597-022-01473-5",
    version     = "1.0",
    license     = "CC0",
    attribution = "Shirey V et al. (2022) LepTraits 1.0: A globally comprehensive dataset of butterfly traits. Scientific Data 9:398.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "consensus.csv")
    },
    parse_fn    = function(path) parse_leptraits(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  animaltraits = list(
    source_url  = "https://zenodo.org/record/6468938/files/observations.csv?download=1",
    source_doi  = "10.1038/s41597-022-01364-9",
    version     = "1.0",
    license     = "CC0",
    attribution = "Hebert K et al. (2022) AnimalTraits -- a curated animal trait database for body mass, metabolic rate and brain size. Scientific Data 9:265.",
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "observations.csv")
    },
    parse_fn    = function(path) parse_animaltraits(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  arthropod_traits = list(
    source_url  = "https://ipt.biodiversity.be/archive.do?r=arthropod-trait-dataset&v=1.1",
    source_doi  = "10.3897/BDJ.13.e146785",
    version     = "1.1",
    license     = "CC BY-NC",
    attribution = "Logghe A et al. (2025) An in-depth dataset of northwestern European arthropod life histories and ecological traits. Biodiversity Data Journal 13:e146785.",
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, pattern = NULL)
    },
    parse_fn    = function(path) parse_arthropod_traits(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  baseflor = list(
    source_url  = paste0("http://web.archive.org/web/20231002005253id_/",
                         "https://philippe.julve.pagesperso-orange.fr/baseflor.xlsx"),
    source_doi  = NULL,
    version     = "2023.10",
    license     = "ODbL 1.0 / CC BY-SA 2.0",
    attribution = paste0(
      "Julve, Ph. (1998 ff.) baseflor. Index botanique, ecologique et ",
      "chorologique de la Flore de France. Programme Catminat. Data under ",
      "ODbL 1.0 / CC BY-SA 2.0; archived snapshot 2023-10-02 of ",
      "philippe.julve.pagesperso-orange.fr (Orange host now retired; current ",
      "releases via tela-botanica.org phytosociologie porte-documents)."
    ),
    download_fn = function(url, dest) {
      download_curl_file(
        url, dest, "baseflor.xlsx",
        user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Firefox/120.0"
      )
    },
    parse_fn    = function(path) parse_baseflor(path),
    group_col   = NULL,
    requires    = "openxlsx2"
  ),

  ecoflora = list(
    source_url  = paste0("https://github.com/gcol33/taxifydb/releases/download/",
                         "scrape-snapshots-2026.06/ecoflora_raw_2026-06-23.csv"),
    source_doi  = NULL,
    version     = "2026.06",
    license     = "CC BY-NC-SA 4.0",
    attribution = paste0(
      "Fitter, A.H. & Peat, H.J. (1994) The Ecological Flora Database. ",
      "Journal of Ecology 82:415-425. https://www.ecoflora.org.uk/ ",
      "Ecoflora has no bulk download or API; the frozen snapshot was scraped ",
      "one species at a time (accessed 2026-06-23), and the access date is ",
      "the dataset version."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "ecoflora_raw.csv")
    },
    parse_fn    = function(path) parse_ecoflora(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  floraweb = list(
    source_url  = paste0("https://github.com/gcol33/taxifydb/releases/download/",
                         "scrape-snapshots-2026.06/floraweb_raw_2026-06-24.csv"),
    source_doi  = NULL,
    version     = "2026.06",
    license     = "free use with acknowledgement + citation (BioFresh metadata statement)",
    attribution = paste0(
      "FloraWeb. Daten und Informationen zu Wildpflanzen und zur Vegetation ",
      "Deutschlands. Bundesamt fuer Naturschutz, Bonn. ",
      "https://www.floraweb.de/ (accessed 2026-06-24). Trait data largely ",
      "derive from BiolFlor (Klotz, S., Kuehn, I. & Durka, W. 2002. BIOLFLOR. ",
      "Schriftenreihe fuer Vegetationskunde 38, Bundesamt fuer Naturschutz, ",
      "Bonn), with Rothmaler morphology and Ellenberg indicator values. ",
      "FloraWeb has no bulk export or API; the frozen snapshot was scraped ",
      "per species and the access date is the dataset version."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "floraweb_raw.csv")
    },
    parse_fn    = function(path) parse_floraweb(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  sealifebase = list(
    source_url  = "https://sealifebase.ropensci.org",
    source_doi  = NULL,
    version     = format(Sys.Date(), "%Y.%m"),
    license     = "CC BY-NC 3.0",
    attribution = "Palomares MLD, Pauly D (eds.) (2024) SeaLifeBase. World Wide Web electronic publication, https://www.sealifebase.org.",
    download_fn = function(url, dest) {
      # rfishbase fetches data directly; dest exists only so the interface
      # is uniform with file-based parsers.
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      dest
    },
    parse_fn    = function(path) parse_sealifebase(path),
    group_col   = NULL,
    requires    = "rfishbase"
  ),

  groot = list(
    source_url  = paste0("https://raw.githubusercontent.com/GRooT-Database/",
                         "GRooT-Data/master/DataFiles/",
                         "GRooTAggregateSpeciesVersion.zip"),
    source_doi  = "10.1111/geb.13179",
    version     = "2021.1",
    license     = "Free use with data-paper citation (no formal license stated)",
    attribution = paste0(
      "Guerrero-Ramirez NR et al. (2021) Global root traits (GRooT) ",
      "database. Global Ecology and Biogeography 30:25-37. Data are publicly ",
      "available from github.com/GRooT-Database/GRooT-Data and are used here ",
      "with the data-paper citation requested by the authors (no formal ",
      "licence is stated on the repository). Species-level means of the nine ",
      "best-populated key root traits."
    ),
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, "GRooTAggregateSpeciesVersion\\.csv$")
    },
    parse_fn    = function(path) parse_groot(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  globtherm = list(
    source_url  = "10.5061/dryad.1cv08",
    source_doi  = "10.1038/sdata.2018.22",
    version     = "2018.1",
    license     = "CC0",
    attribution = paste0(
      "Bennett JM et al. (2018) GlobTherm, a database on the thermal ",
      "tolerance for aquatic and terrestrial organisms. Scientific Data ",
      "5:180022. Data deposited under CC0 on Dryad (doi:10.5061/dryad.1cv08)."
    ),
    download_fn = function(url, dest) {
      download_dryad_file(url, dest, "GlobalTherm.csv",
                          file_pattern = "GlobalTherm.*\\.csv$")
    },
    parse_fn    = function(path) parse_globtherm(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  madin = list(
    source_url  = paste0("https://raw.githubusercontent.com/",
                         "bacteria-archaea-traits/bacteria-archaea-traits/",
                         "master/output/condensed_species_NCBI.csv"),
    source_doi  = "10.1038/s41597-020-0497-4",
    version     = "2020.1",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Madin JS et al. (2020) A synthesis of bacterial and archaeal ",
      "phenotypic trait data. Scientific Data 7:170. Species-aggregated ",
      "table (condensed_species_NCBI.csv) from ",
      "github.com/bacteria-archaea-traits/bacteria-archaea-traits."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "condensed_species_NCBI.csv")
    },
    parse_fn    = function(path) parse_madin(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  coral_traits = list(
    source_url  = "https://ndownloader.figshare.com/files/3678603",
    source_doi  = "10.1038/sdata.2016.17",
    version     = "1.1.1",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Madin JS et al. (2016) The Coral Trait Database, a curated database ",
      "of trait information for coral species from the global oceans. ",
      "Scientific Data 3:160017. Release ctdb_1.1.1 ",
      "(figshare doi:10.6084/m9.figshare.2067414.v1). Species-level values ",
      "are aggregated by taxifydb from the long-format observation table ",
      "(numeric traits by median, categorical traits by mode)."
    ),
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, "ctdb.*_data\\.csv$")
    },
    parse_fn    = function(path) parse_coral_traits(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  spider_traits = list(
    source_url  = paste0(
      "https://spidertraits.sci.muni.cz/backend/data/export/csv/",
      "family/*/genus/*/species/*/original-name/*/trait-category/*/trait/*/",
      "method/*/location/*/country/*/dataset/*/authors/*/reference/*/",
      "row-link/*"),
    source_doi  = "10.1093/database/baab064",
    version     = format(Sys.Date(), "%Y.%m"),
    license     = "CC BY 4.0",
    attribution = paste0(
      "Pekar S et al. (2021) The World Spider Trait database: a centralized ",
      "global open repository for curated data on spider traits. Database ",
      "2021:baab064. spidertraits.sci.muni.cz. Access-restricted records are ",
      "dropped; species-level values are aggregated by taxifydb (numeric by ",
      "median, categorical by mode). The live export is unversioned, so the ",
      "access date is the dataset version."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "wst_export.csv")
    },
    parse_fn    = function(path) parse_spider_traits(path),
    group_col   = NULL,
    requires    = "data.table"
  ),

  amniote = list(
    source_url  = "https://ndownloader.figshare.com/files/8067269",
    source_doi  = "10.1890/15-0846R.1",
    version     = "2015.1",
    license     = "CC0",
    attribution = paste0(
      "Myhrvold NP et al. (2015) An amniote life-history database to perform ",
      "comparative analyses with birds, mammals, and reptiles. Ecology ",
      "96:3109. Data deposited under CC0 ",
      "(figshare doi:10.6084/m9.figshare.3563457). The -999 missing-value ",
      "sentinel is converted to NA."
    ),
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, "Amniote_Database_Aug_2015\\.csv$")
    },
    parse_fn    = function(path) parse_amniote(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  combine = list(
    source_url  = "https://ndownloader.figshare.com/files/27703263",
    source_doi  = "10.1002/ecy.3344",
    version     = "1.0",
    license     = "CC0",
    attribution = paste0(
      "Soria CD et al. (2021) COMBINE: a coalesced mammal database of ",
      "intrinsic and extrinsic traits. Ecology 102:e03344 ",
      "(figshare doi:10.6084/m9.figshare.13028255). Reported (not imputed) ",
      "values, keyed on the IUCN 2020 binomial. COMBINE coalesces several ",
      "sources; it is shipped as its own enrichment, not as a replacement ",
      "for PanTHERIA or EltonTraits."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "trait_data_reported.csv")
    },
    parse_fn    = function(path) parse_combine(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  austraits = list(
    source_url  = paste0("https://zenodo.org/api/records/15718081/files/",
                         "austraits-7.0.0.zip/content"),
    source_doi  = "10.1038/s41597-021-01006-6",
    version     = "7.0.0",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Falster D et al. (2021) AusTraits, a curated plant trait database for ",
      "the Australian flora. Scientific Data 8:254. Release 7.0.0 ",
      "(Zenodo doi:10.5281/zenodo.15718081). Species-level values are ",
      "aggregated by taxifydb from the long traits table (numeric by median, ",
      "categorical by mode)."
    ),
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, "traits\\.csv$")
    },
    parse_fn    = function(path) parse_austraits(path),
    group_col   = NULL,
    requires    = "data.table"
  ),

  gwdd = list(
    source_url  = paste0("https://zenodo.org/api/records/18262736/files/",
                         "gwddagg_v2.2_species.csv/content"),
    source_doi  = "10.1111/nph.70860",
    version     = "2.2",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Fischer FJ et al. (2026) The Global Wood Density Database version 2. ",
      "New Phytologist (doi:10.1111/nph.70860); data Zenodo ",
      "doi:10.5281/zenodo.18262736. Species-aggregated wood specific gravity ",
      "(oven-dry mass / green volume, equal to g/cm3). Bark density is not in ",
      "the aggregated files and is not included."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "gwddagg_v2.2_species.csv")
    },
    parse_fn    = function(path) parse_gwdd(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  bet = list(
    source_url  = paste0("https://www.envidat.ch/dataset/",
                         "4865a082-169d-40d1-920b-fc20ad0acad2/resource/",
                         "d2d2f958-051c-4638-a808-88547cc64d92/download/",
                         "betdata.txt"),
    source_doi  = "10.16904/envidat.348",
    version     = "2023.1",
    license     = "CC BY-SA 4.0",
    attribution = paste0(
      "van Zuijlen K et al. (2023) Bryophytes of Europe Traits (BET): a ",
      "fundamental dataset for European bryophyte ecology. EnviDat ",
      "(doi:10.16904/envidat.348)."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "betdata.txt")
    },
    parse_fn    = function(path) parse_bet(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  phylacine = list(
    source_url  = paste0("https://raw.githubusercontent.com/MegaPast2Future/",
                         "PHYLACINE_1.2/master/Data/Traits/Trait_data.csv"),
    source_doi  = "10.1002/ecy.2443",
    version     = "1.2",
    license     = "CC0",
    attribution = paste0(
      "Faurby S et al. (2018) PHYLACINE 1.2: The Phylogenetic Atlas of Mammal ",
      "Macroecology. Ecology 99:2626. Data under CC0 ",
      "(doi:10.5061/dryad.bp26v20); built from the authors' GitHub mirror ",
      "(MegaPast2Future/PHYLACINE_1.2). Includes recently and prehistorically ",
      "extinct mammals."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "Trait_data.csv")
    },
    parse_fn    = function(path) parse_phylacine(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  chelonians = list(
    source_url  = "https://ndownloader.figshare.com/files/53840531",
    source_doi  = "10.6084/m9.figshare.28828241",
    version     = "2025.1",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Wang Y et al. (2025) CheloniansTraits: a comprehensive trait database ",
      "of global turtles and tortoises (figshare ",
      "doi:10.6084/m9.figshare.28828241). The literal \"NA\" is treated as ",
      "missing and \"min-max\" text ranges are reduced to their midpoint."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "CheloniansTraits.xlsx")
    },
    parse_fn    = function(path) parse_chelonians(path),
    group_col   = NULL,
    requires    = "openxlsx2"
  ),

  birdbase = list(
    source_url  = "https://ndownloader.figshare.com/files/55634729",
    source_doi  = "10.1038/s41597-025-05615-3",
    version     = "2025.1",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Sekercioglu CH et al. (2025) BIRDBASE: a global database of bird ",
      "ecological and life-history traits (figshare ",
      "doi:10.6084/m9.figshare.27051040). Traits redundant with AVONET ",
      "morphology are not carried."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "birdbase.xlsx")
    },
    parse_fn    = function(path) parse_birdbase(path),
    group_col   = NULL,
    requires    = "openxlsx2"
  ),

  tree_of_sex = list(
    source_url  = "10.5061/dryad.v1908",
    source_doi  = "10.1038/sdata.2014.15",
    version     = "2014.1",
    license     = "CC0",
    attribution = paste0(
      "The Tree of Sex Consortium (2014) Tree of Sex: a database of sexual ",
      "systems. Scientific Data 1:140015. Data under CC0 ",
      "(doi:10.5061/dryad.v1908). Plant, vertebrate and invertebrate tables ",
      "are row-bound on a common core with a taxon_group discriminator."
    ),
    download_fn = function(url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      zip <- download_dryad_file(url, dest, "tree_of_sex_plants.zip",
                                 file_pattern = "(^|/)plant.*\\.zip$")
      utils::unzip(zip, exdir = dest)
      download_dryad_file(url, dest, "vert.data.csv",
                          file_pattern = "(^|/)vert.*\\.csv$")
      download_dryad_file(url, dest, "invert.data.csv",
                          file_pattern = "(^|/)invert.*\\.csv$")
      dest
    },
    parse_fn    = function(path) parse_tree_of_sex(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  useful_plants = list(
    source_url  = paste0("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/",
                         "urn:uuid:e576e4b7-845a-422a-8472-b5eb078e08eb"),
    source_doi  = "10.5063/F1CV4G34",
    version     = "2020.1",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Diazgranados M et al. (2020) World Checklist of Useful Plant Species. ",
      "Knowledge Network for Biocomplexity (doi:10.5063/F1CV4G34). The ",
      "checklist is published only as a PDF; ten Level-1 use categories are ",
      "decoded into boolean columns plus a crop-wild-relative flag."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "WCUP_2020.pdf")
    },
    parse_fn    = function(path) parse_useful_plants(path),
    group_col   = NULL,
    requires    = "pdftools"
  ),

  bien = list(
    source_url  = "https://bien.nceas.ucsb.edu",
    source_doi  = "10.1111/2041-210X.12861",
    version     = format(Sys.Date(), "%Y.%m"),
    license     = "CC BY",
    attribution = paste0(
      "Maitner BS et al. (2018) The BIEN R package: A tool to access the ",
      "Botanical Information and Ecology Network (BIEN) database. Methods in ",
      "Ecology and Evolution 9:373-379. Public-access trait records ",
      "(access == \"public\") queried via the BIEN R package and aggregated to ",
      "species level (numeric by median, categorical by mode)."
    ),
    download_fn = function(url, dest) {
      # BIEN is queried directly by the parser; dest exists only so the
      # interface matches the file-based parsers.
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      dest
    },
    parse_fn    = function(path) parse_bien(path),
    group_col   = NULL,
    requires    = "BIEN"
  ),

  sharkipedia = list(
    source_url  = paste0("https://zenodo.org/records/6656525/files/",
                         "Sharkipedia-Traits-v1.0-22-01-25.csv?download=1"),
    source_doi  = "10.1038/s41597-022-01655-1",
    version     = "1.0",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Mull CG et al. (2022) Sharkipedia: a curated open access database of ",
      "shark and ray life history traits and abundance time-series. ",
      "Scientific Data 9:559. Data on Zenodo (doi:10.5281/zenodo.6656525). ",
      "Long-format trait records reduced to species-level medians by taxifydb."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "sharkipedia_traits.csv")
    },
    parse_fn    = function(path) parse_sharkipedia(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  bird_nest = list(
    source_url  = paste0("https://zenodo.org/records/10128906/files/",
                         "NestTrait_v2.csv?download=1"),
    source_doi  = "10.1038/s41597-023-02837-1",
    version     = "2.0",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Chia SY et al. (2023) A global database of bird nest traits. ",
      "Scientific Data (doi:10.1038/s41597-023-02837-1; data on Zenodo, ",
      "doi:10.5281/zenodo.10128906). Binary nest-site, nest-structure and ",
      "nest-attachment indicators, one row per species."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "nesttrait_v2.csv")
    },
    parse_fn    = function(path) parse_bird_nest(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  octocoral = list(
    source_url  = paste0("https://zenodo.org/records/14228404/files/",
                         "OctocoralTraits_v2_2.zip?download=1"),
    source_doi  = "10.5281/zenodo.14228404",
    version     = "2.2",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Octocoral Trait Database v2.2 (Gomez-Gras et al.). Zenodo ",
      "(doi:10.5281/zenodo.14228404), CC BY 4.0. Long-format trait records ",
      "reduced to species-level values by taxifydb (numeric by median, ",
      "categorical by mode); geographic/bioregion fields are not carried."
    ),
    download_fn = function(url, dest) {
      download_and_unzip(url, dest, "OctocoralTraits_v2_2\\.csv$")
    },
    parse_fn    = function(path) parse_octocoral(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  rimet_phyto = list(
    source_url  = paste0("https://zenodo.org/records/1164834/files/",
                         "Appendix-1-Phytoplankton%20metrics%20database-",
                         "revised.xlsx?download=1"),
    source_doi  = "10.5281/zenodo.1164834",
    version     = "2018.1",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Rimet F, Druart JC (2018) A trait database for phytoplankton of ",
      "temperate lakes. Zenodo (doi:10.5281/zenodo.1164834), CC BY 4.0. ",
      "Cell-level morphometrics, one row per taxon."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "rimet_phyto.xlsx")
    },
    parse_fn    = function(path) parse_rimet_phyto(path),
    group_col   = NULL,
    requires    = "openxlsx2"
  ),

  huang_amph = list(
    source_url  = "https://api.figshare.com/v2/articles/21159229",
    source_doi  = "10.6084/m9.figshare.21159229",
    version     = "2022.1",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Huang et al. A global amphibian morphological trait dataset (figshare ",
      "doi:10.6084/m9.figshare.21159229), CC BY 4.0. Per-specimen ",
      "morphometrics reduced to species-level medians by taxifydb for the ",
      "measurements comparable across Anura, Caudata and Gymnophiona."
    ),
    download_fn = function(url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      r <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(url)$content))
      for (nm in c("Anura.csv", "Caudata.csv", "Gymnophiona.csv")) {
        fu <- r$files$download_url[r$files$name == nm]
        if (length(fu)) {
          tryCatch(download_curl_file(fu[1], dest, nm),
                   error = function(e) message("  huang dl fail: ", nm))
        }
      }
      dest
    },
    parse_fn    = function(path) parse_huang_amph(path),
    group_col   = NULL,
    requires    = character(0)
  ),

  bee_ostwald = list(
    source_url  = paste0("https://zenodo.org/records/13366989/files/",
                         "Sup%20Table%204%20Morphological%20Dataset%20",
                         "Revised.csv?download=1"),
    source_doi  = "10.5281/zenodo.13366989",
    version     = "2024.1",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Ostwald MM et al. (2024) A global database of bee morphological traits. ",
      "Zenodo (doi:10.5281/zenodo.13366989), CC BY 4.0. Long-format Darwin Core ",
      "measurements reduced to species-level medians by taxifydb."
    ),
    download_fn = function(url, dest) {
      download_curl_file(url, dest, "bee_morphology.csv")
    },
    parse_fn    = function(path) parse_bee_ostwald(path),
    group_col   = NULL,
    requires    = "data.table"
  ),

  pottier = list(
    source_url  = "https://zenodo.org/api/records/6565454",
    source_doi  = "10.1038/s41597-022-01704-9",
    version     = "1.0",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Pottier P et al. (2022) A comprehensive database of amphibian heat ",
      "tolerance. Scientific Data 9:600 (doi:10.1038/s41597-022-01704-9; data ",
      "Zenodo doi:10.5281/zenodo.6565454), CC BY 4.0. Per-measurement upper ",
      "thermal limits reduced to species-level medians by taxifydb."
    ),
    download_fn = function(url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      r <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(url)$content))
      zu <- r$files$links$self[grepl("\\.zip$", r$files$key)][1]
      zp <- file.path(dest, "src.zip")
      if (!file.exists(zp)) {
        h <- curl::new_handle(); curl::handle_setopt(h, followlocation = TRUE)
        curl::curl_download(zu, zp, handle = h)
      }
      ex <- file.path(dest, "ex")
      if (!dir.exists(ex)) { dir.create(ex); utils::unzip(zp, exdir = ex) }
      list.files(ex, pattern = "Curated_data\\.csv$", recursive = TRUE,
                 full.names = TRUE)[1]
    },
    parse_fn    = function(path) parse_pottier(path),
    group_col   = NULL,
    requires    = "data.table"
  ),

  quimbayo = list(
    source_url  = "https://zenodo.org/api/records/4455016",
    source_doi  = "10.5281/zenodo.4455016",
    version     = "1.0",
    license     = "Open (ESA Ecology data policy)",
    attribution = paste0(
      "Quimbayo JP et al. (2021) Life-history traits, geographical range, and ",
      "conservation aspects of reef fishes from the Atlantic and Eastern ",
      "Pacific. Ecology (ESA data paper); data Zenodo ",
      "(doi:10.5281/zenodo.4455016). One row per species."
    ),
    download_fn = function(url, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      r <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(url)$content))
      zu <- r$files$links$self[grepl("\\.zip$", r$files$key)][1]
      zp <- file.path(dest, "src.zip")
      if (!file.exists(zp)) {
        h <- curl::new_handle(); curl::handle_setopt(h, followlocation = TRUE)
        curl::curl_download(zu, zp, handle = h)
      }
      ex <- file.path(dest, "ex")
      if (!dir.exists(ex)) { dir.create(ex); utils::unzip(zp, exdir = ex) }
      list.files(ex, pattern = "Fish_aspects.*\\.csv$", recursive = TRUE,
                 full.names = TRUE)[1]
    },
    parse_fn    = function(path) parse_quimbayo(path),
    group_col   = NULL,
    requires    = "data.table"
  )
)


# On-demand trait sources: NOT built into a `.vtr` by taxifydb.
#
# Pignatti's values originate in a copyrighted publication and cannot be
# redistributed, so taxifydb builds no `.vtr` for it; taxify's add_pignatti()
# reads the copy bundled in the TR8 package on the user's own machine.
#
# (Ecoflora and FloraWeb were previously here too. Both are now built into
# `.vtr` files by .enrichment_build_registry from frozen scrape snapshots:
# Ecoflora's CC BY-NC-SA licence permits redistribution, and FloraWeb -- the
# live BfN portal carrying the BiolFlor data -- may be used with
# acknowledgement and citation per the BioFresh metadata statement.)
#' @noRd
.enrichment_scrape_only <- list(
  pignatti = list(
    tr8_db      = "Pignatti",
    taxify_fn   = "add_pignatti",
    license     = "copyrighted",
    reason      = "values originate in a copyrighted publication",
    attribution = "Pignatti S, Menegoni P, Pietrosanti S (2005) Bioindicazione attraverso le piante vascolari. Braun-Blanquetia 39."
  )
)


#' List scrape-only (non-redistributed) trait sources
#'
#' taxifydb builds no `.vtr` for these sources because they cannot be
#' redistributed. Pignatti's values originate in a copyrighted publication and
#' are accessed on demand by taxify's `add_pignatti()` through the TR8 package
#' (which ships a copy under its own licence). This returns the catalog of
#' those sources.
#'
#' @return A data.frame with columns `name`, `tr8_db`, `taxify_fn`, `license`,
#'   and `reason`.
#' @export
list_scrape_only_enrichments <- function() {
  do.call(rbind, lapply(names(.enrichment_scrape_only), function(n) {
    e <- .enrichment_scrape_only[[n]]
    data.frame(
      name      = n,
      tr8_db    = e$tr8_db,
      taxify_fn = e$taxify_fn,
      license   = e$license,
      reason    = e$reason,
      stringsAsFactors = FALSE
    )
  }))
}
