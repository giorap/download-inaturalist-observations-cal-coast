#' ---
#' title: Deriving indicators of biodiversity change from unstructured community-contributed data <br> <strong> Download and filter iNaturalist observations
#' ---
#' 
#' ## Write custom functions to download and clean iNaturalist observations as json from iNaturalist API
#' ### Get iNaturalist observations from URL using API v1
get_inat_observations_fromURL <- function(URL){
  
  ping <- paste0(URL, "&page=1&per_page=1")
  
  max_results <- fromJSON(content(GET(ping), as = "text"))[[1]]
  
  dat <- vector("list", max_results/100)
  
  if (max_results%%100 == 0) {
    loopval <- max_results%/%100
  } else {
    loopval <- (max_results%/%100) + 1
  }
  i <- 1
  page_url <- paste0(URL, "&page=", i, "&per_page=100")
  cat(paste("-", i * 100, sep = ""))
  
  dat[[i]] <- fromJSON(content(GET(page_url), as = "text"))
  
  for (i in 2:loopval) {
    page_url = paste0(URL, "&per_page=100&id_below=", dat[[i-1]]$results$id[length(dat[[i-1]]$results$id)], sep = "")
    cat(paste("-", i * 100, sep = ""))
    dat[[i]] <- fromJSON(content(GET(page_url), as = "text"))
  }
  
  dat <- purrr::map(dat, clean_up_inat_json) %>% bind_rows()
  
  return(dat)
  
}
#'
#' ### Clean up json content downloaded from iNaturalist API v1
clean_up_inat_json <- function(json_content){
  
  json_content <- json_content$result
  
  cleaned_obs <- data.frame(
    id = json_content$id,
    occurrenceID = json_content$uri,
    recordedBy = json_content$user$login,
    eventDate = json_content$observed_on_details$date,
    eventTime = substr(json_content$time_observed_at, 12, 19),
    json_content$observed_on_details[, -1] %>% as.data.frame(),
    location = json_content$location,
    verbatimLocality = json_content$place_guess,
    decimalLongitude = do.call("rbind", json_content$geojson$coordinates)[, 1] %>% as.numeric(),
    decimalLatitude = do.call("rbind", json_content$geojson$coordinates)[, 2] %>% as.numeric(),
    coordinateUncertaintyInMeters = json_content$positional_accuracy,
    taxonID = json_content$taxon$id,
    scientificName = json_content$taxon$name,
    taxonRank = json_content$taxon$rank
  )
  
  return(cleaned_obs)
  
}
#'
#' ## Download observations from iNaturalist 
#' ### This code identifies all Research Grade iNaturalist observations of the broader taxa of interest overlapping spatial boundaries of interest
#' ### Download iNaturalist observations for all intertidal species except plants, red and green algae:
inaturalist_data_north <- get_inat_observations_fromURL("https://api.inaturalist.org/v1/observations?d1=2010-01-01&quality_grade=research&coordinates_obscured=false&taxon_ids=1,48222&without_taxon_id=47158,47119,20978,26036,3,40151,48295,47777&place_id=133888")
inaturalist_data_central <- get_inat_observations_fromURL("https://api.inaturalist.org/v1/observations?d1=2010-01-01&quality_grade=research&coordinates_obscured=false&taxon_ids=1,48222&without_taxon_id=47158,47119,20978,26036,3,40151,48295,47777&place_id=133886")
inaturalist_data_south <- get_inat_observations_fromURL("https://api.inaturalist.org/v1/observations?d1=2010-01-01&quality_grade=research&coordinates_obscured=false&taxon_ids=1,48222&without_taxon_id=47158,47119,20978,26036,3,40151,48295,47777&place_id=133884")
#' ### Download observations for intertidal plants, red and green algae:
inaturalist_data_additional <- get_inat_observations_fromURL("https://api.inaturalist.org/v1/observations?d1=2010-01-01&quality_grade=research&coordinates_obscured=false&taxon_ids=72297,52616,57774,50863&place_id=133884,133886,133888")
#' ### Combine all iNaturalist observations
inaturalist_data <- rbind(inaturalist_data_north, inaturalist_data_central, inaturalist_data_south, inaturalist_data_additional)
#' #### Remove unneeded objects and save seralized object
rm(inaturalist_data_additional, inaturalist_data_central, inaturalist_data_north, inaturalist_data_south)
#' ### Load serialized object downloaded on 2020 12 30
#' ### inaturalist_data <- readRDS("data/inaturalist_data-20201230.rds")
#'
#' ## Filter iNaturalist observations
#' ### Remove terrestrial mollusks
ca_terrestrial_mollusks <- c("Discus whitneyi", "Discus selenitoides", "Discus rotundatus", "Speleodiscoides spirellum", "Oreohelix californica", "Oreohelix handi", "Radiocentrum avalonense", "Lucilla singleyana", "Ancotrema hybridum", "Ancotrema sportella", "Ancotrema sportella sinkyonum", "Ancotrema voyanum", "Ancotrema zopherum", "Haplotrema minimum", "Haplotrema vancouverense", "Haplotrema caelatum", "Haplotrema alameda", "Haplotrema catalinense", "Haplotrema costatum", "Haplotrema duranti", "Haplotrema duranti continentis", "Haplotrema duranti duranti", "Haplotrema guadalupense", "Haplotrema keepi", "Haplotrema mokelumnense", "Haplotrema transfuga", "Euglandina rosea", "Cernuella virgata", "Cochlicella barbara", "Xerotricha conspurcata", "Monadenia circumcarinata", "Monadenia mormonum", "Monadenia mormonum buttoni", "Monadenia mormonum hillebrandi", "Monadenia mormonum hirsuta", "Monadenia tuolumneana", "Monadenia yosemitensis", "Monadenia callipeplus", "Monadenia chaceana", "Monadenia cristulata", "Monadenia fidelis", "Monadenia fidelis leonina", "Monadenia fidelis pronotis", "Monadenia infumata", "Monadenia infumata infumata", "Monadenia infumata ochromphalus", "Monadenia infumata setosa", "Monadenia infumata subcarinata", "Monadenia marmarotis", "Monadenia churchi", "Monadenia troglodytes", "Monadenia troglodytes wintu", "Mohavelix micrometalleus", "Sonorelix avawatzica", "Sonorelix baileyi", "Sonorelix borregoensis", "Sonorelix harperi", "Sonorelix melanopylon", "Sonorelix rixfordi", "Micrarionta beatula", "Micrarionta facta", "Micrarionta feralis", "Micrarionta gabbi", "Micrarionta intermedia", "Micrarionta rufocincta", "Micrarionta opuntia", "Chamaearionta aquaealbae", "Herpeteros angelus", "Plesarionta stearnsiana", "Xerarionta tryoni", "Xerarionta intercisa", "Xerarionta kellettii", "Xerarionta redimita", "Cahuillus greggi", "Cahuillus indioensis", "Cahuillus indioensis cathedralis", "Cahuillus indioensis indioensis", "Eremarionta brunnea", "Eremarionta immaculata", "Eremarionta millepalmarum", "Eremarionta morongoana", "Eremarionta orocopia", "Eremarionta rowelli", "Eremarionta rowelli bakerensis", "Eremarionta rowelli mccoiana", "Eremarionta argus", "Noyo intersessa", "Rothelix cuyamacensis", "Rothelix lowei", "Rothelix rhodophila", "Rothelix warnerfontis", "Helminthoglypta ayresiana", "Helminthoglypta carpenteri", "Helminthoglypta coelata", "Helminthoglypta fieldi", "Helminthoglypta morroensis", "Helminthoglypta phlyctaena", "Helminthoglypta reediana", "Helminthoglypta salviae", "Helminthoglypta sanctaecrucis", "Helminthoglypta similans", "Helminthoglypta tejonis", "Helminthoglypta traskii", "Helminthoglypta traskii isidroensis", "Helminthoglypta traskii pacoimensis", "Helminthoglypta traskii traskii", "Helminthoglypta uvasana", "Helminthoglypta vasquezi", "Helminthoglypta walkeriana", "Helminthoglypta willetti", "Helminthoglypta caruthersi", "Helminthoglypta concolor", "Helminthoglypta crotalina", "Helminthoglypta fisheri", "Helminthoglypta fontiphila", "Helminthoglypta graniticola", "Helminthoglypta greggi", "Helminthoglypta isabella", "Helminthoglypta jaegeri", "Helminthoglypta micrometalleoides", "Helminthoglypta mohaveana", "Helminthoglypta petricola", "Helminthoglypta taylori", "Helminthoglypta venturensis", "Helminthoglypta allynsmithi", "Helminthoglypta arrosa", "Helminthoglypta arrosa arrosa", "Helminthoglypta arrosa monticola", "Helminthoglypta arrosa pomoensis", "Helminthoglypta avus", "Helminthoglypta benitoensis", "Helminthoglypta berryi", "Helminthoglypta californiensis", "Helminthoglypta callistoderma", "Helminthoglypta contracostae", "Helminthoglypta cuyama", "Helminthoglypta cypreophila", "Helminthoglypta diabloensis", "Helminthoglypta dupetithouarsii", "Helminthoglypta edwardsi", "Helminthoglypta euomphalodes", "Helminthoglypta exarata", "Helminthoglypta expansilabris", "Helminthoglypta expansilabris mattolensis", "Helminthoglypta ferrissi", "Helminthoglypta hertleini", "Helminthoglypta inglesi", "Helminthoglypta liodoma", "Helminthoglypta mailliardi", "Helminthoglypta milleri", "Helminthoglypta montezuma", "Helminthoglypta napaea", "Helminthoglypta nickliniana", "Helminthoglypta nickliniana anachoreta", "Helminthoglypta nickliniana awania", "Helminthoglypta orina", "Helminthoglypta piutensis", "Helminthoglypta proles", "Helminthoglypta sequoicola", "Helminthoglypta sequoicola consors", "Helminthoglypta sonoma", "Helminthoglypta stageri", "Helminthoglypta stiversiana", "Helminthoglypta stiversiana miwoka", "Helminthoglypta stiversiana williamsi", "Helminthoglypta talmadgei", "Helminthoglypta thermimontis", "Helminthoglypta tudiculata", "Helminthoglypta fairbanksi", "Helminthoglypta tularensis", "Helminthoglypta umbilicata", "Helminthoglypta waltoni", "Cepaea nemoralis", "Massylaea vermiculata", "Cantareus apertus", "Cornu aspersum", "Otala lactea", "Theba pisana", "Vespericola armiger", "Vespericola embertoni", "Vespericola eritrichius", "Vespericola euthales", "Vespericola haplus", "Vespericola karokorum", "Vespericola klamathicus", "Vespericola marinensis", "Vespericola megasoma", "Vespericola orius", "Vespericola pilosus", "Vespericola pinicola", "Vespericola pressleyi", "Vespericola rhodophila", "Vespericola rothi", "Vespericola sasquatch", "Vespericola scotti", "Vespericola shasta", "Vespericola sierranus", "Trilobopsis loricata", "Trilobopsis loricata nortensis", "Trilobopsis penitens", "Trilobopsis roperi", "Trilobopsis tehamana", "Trilobopsis trachypepla", "Ammonitella yatesii", "Glyptostoma gabrielense", "Glyptostoma newberryanum", "Megomphix californicus", "Polygyroidea harfordiana", "Catinella gabbii", "Catinella rehderi", "Catinella stretchiana", "Catinella vermeta", "Oxyloma chasmodes", "Oxyloma retusum", "Oxyloma sillimani", "Succinea ovalis", "Succinea luteola", "Succinea californica", "Succinea rusticana", "Testacella haliotidea", "Paralaoma servilis", "Punctum californicum", "Punctum hannai", "Punctum minutissimum", "Binneya notabilis", "Arion rufus", "Arion silvaticus", "Arion distinctus", "Arion hortensis", "Arion intermedius", "Arion subfuscus", "Anadenulus cockerelli", "Prophysaon fasciatum", "Prophysaon andersoni", "Prophysaon dubium", "Ariolimax buttoni", "Ariolimax columbianus", "Ariolimax stramineus", "Ariolimax californicus", "Ariolimax dolichophallus", "Hesperarion hemphilli", "Hesperarion niger", "Hesperarion plumbeus", "Hawaiia minuscula", "Pristiloma chersinella", "Pristiloma gabrielinum", "Pristiloma nicholsoni", "Pristiloma orotis", "Pristiloma shepardae", "Pristiloma spelaeum", "Pristiloma lansingi", "Pristiloma cavator", "Vitrea contracta", "Euconulus fulvus", "Striatura pugetensis", "Zonitoides arboreus", "Oxychilus alliarius", "Oxychilus cellarius", "Oxychilus draparnaudi", "Nesovitrea binneyana", "Milax gagates", "Vitrina pellucida", "Ambigolimax valentianus", "Limacus flavus", "Limax maximus", "Deroceras reticulatum", "Deroceras laeve", "Deroceras monentolophus", "Deroceras panormitanum", "Cochlicopa lubrica", "Cochlicopa morseana", "Pupilla hebes", "Pupoides albilabris", "Gastrocopta pellucida", "Gastrocopta pentodon", "Vertigo clementina", "Sterkia hemphilli", "Nearctula rowellii", "Nearctula rowellii catalinaria", "Nearctula rowellii cupressicola", "Vertigo andrusiana", "Vertigo andrusiana sanbernardinensis", "Vertigo berryi", "Vertigo modesta", "Vertigo modesta castanea", "Vertigo occidentalis", "Vertigo ovata", "Vertigo sterkii", "Columella edentula", "Planogyra clappi", "Vallonia costata", "Vallonia cyclophorella", "Vallonia excentrica", "Vallonia pulchella", "Cecilioides acicula", "Rumina decollata", "Monadenia mormonum cala", "Monadenia fidelis smithiana", "Helminthoglypta nickliniana bridgesi", "Helminthoglypta nickliniana ramentosa", "Helminthoglypta proles mariposa", "Prophysaon vanattae", "Haplotrema sportella", "Haplotrema voyanum", "Helicodiscus singleyanus", "Eremarionta rowelli chuckwallana", "Eremarionta rowelli granitensis", "Eremariontoides argus", "Helminthoglypta rhodophila", "Sonorella bowiensis", "Monadenia scottiana", "Monadenia setosa", "Vespericola ohlone", "Pristiloma stearnsi", "Punctum randolphi", "Zonites diegoensis", "Catinella gabbi", "Oxyloma nuttallianum", "Succinea oregonensis", "Vertigo allyniana", "Vertigo dalliana", "Boettgerilla pallens")
inaturalist_data <- inaturalist_data %>% 
  dplyr::filter(!(scientificName %in% ca_terrestrial_mollusks))
rm(ca_terrestrial_mollusks)
#' ### Remove duplicate observations which could be of the same individual or colony
inaturalist_data <- inaturalist_data %>% 
  distinct(scientificName, eventDate, decimalLongitude, decimalLatitude, .keep_all = TRUE)
#' ### Filter out observations with a coordinateUncertaintyInMeters higher than 1000 meters
inaturalist_data <- inaturalist_data %>% 
  dplyr::filter(coordinateUncertaintyInMeters < 1000 | is.na(coordinateUncertaintyInMeters))