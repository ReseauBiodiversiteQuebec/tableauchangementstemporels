
# simple function to get the data
# TODO add api request to atlas later
get_data <- function(){
  dat <- tableauphenologie::inatqc
  dat_reg <- mapselector::add_region(dat)
  
  dd_no_geom <- subset(as.data.frame(dat_reg), select = -geometry)
  return(dd_no_geom)
}

# # they're not the same regions are they
# inat_region <- mapselector::add_region(tableauphenologie::inatqc)
# 
# dplyr::distinct(dplyr::select(as.data.frame(inat_region), NOM_PROV_N, region))

  
#' identify the top 10 species in every site
#' @importFrom magrittr %>%
find_top_ten <- function(dataset){
  
  assertthat::assert_that(
    assertthat::has_name(dataset, c("NOM_PROV_N"))
  )
  
  top10species <- dd %>% 
    dplyr::count(NOM_PROV_N, taxon_species_name) %>% 
    dplyr::group_by(NOM_PROV_N) %>%
    dplyr::arrange(NOM_PROV_N,desc(n)) %>%
    tidyr::nest(.) %>%
    dplyr::mutate(top10 = purrr::map(data, head, 10)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(top10) %>% 
    dplyr::select(-n) %>% 
    dplyr::ungroup(.)
  
  return(top10species)
}


#' count observations on each day of the year for each species
#' @importFrom magrittr %>%
count_taxa_julday <- function(dataset){
  count_taxa <- dataset %>%
    dplyr::mutate(julianday = lubridate::yday(observed_on)) %>%
    dplyr::group_by(NOM_PROV_N, taxon_species_name, julianday) %>% 
    dplyr::tally
  
  return(count_taxa)
}

#' take only the top 10 species in each site
#' 
#' @importFrom magrittr %>%
filter_julday_by_top <- function(count_taxa = count_taxa,
                                 top_spp = top10species){
  top_10_julian_day <- count_taxa %>%
    dplyr::semi_join(top10species, by = c("NOM_PROV_N", "taxon_species_name"))
  return(top_10_julian_day)
}
  

convert_julian_to_gantt <- function(.top_10_julian_day = top_10_julian_day){
  .top_10_julian_day %>%
    dplyr::group_by(NOM_PROV_N, taxon_species_name) %>% 
    tidyr::nest %>% 
    dplyr::mutate(purrr::map_df(data, ~ tibble::tibble(start = min(.x$julianday), 
                                         end = max(.x$julianday)))) %>% 
    dplyr::select(-data) %>% 
    dplyr::ungroup %>% 
    tidyr::pivot_longer(cols = c("start", "end"), names_to = "dayname", values_to = "jday")
}
  
# dd <- get_data()
# find_top_ten(dd)
# 
# library(magrittr)
# 
# %>%
#   dplyr::arrange(NOM_PROV_N,desc(n))
# 
# make_gantt_from_observations <- function(dd){
#   
# }
  