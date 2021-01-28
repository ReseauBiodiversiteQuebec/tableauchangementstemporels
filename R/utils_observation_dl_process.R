
# simple function to get the data
# TODO add api request to atlas later
#' @export
get_data <- function(){
  dat <- tableauchangementstemporels::inatQC
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
#' @export
find_top_ten <- function(dataset, how_many_top = 10){
  
  assertthat::assert_that(
    assertthat::has_name(dataset, c("NOM_PROV_N"))
  )
  
  top10species <- dataset %>% 
    dplyr::count(NOM_PROV_N, taxon_species_name) %>% 
    dplyr::group_by(NOM_PROV_N) %>%
    dplyr::arrange(NOM_PROV_N,desc(n)) %>%
    tidyr::nest(.) %>%
    dplyr::mutate(top10 = purrr::map(data, head, how_many_top)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(top10) %>% 
    dplyr::select(-n) %>% 
    dplyr::ungroup(.)
  
  return(top10species)
}


#' count observations on each day of the year for each species
#' @importFrom magrittr %>%
#' @export
count_taxa_julday <- function(dataset){
  count_taxa <- dataset %>%
    dplyr::mutate(julianday = lubridate::yday(observed_on)) %>%
    dplyr::group_by(NOM_PROV_N, taxon_species_name, julianday) %>% 
    dplyr::tally(.)
  
  return(count_taxa)
}

#' take only the top 10 species in each site
#' 
#' @importFrom magrittr %>%
#' @export
filter_julday_by_top <- function(count_taxa_df = count_taxa,
                                 top_spp = top10species){
  top_10_julian_day <- count_taxa_df %>%
    dplyr::semi_join(top_spp, by = c("NOM_PROV_N", "taxon_species_name"))
  return(top_10_julian_day)
}
  
#' fill in a gantt chart
#' 
#' 
#' @importFrom magrittr %>%
#' @export
convert_julian_to_gantt <- function(.top_10_julian_day = top_10_julian_day){
  .top_10_julian_day %>%
    dplyr::group_by(NOM_PROV_N, taxon_species_name) %>% 
    tidyr::nest(.) %>% 
    dplyr::mutate(purrr::map_df(data, ~ tibble::tibble(start = min(.x$julianday), 
                                         end = max(.x$julianday)))) %>% 
    dplyr::select(-data) %>% 
    dplyr::ungroup(.) %>% 
    tidyr::pivot_longer(cols = c("start", "end"), names_to = "dayname", values_to = "jday")
}




select_top_n <- function(){
  observations <- get_data()
  
  top_ten <- find_top_ten(observations)
  
  top_ten_julday <- count_taxa_julday(observations)
  
  filtered_by_top <- filter_julday_by_top(count_taxa = top_ten_julday, top_spp = top_ten)
  
  return(filtered_by_top)
}

select_top_n_df_input <- function(df, .how_many_top = 10){
  
  top_ten <- find_top_ten(df, how_many_top = .how_many_top)
  
  top_ten_julday <- count_taxa_julday(df)
  
  filtered_by_top <- filter_julday_by_top(count_taxa = top_ten_julday,
                                          top_spp = top_ten)
  
  return(filtered_by_top)
}



format_for_gantt_figure <- function(.filtered_by_top){
  gantt_observations <- convert_julian_to_gantt(.filtered_by_top)
  return(gantt_observations)
}


format_for_count_figure <- function(.filtered_by_top){
  
  
  chosen_species_range_days <- .filtered_by_top %>%
    dplyr::group_by(NOM_PROV_N, taxon_species_name) %>% 
    dplyr::summarize(jday = range(julianday)) %>%
    # rank species -- flexibly, for those seen only one day
    dplyr::mutate(dayname = c("start", "end")) %>% 
    tidyr::pivot_wider(names_from = dayname, values_from = jday) 
  
  # browser()
  n_per_day <- chosen_species_range_days %>%
    dplyr::mutate(dayrange = purrr::map2(start, end, ~.x:.y)) %>%
    dplyr::select(dayrange) %>%
    tidyr::unnest(cols = c(dayrange)) %>%
    dplyr::ungroup(.) %>% 
    dplyr::group_by(NOM_PROV_N, dayrange, .add = FALSE) %>% 
    dplyr::tally(.) %>%
    # grouped by region
    tidyr::nest(.) %>% 
    dplyr::mutate(data2 = purrr::map(data, dplyr::right_join, 
                                     y = tibble::tibble(dayrange = 1:365),
                                     by = "dayrange")) %>%
    dplyr::select(-data) %>% 
    tidyr::unnest(data2) %>% 
    tidyr::replace_na(list(n = 0)) %>%
    dplyr::arrange(NOM_PROV_N, dayrange) %>% 
    dplyr::ungroup(.) %>% 
    as.data.frame(.)
  
  return(n_per_day)
}






#' function that takes a site name and spits out a plot
#' 
#' @importFrom magrittr %>%
#' @export
filter_plot_gantt <- function(site_selected, gantt_df){

  renderPlot({
  ggplot2::ggplot(
    subset(gantt_df, gantt_df$NOM_PROV_N == site_selected()),  
    ggplot2::aes(x = jday, y = taxon_species_name)   
    ) +
    ggplot2::geom_line(size = 20, col = "darkgreen") +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(xlim = c(0,365)) +
    ggplot2::labs(x = "Jour de l'année", 
                  y = NULL)
  })
}

filter_plot_count <- function(site_selected, count_df){
  renderPlot({
    ggplot2::ggplot(
      subset(count_df, count_df$NOM_PROV_N == site_selected()),  
      ggplot2::aes(x = dayrange, y = n)) + 
      ggplot2::geom_polygon() + 
      ggplot2::theme_minimal() + 
      ggplot2::coord_cartesian(xlim = c(0,365)) +
      ggplot2::labs(x = "Jour de l'année", 
                    y = "Richess d'especes")
  })
}

#' @import patchwork 
plot_both_together <- function(site_selected, gantt_df, count_df){
  
  renderPlot({
    ganttplot <-   ggplot2::ggplot(
      subset(gantt_df, gantt_df$NOM_PROV_N == site_selected()),  
      ggplot2::aes(x = jday, y = taxon_species_name)   
    ) +
      ggplot2::geom_line(size = 20, col = "darkgreen") +
      ggplot2::theme_minimal() +
      ggplot2::coord_cartesian(xlim = c(0,365)) +
      ggplot2::labs(x = "Jour de l'année", 
                    y = NULL)
    
    countplot <-     ggplot2::ggplot(
      subset(count_df, count_df$NOM_PROV_N == site_selected()),  
      ggplot2::aes(x = dayrange, y = n)) + 
      ggplot2::geom_area() + 
      ggplot2::theme_minimal() + 
      ggplot2::coord_cartesian(xlim = c(0,365)) +
      ggplot2::labs(x = "Jour de l'année", 
                    y = "Richess d'especes")
    
    ganttplot / countplot
  
  })
}

plot_some_bats <- function(site_selected, bat_df){
  
  plotly::renderPlotly({
    p <- 
      ggplot2::ggplot(
        subset(bat_df, bat_df$Numero.de.reference.du.site == site_selected())) +
      ggplot2::aes(x = wk, y = n, fill = match, text = match) + 
      ggplot2::geom_area() + 
      ggplot2::scale_fill_brewer(palette = "Dark2") + 
      ggplot2::guides(fill = FALSE) + 
      ggplot2::theme_bw() +
      ggplot2::labs(y = "Chiropteres observés", x = "Semaine de l'année")
    
    plotly::ggplotly(p, tooltip = "text") %>% 
      plotly::layout(legend = list(orientation = 'h',
                                   x = 0, 
                                   y = -0.4))
  })  
  
}