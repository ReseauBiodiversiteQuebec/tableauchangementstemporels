
# simple demo function for testing the icon plotting
testapp_bat_maps <- function(){
  ui <- fluidPage(
    fa_dependency(),
    mapselector::mod_map_select_ui("bat_map"),
    verbatimTextOutput("sel")
  )
  
  server <-  function(input, output, session) {
    
    
    all_sites <- rcoleo::download_sites_sf()
    
    target_sites <- mapselector::subset_site_df(downloaded_sites = all_sites,
                                                campaign_type = "acoustique")
    
    
    selsite <- mapselector::mod_map_select_server("bat_map",
                          what_to_click = "marker",
                          fun = mapselector::plot_rcoleo_sites,
                          rcoleo_sites_sf = target_sites)
    
    ff <- reactive({mapselector::get_subset_site(site = target_sites,
                                                 site_code_sel = selsite())})
    
    output$sel <- renderPrint(str(ff()))
  }
  shinyApp(ui, server)
}

testapp_bat_maps()



#' count species per day
#'
#' @param selected_site_result the output of mapselector::get_subset_site
#'
#' @return data frame with days as data
#' @importFrom magrittr %>%
#' @export
count_day_species <- function(selected_site_result){
  
  one_site_bats <- selected_site_result
  
  count_day_spp <- one_site_bats %>% 
    dplyr::select(taxa = obs_species.taxa_name, date_obs) %>% 
    dplyr::count(taxa, date_obs) %>% 
    dplyr::mutate(date_obs = lubridate::ymd(date_obs))
  
  return(count_day_spp)
}



#' Count the weeks and complete them
#'
#' @param count_day_spp_df output of count_day_species
#'
#' @return data frame with completed weeks
#' @importFrom magrittr %>%
#' @export
complete_weeks <- function(count_day_spp_df){
  count_complete_weeks <- count_day_spp_df %>% 
    dplyr::mutate(wk = lubridate::week(date_obs)) %>% 
    dplyr::group_by(taxa, wk) %>% 
    dplyr::tally(.) %>% 
    tidyr::complete(taxa, wk = 16:41,
             fill = list(n = 0))
  
  return(count_complete_weeks)
}



#' take a bat df and make a bat plot
#'
#' @param df data frame as returned by mapselector::get_subset_site
#'
#' @return
#' @importFrom magrittr %>%
#' @export
df_to_plot <- function(df){
  complete_bats <- df %>% 
    count_day_species(.) %>% 
    complete_weeks(.)
  
  
  complete_bats %>% 
    plot_some_bats(.)
}