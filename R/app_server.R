#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import mapselector
#' @noRd
app_server <- function( input, output, session ) {
  
  
  
  # regional phenology ------------------------------------------------------
  
  
  # Your application server logic 
  chosen_region <- mod_map_select_server("reg_map",
                                         what_to_click = "shape",
                                         fun = make_leaflet_map,
                                         # these are arguments to make_leaflet_map
                                         mapdata = mapselector::CERQ,
                                         label = TRUE,
                                         region_name = "NOM_PROV_N")
  
  
  sample_data <- select_top_n_df_input(tableauchangementstemporels::data_with_region,
                                       .how_many_top = 7)
  
  gantt_data <- format_for_gantt_figure(sample_data)
  
  count_data <- format_for_count_figure(sample_data)
  
  
  mod_modal_make_server("modal_make_ui_1", 
                        region = chosen_region,
                        title_format_pattern = "La phénologie pour la region %s",
                        # here place all the tabs you want in your final modal! 
                        ## this can be a function which returns a reactive output (e.g. renderPlotly)
                        tabPanel(title = "Visualization",
                                 plot_both_together(chosen_region, gantt_data, count_data))
  )
  
  
  # bat sites ---------------------------------------------------------------
  
  ## replace with mapselector::subset_site_df
  rcoleo_sites_sf <- rcoleo::download_sites_sf(token = rcoleo:::bearer())
  
  rcoleo_sites_bats <- mapselector::subset_site_df(downloaded_sites = rcoleo_sites_sf,
                              campaign_type = "acoustique")
  
  chosen_site <- mod_map_select_server("bat_map",
                                       what_to_click = "marker",
                                       fun = plot_rcoleo_sites,
                                       rcoleo_sites_sf = rcoleo_sites_bats)
  
  mod_observation_display_server("siteobs", 
                                 site = rcoleo_sites_bats, 
                                 region = chosen_site)
  
  to_show <- reactive({mapselector::get_subset_site(site = rcoleo_sites_bats, 
                                                    site_code_sel = chosen_site())})
  
  mod_modal_make_server("modal_make_ui_bats", 
                        region = chosen_site,
                        title_format_pattern = "Les chauves-souris de la region %s",
                        # here place all the tabs you want in your final modal! 
                        ## this can be a function which returns a reactive output (e.g. renderPlotly)
                        tabPanel(title = "Visualization",
                                 plotly::renderPlotly(
                                   df_to_plot(
                                     to_show()
                                   ))),
                        tabPanel(title = "Observations",
                                 mod_observation_display_ui("siteobs")
                        )
  )

}
