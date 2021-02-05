#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Your application server logic 
  output$map <- leaflet::renderLeaflet(make_leaflet_map())
  chosen_region <- mod_map_select_server("reg_map",
                                       what_to_click = "shape",
                                       fun = make_leaflet_map,
                                       # these are arguments to make_leaflet_map
                                       mapdata = mapselector::CERQ,
                                       label = TRUE,
                                       region_name = "NOM_PROV_N")
  
  
  chosen_site <- mod_map_select_server("bat_map",
                                       what_to_click = "marker",
                                       fun = make_leaflet_batmap)
  
  
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
  
  bat_df_saved <- tableauchangementstemporels::common_bats_wk
  
  mod_modal_make_server("modal_make_ui_bats", 
                        region = chosen_site,
                        title_format_pattern = "Les chauves-souris de la region %s",
                        # here place all the tabs you want in your final modal! 
                        ## this can be a function which returns a reactive output (e.g. renderPlotly)
                        tabPanel(title = "Visualization",
                                 plot_some_bats(chosen_site, bat_df_saved))
  )
}
