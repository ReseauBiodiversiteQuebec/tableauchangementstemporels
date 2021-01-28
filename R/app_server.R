#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Your application server logic 
  output$map <- leaflet::renderLeaflet(make_leaflet_map())
  
  output$batmap <- leaflet::renderLeaflet(make_leaflet_batmap())
  
  sample_data <- select_top_n_df_input(tableauchangementstemporels::data_with_region,
                                       .how_many_top = 7)
  
  gantt_data <- format_for_gantt_figure(sample_data)
  
  count_data <- format_for_count_figure(sample_data)
  
  # convenient reactive value for the clicked region
  chosen_region <- reactive({input$map_shape_click$id})
  
  # on a different map, with a different thing to click:
  chosen_site <- reactive({input$batmap_marker_click$id})
  
  mod_modal_make_server("modal_make_ui_1", 
                        region = chosen_region,
                        # here place all the tabs you want in your final modal! 
                        ## this can be a function which returns a reactive output (e.g. renderPlotly)
                        tabPanel(title = "Visualization",
                                 plot_both_together(chosen_region, gantt_data, count_data))
  )
  
  bat_df_saved <- tableauchangementstemporels::common_bats_wk
  
  mod_modal_make_server("modal_make_ui_bats", 
                        region = chosen_site,
                        # here place all the tabs you want in your final modal! 
                        ## this can be a function which returns a reactive output (e.g. renderPlotly)
                        tabPanel(title = "Visualization",
                                 plot_some_bats(chosen_site, bat_df_saved))
  )
  
  output$what_does_the_badge_say <- renderText({paste("
    Les données scientifiques recueillies par les citoyens nous donnent des informations précieuses sur la biodiversité québécoise. 
<br><br>
Ce tableau de bord indique quels animaux ont été vus dans quelles régions du Québec.
<br><br>
Il indique également <b>quand</b> cette information a été recueillie. Les animaux et les plantes sont moins actifs en hiver, et plus actifs en été. En explorant ces données, n'oubliez pas : il en va de même pour les personnes qui les observent !
    ")#, input$tabs)
    })
    
}
