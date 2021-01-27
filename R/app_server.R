#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Your application server logic 
  output$map <- leaflet::renderLeaflet(make_leaflet_map())
  
  sample_data <- select_top_n()
  
  gantt_data <- format_for_gantt_figure(sample_data)
  
  count_data <- format_for_count_figure(sample_data)
  
  # convenient reactive value for the clicked region
  chosen_region <- reactive({input$map_shape_click$id})
  
  mod_modal_make_server("modal_make_ui_1", 
                        region = chosen_region,
                        # here place all the tabs you want in your final modal! 
                        ## this can be a function which returns a reactive output (e.g. renderPlotly)
                        tabPanel(title = "Visualization",
                                 # see mapselector::ipso_zoo for an example
                                 filter_plot_gantt(chosen_region, gantt_df = gantt_data)),
                        ## could also be html elements
                        tabPanel(title = "count",
                                 filter_plot_count(chosen_region, count_df = count_data)),
                        ## can also (probably should?) include a reactive input from the selected map region
                        tabPanel(title = "ou suis-je",
                                 plot_both_together(chosen_region, gantt_data, count_data))
  )
}
