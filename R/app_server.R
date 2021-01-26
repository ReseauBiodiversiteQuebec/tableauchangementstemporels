#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Your application server logic 
  output$map <- leaflet::renderLeaflet(make_leaflet_map())
  
  sample_data <- format_sample_data()
  
  # convenient reactive value for the clicked region
  chosen_region <- reactive({input$map_shape_click$id})
  
  mod_modal_make_server("modal_make_ui_1", 
                        region = chosen_region,
                        # here place all the tabs you want in your final modal! 
                        ## this can be a function which returns a reactive output (e.g. renderPlotly)
                        tabPanel(title = "Visualization",
                                 # see mapselector::ipso_zoo for an example
                                 filter_plot_gantt(chosen_region(), gantt_df = sample_data)
                        ),
                        ## could also be html elements
                        tabPanel(title = "C'est un tab",
                                 div("Bien sur c'est un tab")),
                        ## can also (probably should?) include a reactive input from the selected map region
                        tabPanel(title = "ou suis-je",
                                 renderText({paste("tu est sur", chosen_region())})
                        )
  )
}
