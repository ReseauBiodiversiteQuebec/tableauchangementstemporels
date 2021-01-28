#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny 
#' @import mapselector
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tableau_de_bord(
      dash_title(title = "Changements temporels"), 
      dash_sidebar(
        badge(
          text_badge = htmlOutput("what_does_the_badge_say")
        ),
        checkboxGroupInput("group",
                    "Groupes d'organismes:",
                    choices = c("mammals", "birds", "plants", "other"),
                    selected = c("mammals", "birds", "plants", "other")),
        numericInput("number_obs",
                     "Pour combien des especes?", 
                     value = 10)
      ), 
      dash_tabs(tab_map(title = "ATLAS"),
                tab_map(title = "COLEO", id = 'batmap'))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Changements temporels'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

