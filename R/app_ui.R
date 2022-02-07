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
      dash_sidebar(
        dash_title(title = "Changements temporels",icon="nature-cute-028-tree"), 
        badge(
          text_badge = "
    Les données scientifiques recueillies par les citoyens nous donnent des informations précieuses sur la biodiversité québécoise."
    # <br><br>
    # Ce tableau de bord indique quels animaux ont été vus dans quelles régions du Québec.
    # <br><br>
    # Il indique également <b>quand</b> cette information a été recueillie. Les animaux et les plantes sont moins actifs en hiver, et plus actifs en été. En explorant ces données, n'oubliez pas : il en va de même pour les personnes qui les observent !
    #     "
        )#,
    # checkboxGroupInput("group",
    #             "Groupes d'organismes:",
    #             choices = c("mammals", "birds", "plants", "other"),
    #             selected = c("mammals", "birds", "plants", "other"))
    # numericInput("number_obs",
    #              "Pour combien des especes?", 
    #              value = 10)
      ), 
    dash_tabs(
      #maybe a little strange, but here we pass in the UI of a modal and the id that defines it.
      tab_map(title = "ATLAS", id = "reg_map", outputFunction = mod_map_select_ui),
      tab_map(title = "COLEO", id = 'bat_map', outputFunction = mod_map_select_ui))
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

