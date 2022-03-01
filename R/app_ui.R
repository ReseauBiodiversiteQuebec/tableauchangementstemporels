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
    shinycssloaders::withSpinner(
      mapselector::tableau_de_bord(
        mapselector::dash_sidebar(
          mapselector::dash_title(title = "Phénologie des chauves-souris",icon="fianimals animals-007-bat"), 
          mapselector::badge(text_badge = "Cette interface permet d'explorer la phénologie des chauves-souris et ses changements temporels à l'aide des points d'écoute effectués dans le cadre du réseau de suivi de la biodiversité du Québec."),
          mod_fun_facts_ui('fun_facts')
        ), 
        mapselector::dash_tabs(
          #maybe a little strange, but here we pass in the UI of a modal and the id that defines it.
          mapselector::tab_map(title = "COLEO", id = 'bat_map', outputFunction = mapselector::mod_map_select_ui),
          mapselector::tab_gen(title = "Comparaison entre sites",
                               outputFunction = mod_pheno_sites_ui,
                               id = "pheno_sites")
        )
      ),
      proxy.height = '200px',color='#538887',type=7)
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

