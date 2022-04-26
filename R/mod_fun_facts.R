#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fun_facts_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::htmlOutput(ns("facts"))
  )
}

#'
#' @param acoustique_obs df of Taxon & min and max observation dates
#'
mod_fun_facts_server <- function(id, acoustique_obs, obs_sum){
  moduleServer( id, function(input, output, session){
    output$facts<-renderUI({div(
      fact_card(paste(lubridate::year(min(acoustique_obs$min_date)),"à",lubridate::year(max(acoustique_obs$max_date))),'Période couverte','calendar-alt','main-1'),
      fact_card(obs_sum,'Observations auditives','microphone-alt','main-2'),
      fact_card(length(unique(acoustique_obs$taxa_name)),'Taxons identifiés','search','main-3')
      )
    })
  })
}