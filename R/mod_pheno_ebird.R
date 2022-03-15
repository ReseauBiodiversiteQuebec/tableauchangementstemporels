#' Sunburst UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pheno_hist_ui <- function(id){
  ns <- NS(id)
  shiny::fluidPage(
    # shiny::fluidRow(
    #   shiny::selectInput(
    #     ns("ordre"),
    #     "Ordre de comparaison",
    #     c("Jours de présence"='jours_de_presence', "Première observation"="premiere_obs"))
    # ),
    shiny::fluidRow(
      shiny::column(3,
                    shiny::htmlOutput(ns("tbl_data"))
                    #shiny::htmlOutput(ns("photo"))
      ),
      shiny::column(9,
                    ggiraph::girafeOutput(ns("pheno_hist"))
      )
    )
  )
}


#' Sunburst Server Functions
#'
#' @param species_data sites x species count summary table
#'
mod_hist_species_server <- function(id, site_name, site, acoustique_sites_sf, bats_pheno){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    #-------------------------------------------------------------------------------
    # Fun facts cards
    #-------------------------------------------------------------------------------
    output$tbl_data <- renderUI({
      
      # Compute metrics
      ## Get site_code
      site_code_sel <- bats_pheno$site_code[bats_pheno$display_name == site()]
      ## Get all the coleo sites
      obs_site <- acoustique_sites_sf |>
        mapselector::get_subset_site(site_code_sel = site_code_sel)
      
      ## jour max observations
      day_max <- obs_site$date_obs |>
        lubridate::ymd() |>
        lubridate::yday() |>
        table() |>
        which.max() |>
        names() |>
        as.numeric() |>
        as.Date(origin = "2016-01-01") 
      month <- lubridate::month(day_max, label=TRUE, abbr = TRUE)
      month_df <- data.frame(month = c("Jan", "Feb", "Mar" , "Apr" , "May" , "Jun" , "Jul" , "Aug" , "Sep" , "Oct" , "Nov" , "Dec"),
                             mois = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Aout", "Septembre","Octobre", "Novembre", "Décembre"))
      mois <- month_df[month_df$month==month, "mois"]
      jour <- lubridate::mday(day_max)
      jour_max <- paste(jour, mois)
      
      ## Heure max observations
      hour_max <- 
        sapply(strsplit(obs_site$time_obs,split=":"), function(x) x[1]) |>
        table() |>
        which.max() |>
        names()
      hour_max <- paste0(hour_max, ":00")
      
      # Cards
      div(
        fact_card(jour_max,'Journée la plus active','calendar-day','main-1'),
        fact_card(hour_max,'Heure d\'activité maximale','clock','main-2')
      )
    })
    
    #-------------------------------------------------------------------------------
    # Build plot
    #-------------------------------------------------------------------------------
    output$pheno_hist <- ggiraph::renderGirafe({
      ## Get site_code
      site_code_sel <- bats_pheno$site_code[bats_pheno$display_name == site()]
      ## Subset observations for site of interest
      obs_site <- acoustique_sites_sf |>
        mapselector::get_subset_site(site_code_sel = site_code_sel)
      bats_pheno_site <- bats_pheno |> dplyr::filter(display_name == site())
      # Prep data
      weekly_one_bat <- get_weekly_one_bat(obs_site, bats_pheno_site, ordre = input$ordre)
      # Create giraph
      ggplot <- ggObsTable(weekly_one_bat, "n_std", "Taxon", "mth", "wk",
                           col.fill = rgb(224,182,88, maxColorValue = 255),
                           bg.fill = rgb(123,181,178, maxColorValue = 255))
      # ggplot to ggigraph object
      ggiraph::girafe(ggobj = ggplot,
                      width_svg = 10, 
                      #height_svg = 9,
                      options = list(ggiraph::opts_sizing(rescale = TRUE, width = 1)))
    })
  })
}

## To be copied in the UI
# mod_map_richness_campaigns_ui("map_richness_campaigns_1")

## To be copied in the server
# mod_map_richness_campaigns_server("map_richness_campaigns_1")
