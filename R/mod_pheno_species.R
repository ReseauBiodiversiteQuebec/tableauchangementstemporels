#' Sunburst UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pheno_species_ui <- function(id){
  ns <- NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::selectInput(
        ns("ordre"),
        "Ordre de comparaison",
        c("Jours de présence"='jours_de_presence', "Première observation"="premiere_obs"))
    ),
    shiny::fluidRow(
      shiny::column(3,
                    shiny::htmlOutput(ns("tbl_data"))
                    #shiny::htmlOutput(ns("photo"))
      ),
      shiny::column(9,
                    ggiraph::girafeOutput(ns("pheno_species"))
      )
    )
  )
}

#' Sunburst Server Functions
#'
#' @param species_data sites x species count summary table
#'
mod_pheno_species_server <- function(id, site_name, site, rcoleo_sites_sf, bats_pheno){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Fun facts -----------------------------------------------------------
    output$tbl_data <- renderUI({
      
      # Compute metrics
      ## Get site_code
      site_code_sel <- bats_pheno$site_code[bats_pheno$display_name == site()] |> unique()
      ## Get all the coleo sites
      obs_site <- rcoleo::download_sites_sf(token = rcoleo:::bearer()) |>
        mapselector::subset_site_df(campaign_type = "acoustique") |>
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
    
    # Figure --------------------------------------------------------------
    output$pheno_species <- ggiraph::renderGirafe({
      
      ordre = input$ordre

      bats_pheno_site <- bats_pheno |>
        ## Select one site
        dplyr::filter(display_name == site())
      
      # Add labels to points
      m_en <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      m_fr <- c("Jan", "Fév", "Mars", "Avril", "Mai", "Juin", "Juil", "Août", "Sept","Oct", "Nov", "Déc")
      bats_pheno_site <-
        bats_pheno_site|>
        dplyr::mutate(date_lbl = lubridate::month(min_date, abbr = TRUE, label = TRUE),
                      date_lbl = sapply(lubridate::month(bats_pheno_site$min_date, abbr = TRUE, label = TRUE), function(x) m_fr[m_en == x]),
                      date_lbl = paste(min_d, date_lbl),
                      max_date_lbl = lubridate::month(max_date, abbr = TRUE, label = TRUE),
                      max_date_lbl = sapply(lubridate::month(bats_pheno_site$max_date, abbr = TRUE, label = TRUE), function(x) m_fr[m_en == x]),
                      max_date_lbl = paste(max_d, max_date_lbl))
      
      # Order species
      if(ordre == "premiere_obs") {
        bats_pheno_site$Taxon <- reorder(bats_pheno_site$Taxon, -bats_pheno_site$min_yd)
      } else if(ordre == "jours_de_presence") {
        bats_pheno_site$Taxon <- reorder(bats_pheno_site$Taxon, bats_pheno_site$pres)
      }
      
      bats_pheno_site$Taxon <- droplevels(bats_pheno_site$Taxon)
      
      # Dates
      mth_breaks <-
        bats_pheno_site |> 
        # Complete weeks and months to have a full year represented
        dplyr::mutate(date = lubridate::ymd(min_date)) |>
        dplyr::select(date) |>
        tidyr::complete(date = lubridate::ymd(seq.Date(lubridate::dmy("01-01-2000"),
                                                       lubridate::dmy("31-12-2000"), by="week"))) |>
        dplyr::mutate(day = lubridate::yday(date),
                      wk = lubridate::week(date),
                      mth = lubridate::month(date,label=TRUE,abbr=TRUE)) |>
        dplyr::distinct() |>
        dplyr::group_by(mth) |>
        dplyr::summarise(wk = min(wk), day = min(day)) |>
        dplyr::mutate(mois = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre","Octobre", "Novembre", "Décembre"))
      
      # colors
      first <- rgb(0.18,0.545,0.341,0.8)
      last <- rgb(0.7,0.2,0.1,0.8)
      
      # Plot
      ggplot <-
      ## Inspired by https://towardsdatascience.com/create-dumbbell-plots-to-visualize-group-differences-in-r-3536b7d0a19a
      bats_pheno_site |>
        ggplot2::ggplot() +
        ## Add presence column
        ggplot2::geom_rect(
          ggplot2::aes(xmin=366, xmax=441, ymin=-Inf, ymax=Inf), fill="grey") +
        ggplot2::geom_text(
          ggplot2::aes(label=pres, y=Taxon, x=403.5), fontface="bold", size=3) +
        ggplot2::geom_text(
          data=dplyr::filter(bats_pheno_site, bats_pheno_site$Taxon==tail(levels(bats_pheno_site$Taxon), n=1)),
          ggplot2::aes(x=403.5, y=Taxon, label="Jours de\nprésence"),
          color="black", size=3.1, vjust=-0.5, fontface="bold") +
        ## Add horizontal grey lines
        ggplot2::geom_segment(
          ggplot2::aes(y=Taxon, yend=Taxon, x=0, xend=366),
          color="#b2b2b2", size=0.15) +
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.06, 0.15))) +
        ## Add horizontal grey lines
        ggplot2::geom_segment(
          ggplot2::aes(y=Taxon, yend=Taxon, x=min_yd, xend=max_yd), color="grey", size=1.5) +
        ## Add first observation points
        #ggplot2::geom_point(ggplot2::aes(y=Taxon, x=min_yd), size=3, colour = first) +
        ggiraph::geom_point_interactive(ggplot2::aes(y=Taxon, x=min_yd, tooltip=date_lbl), size=3, colour = first) +
        ## Add last observation points
        ggiraph::geom_point_interactive(ggplot2::aes(y=Taxon, x=max_yd, tooltip=max_date_lbl), size=3, colour = last) +
        ## Labels
        ggplot2::geom_text(
          data=dplyr::filter(bats_pheno_site, bats_pheno_site$Taxon==tail(levels(bats_pheno_site$Taxon), n=1)),
          ggplot2::aes(x=max_yd, y=Taxon, label="Dernière\nobservation"),
          color=last, size=3, vjust=-0.5, fontface="bold") +
        ggplot2::geom_text(
          data=dplyr::filter(bats_pheno_site, bats_pheno_site$Taxon==tail(levels(bats_pheno_site$Taxon), n=1)),
          ggplot2::aes(x=min_yd, y=Taxon, label="Première\nobservation"),
          color=first, size=3, vjust=-0.5, fontface="bold") +
        ## Add labels to values
        ggplot2::geom_text(
          ggplot2::aes(x=min_yd, y=Taxon, label=date_lbl),
#                       label=ifelse(ordre == "premiere_obs", min_d, "")),
          color=first, size=2.75, vjust=2.5) +
        # ggplot2::geom_text(
        #   ggplot2::aes(x=max_wk, y=Taxon, label=max_d),
        #   color=last, size=2.75, vjust=2.5) +
        # Add presence column
        ggplot2::scale_x_continuous(breaks = mth_breaks$day,
                                    limits = c(1,441),
                                    labels = mth_breaks$mois,
                                    minor_breaks = c(1:366)[!c(1:366) %in% mth_breaks$day],
                                    expand = c(0, 0)) +
        #ggplot2::scale_y_discrete(expand=c(0.2,0)) +
        ggplot2::theme_bw(base_family="Lato", base_size = 16) +
        ggplot2::theme(
          #text = ggplot2::element_text(size = 16),
          axis.text.x = ggplot2::element_text(margin=ggplot2::margin(10,0,0,0), angle = 30, vjust = 1, hjust = 1),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(colour = "lightgrey", size = 0.1),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.border=ggplot2::element_blank(),
          axis.ticks=ggplot2::element_blank(),
          axis.title.x=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_blank()
          #axis.text.x=ggplot2::element_blank()
        )
      # Create giraph
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
