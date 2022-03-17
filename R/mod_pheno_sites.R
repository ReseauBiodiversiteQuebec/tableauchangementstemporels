

#' Sunburst UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pheno_sites_ui <- function(id){
  ns <- NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(4,
                    selectInput(ns("taxa"),"Taxon",c("Eptesicus fuscus" = "Eptesicus fuscus",
                                                     "Eptesicus fuscus|Lasionycteris noctivagans" = "Eptesicus fuscus|Lasionycteris noctivagans",
                                                     "Eptesicus fuscus|Lasiurus borealis" = "Eptesicus fuscus|Lasiurus borealis",
                                                     "Lasionycteris noctivagans" = "Lasionycteris noctivagans",
                                                     "Lasiurus borealis" = "Lasiurus borealis",
                                                     "Lasiurus cinereus" = "Lasiurus cinereus",
                                                     "Lasiurus cinereus|Lasionycteris noctivagans" = "Lasiurus cinereus|Lasionycteris noctivagans",
                                                     "Myotis lucifugus" = "Myotis lucifugus",
                                                     "Myotis lucifugus|Lasiurus borealis" = "Myotis lucifugus|Lasiurus borealis",
                                                     "Myotis lucifugus|Myotis septentrionalis|Myotis leibii" = "Myotis lucifugus|Myotis septentrionalis|Myotis leibii",
                                                     "Myotis septentrionalis" = "Myotis septentrionalis",
                                                     "Perimyotis subflavus" = "Perimyotis subflavus",
                                                     "Perimyotis subflavus|Myotis lucifugus" = "Perimyotis subflavus|Myotis lucifugus",
                                                     "Chiroptera" = "Chiroptera"))
      ),
      shiny::column(4,
                    selectInput(ns("annee"),"Année",c("2018"="2018","2017"="2017","2016"="2016"))
      ),
      shiny::column(4,
                    selectInput(ns("ordre"),"Ordre de comparaison",c("Jours de présence"='jours_de_presence',"Latitude"='lat', "Première observation"="premiere_obs", "Type de site"="type_site"))
      )
    ),
    shiny::fluidRow(
      shinycssloaders::withSpinner(
        ggiraph::girafeOutput(ns("pheno_sites"),width='95%',height='50%'),
        proxy.height = '200px',color='#538887',type=7)
    )
  )
}

#' Sunburst Server Functions
#'
#' @param species_data sites x species count summary table
#'
mod_pheno_sites_server <- function(id, acoustique_sites_sf, bats_pheno){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$pheno_sites<-ggiraph::renderGirafe({ #plotly::renderPlotly({
      
      # Add latitude data to bats_pheno_site
      rcoleo_sites_bats <- acoustique_sites_sf |>
        dplyr::mutate(lat = sf::st_coordinates(geom.coordinates)[,"Y"]) |>
        as.data.frame() |>
        dplyr::select(site_code, cell.name, type, lat)
      
      bat_pheno_sites <- 
        bats_pheno |>
        ## Select one species
        dplyr::filter(Taxon == input$taxa) |>
        ## Select year
        dplyr::filter(lubridate::year(min_date) == input$annee) |>
        # Get site's latitude
        dplyr::left_join(rcoleo_sites_bats, by = "site_code")
      
      # Add labels to points
      m_en <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      m_fr <- c("Jan", "Fév", "Mars", "Avril", "Mai", "Juin", "Juil", "Août", "Sept","Oct", "Nov", "Déc")
      bat_pheno_sites <-
        bat_pheno_sites|>
        dplyr::mutate(date_lbl = lubridate::month(min_date, abbr = TRUE, label = TRUE),
                      date_lbl = sapply(lubridate::month(bat_pheno_sites$min_date, abbr = TRUE, label = TRUE), function(x) m_fr[m_en == x]),
                      date_lbl = paste(min_d, date_lbl),
                      max_date_lbl = lubridate::month(max_date, abbr = TRUE, label = TRUE),
                      max_date_lbl = sapply(lubridate::month(bat_pheno_sites$max_date, abbr = TRUE, label = TRUE), function(x) m_fr[m_en == x]),
                      max_date_lbl = paste(max_d, max_date_lbl))
      
      
      
      # Order species
      if(input$ordre == "lat") {
        bat_pheno_sites$display_name <- reorder(bat_pheno_sites$display_name, -bat_pheno_sites$lat)
      }else if(input$ordre == "type_site") {
        bat_pheno_sites$type <- factor(bat_pheno_sites$type, levels = unique(bat_pheno_sites$type))
        bat_pheno_sites <- bat_pheno_sites[order(bat_pheno_sites$type, -bat_pheno_sites$min_yd),]
        bat_pheno_sites$display_name <- reorder(bat_pheno_sites$display_name, seq_along(bat_pheno_sites$display_name))
      }else if(input$ordre == "premiere_obs") {
        bat_pheno_sites$display_name <- reorder(bat_pheno_sites$display_name, -bat_pheno_sites$min_yd)
      }else if(input$ordre == "jours_de_presence") {
        bat_pheno_sites$display_name <- reorder(bat_pheno_sites$display_name, bat_pheno_sites$pres)
      }
      
      # Dates
      mth_breaks <-
        bat_pheno_sites |> 
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
      bat_pheno_sites |>
        ggplot2::ggplot() +
        ### Add presence column
        ggplot2::geom_rect(
          ggplot2::aes(xmin=366, xmax=441, ymin=-Inf, ymax=Inf), fill="grey") +
        ggplot2::geom_text(
          ggplot2::aes(label=pres, y=display_name, x=403.5), fontface="bold", size=2) +
        ggplot2::geom_text(
          data=dplyr::filter(bat_pheno_sites, bat_pheno_sites$display_name==tail(levels(bat_pheno_sites$display_name), n=1)), 
          ggplot2::aes(x=403.5, y=display_name, label="Jours de\nprésence"),
          color="black", size=2, vjust=-0.5, fontface="bold") +
        ## Add horizontal grey lines
        ggplot2::geom_segment(
          ggplot2::aes(y=display_name, yend=display_name, x=min_yd, xend=max_yd), color="grey", size=1) +
        ## Add first observation points
        ggiraph::geom_point_interactive(ggplot2::aes(y=display_name, x=min_yd, tooltip = date_lbl), size=2, colour = first) +
        ## Add last observation points
        ggiraph::geom_point_interactive(ggplot2::aes(y=display_name, x=max_yd, tooltip = max_date_lbl), size=2, colour = last) +
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.06, 0.15))) +
        ## Labels
        ggplot2::geom_text(
          data=dplyr::filter(bat_pheno_sites, bat_pheno_sites$display_name==tail(levels(bat_pheno_sites$display_name), n=1)),
          ggplot2::aes(x=max_yd, y=display_name, label="Dernière\nobservation"),
          color=last, size=2, vjust=-0.5, fontface="bold") +
        ggplot2::geom_text(
          data=dplyr::filter(bat_pheno_sites, bat_pheno_sites$display_name==tail(levels(bat_pheno_sites$display_name), n=1)),
          ggplot2::aes(x=min_yd, y=display_name, label="Première\nobservation"),
          color=first, size=2, vjust=-0.5, fontface="bold") +
        ## Add labels to values
        ggplot2::geom_text(
          ggplot2::aes(x=min_yd, y=display_name, label=date_lbl),
          color=first, size=1.75, vjust=2.5) +
        # ggplot2::geom_text(
        #   ggplot2::aes(x=max_wk, y=display_name, label=max_d),
        #   color=last, size=2.75, vjust=2.5) +
        ggplot2::scale_x_continuous(breaks = mth_breaks$day,
                                    limits = c(1,441),
                                    labels = mth_breaks$mois,
                                    minor_breaks = c(1:366)[!c(1:366) %in% mth_breaks$day],
                                    expand = c(0, 0)) +
        #ggplot2::scale_y_discrete(expand=c(0.2,0)) +
        ggplot2::theme_bw(base_family="Lato", base_size = 8) +
        ggplot2::theme(
          #plot.margin = ggplot2::margin(3,0,1,0, "cm"),
          axis.text.x = ggplot2::element_text(margin=ggplot2::margin(10,0,0,0), angle = 30, vjust = 1, hjust = 1),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(colour = "lightgrey", size = 0.1),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.border=ggplot2::element_blank(),
          axis.ticks=ggplot2::element_blank(),
          axis.title.x=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_blank()
        ) 
        # Create giraph
        ggiraph::girafe(ggobj = ggplot,
                        #width_svg = 16, 
                        height_svg = 3.4,
                        options = list(ggiraph::opts_sizing(rescale = TRUE, width = 1)))
      }) 
  })
}

## To be copied in the UI
# mod_map_richness_campaigns_ui("map_richness_campaigns_1")

## To be copied in the server
# mod_map_richness_campaigns_server("map_richness_campaigns_1")
