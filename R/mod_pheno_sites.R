

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
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("pheno_sites"),width='95%',height='95%'),proxy.height = '100%',color='#538887',type=7)
  )
}

#' Sunburst Server Functions
#'
#' @param species_data sites x species count summary table
#'
mod_pheno_sites_server <- function(id, rcoleo_sites_sf, bat_pheno_sites){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #put in dropdown
    taxa <- "Eptesicus fuscus"
    annee <- "2016"
    ordre <- "lat"
    
    output$pheno_sites<-plotly::renderPlotly({
      bat_pheno_sites <- 
        bat_pheno_sites |>
        dplyr::mutate(min_date = lubridate::ymd(min_date),
                      max_date = lubridate::ymd(max_date),
                      min_yd = lubridate::yday(min_date),
                      max_yd = lubridate::yday(max_date),
                      min_d = lubridate::day(min_date),
                      max_d = lubridate::day(max_date),
                      Taxon = factor(taxa_name),
                      ## Compute presence time
                      pres = (lubridate::interval(min_date, max_date) / lubridate::days(1))+1) |>
        dplyr::select(Taxon, min_yd, max_yd, pres, min_d, max_d, min_date, site_code) |>
        ## Select one site
        dplyr::filter(Taxon == taxa) |>
        ## Select year
        dplyr::filter(lubridate::year(min_date) == annee)
      
      # Add latitude data to bats_pheno_site
      rcoleo_sites_bats <- rcoleo_sites_sf |>
        dplyr::mutate(lat = sf::st_coordinates(geom.coordinates)[,"Y"]) |>
        as.data.frame() |>
        dplyr::select(site_code, display_name, cell.name, type, lat)
      
      # Order sites according to latitude
      bat_pheno_sites <-
        bat_pheno_sites |>
        dplyr::left_join(rcoleo_sites_bats, by = "site_code")
      
      
      # Order species
      if(ordre == "lat") {
        bat_pheno_sites$display_name <- reorder(bat_pheno_sites$display_name, -bat_pheno_sites$lat)
      }else if(ordre == "type_site") {
        bat_pheno_sites$type <- factor(bat_pheno_sites$type, levels = unique(bat_pheno_sites$type))
        bat_pheno_sites <- bat_pheno_sites[order(bat_pheno_sites$type, -bat_pheno_sites$min_yd),]
        bat_pheno_sites$display_name <- reorder(bat_pheno_sites$display_name, seq_along(bat_pheno_sites$display_name))
      }else if(ordre == "premiere_obs") {
        bat_pheno_sites$display_name <- reorder(bat_pheno_sites$display_name, -bat_pheno_sites$min_yd)
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
        dplyr::mutate(mois = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Aout", "Septembre","Octobre", "Novembre", "Décembre"))
      
      # colors
      first <- rgb(0.18,0.545,0.341,0.8)
      last <- rgb(0.7,0.2,0.1,0.8)
      
      # Plot
      ## Inspired by https://towardsdatascience.com/create-dumbbell-plots-to-visualize-group-differences-in-r-3536b7d0a19a
      bat_pheno_sites |>
        ggplot2::ggplot() +
        ## Add horizontal grey lines
        ggplot2::geom_segment(
          ggplot2::aes(y=display_name, yend=display_name, x=min_yd, xend=max_yd), color="grey", size=1.5) +
        ## Add first observation points
        ggplot2::geom_point(ggplot2::aes(y=display_name, x=min_yd), size=3, colour = first) +
        ## Add last observation points
        ggplot2::geom_point(ggplot2::aes(y=display_name, x=max_yd), size=3, colour = last) +
        ggplot2::scale_y_discrete(expand = c(0,1.2)) +
        ## Labels
        ggplot2::geom_text(
          data=dplyr::filter(bat_pheno_sites, bat_pheno_sites$display_name==tail(levels(bat_pheno_sites$display_name), n=1)),
          ggplot2::aes(x=max_yd, y=display_name, label="Dernière\nobservation"),
          color=last, size=3, vjust=-0.5, fontface="bold") +
        ggplot2::geom_text(
          data=dplyr::filter(bat_pheno_sites, bat_pheno_sites$display_name==tail(levels(bat_pheno_sites$display_name), n=1)),
          ggplot2::aes(x=min_yd, y=display_name, label="Première\nobservation"),
          color=first, size=3, vjust=-0.5, fontface="bold") +
        ### Add presence column
        ggplot2::geom_rect(
          ggplot2::aes(xmin=275, xmax=350, ymin=-Inf, ymax=Inf), fill="grey") +
        ggplot2::geom_text(
          ggplot2::aes(label=pres, y=display_name, x=312.5), fontface="bold", size=3) +
        ggplot2::geom_text(
          data=dplyr::filter(bat_pheno_sites, bat_pheno_sites$display_name==tail(levels(bat_pheno_sites$display_name), n=1)), 
          ggplot2::aes(x=312.5, y=display_name, label="Jours de présence"),
          color="black", size=3.1, vjust=-2, fontface="bold") +
        ## Add labels to values
        # ggplot2::geom_text(
        #   ggplot2::aes(x=min_wk, y=display_name, label=min_d),
        #   color=first, size=2.75, vjust=2.5) +
        # ggplot2::geom_text(
        #   ggplot2::aes(x=max_wk, y=display_name, label=max_d),
        #   color=last, size=2.75, vjust=2.5) +
        ggplot2::scale_x_continuous(breaks = mth_breaks$day,
                                    limits = c(1,366),
                                    labels = mth_breaks$mois,
                                    minor_breaks = c(1:366)[!c(1:366) %in% mth_breaks$day],
                                    expand = c(0, 0)) +
        #ggplot2::scale_y_discrete(expand=c(0.2,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(margin=ggplot2::margin(10,0,0,0), angle = 30, vjust = 1, hjust = 1),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(colour = "lightgrey", size = 0.1),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.border=ggplot2::element_blank(),
          axis.ticks=ggplot2::element_blank(),
          axis.title.x=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_blank()
        )    })
  })
}

## To be copied in the UI
# mod_map_richness_campaigns_ui("map_richness_campaigns_1")

## To be copied in the server
# mod_map_richness_campaigns_server("map_richness_campaigns_1")
