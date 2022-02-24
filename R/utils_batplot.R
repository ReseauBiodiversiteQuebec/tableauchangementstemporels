
# simple demo function for testing the icon plotting
testapp_bat_maps <- function(){
  ui <- fluidPage(
    mapselector::fa_dependency(),
    mapselector::mod_map_select_ui("bat_map"),
    verbatimTextOutput("sel")
  )
  
  server <-  function(input, output, session) {
    
    
    all_sites <- rcoleo::download_sites_sf()
    
    target_sites <- mapselector::subset_site_df(downloaded_sites = all_sites,
                                                campaign_type = "acoustique")
    
    
    selsite <- mapselector::mod_map_select_server("bat_map",
                          what_to_click = "marker",
                          fun = mapselector::plot_rcoleo_sites,
                          rcoleo_sites_sf = target_sites)
    
    ff <- reactive({mapselector::get_subset_site(site = target_sites,
                                                 site_code_sel = selsite())})
    
    output$sel <- renderPrint(str(ff()))
  }
  shinyApp(ui, server)
}

# testapp_bat_maps()



#' count species per day
#'
#' @param selected_site_result the output of mapselector::get_subset_site
#'
#' @return data frame with days as data
#' @importFrom magrittr %>%
#' @export
count_day_species <- function(selected_site_result){
  
  one_site_bats <- selected_site_result
  
  count_day_spp <- one_site_bats %>% 
    dplyr::select(taxa = obs_species.taxa_name, date_obs) %>% 
    dplyr::count(taxa, date_obs) %>% 
    dplyr::mutate(date_obs = lubridate::ymd(date_obs))
  
  return(count_day_spp)
}



#' Count the weeks and complete them
#'
#' @param count_day_spp_df output of count_day_species
#'
#' @return data frame with completed weeks
#' @importFrom magrittr %>%
#' @export
complete_weeks <- function(count_day_spp_df){
  count_complete_weeks <- count_day_spp_df %>% 
    dplyr::mutate(wk = lubridate::week(date_obs)) %>% 
    dplyr::group_by(taxa, wk) %>% 
    dplyr::tally(.) %>% 
    dplyr::ungroup(.) %>% 
    tidyr::complete(taxa, wk = 16:41,
             fill = list(n = 0))
  
  return(count_complete_weeks)
}



#' take a bat df and make a bat plot
#'
#' @param df data frame as returned by mapselector::get_subset_site
#'
#' @return
#' @importFrom magrittr %>%
#' @export
df_to_plot <- function(df){
  complete_bats <- df %>% 
    count_day_species(.) %>% 
    complete_weeks(.)
  
  
  complete_bats %>% 
    plot_some_bats(.)
}


df_to_dotplot <- function(df){
  df %>% 
    dplyr::mutate(date_fmt = lubridate::ymd(date_obs),
           wk = lubridate::week(date_fmt)) %>% 
    dplyr::select(wk, taxa = obs_species.taxa_name) %>% 
    dplyr::distinct(.) %>% 
    ggplot2::ggplot(ggplot2::aes(x = wk, fill = taxa)) + 
    ggplot2::geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all") + 
    ggplot2::scale_fill_brewer(palette = "Dark2")
}


#' D'un jeu de données, retourne une figure de la phénologie par espèces
#'
#' @param bats_pheno_site jeu de données contenant les taxons et leur phénologie pour un site
#'
#' @return
#' 
#' @export
plot_pheno_par_sp <- function(bats_pheno_site){
  ## Inspired by https://towardsdatascience.com/create-dumbbell-plots-to-visualize-group-differences-in-r-3536b7d0a19a
  bats_pheno_site |>
    ggplot2::ggplot() +
    ## Add horizontal grey lines
    ggplot2::geom_segment(
      ggplot2::aes(y=Taxon, yend=Taxon, x=0, xend=366),
      color="#b2b2b2", size=0.15) +
    ggplot2::scale_y_discrete(expand = c(0,1.2)) +
    ## Add horizontal grey lines
    ggplot2::geom_segment(
      ggplot2::aes(y=Taxon, yend=Taxon, x=min_yd, xend=max_yd), color="grey", size=1.5) +
    ## Add first observation points
    ggplot2::geom_point(ggplot2::aes(y=Taxon, x=min_yd), size=3, colour = first) +
    ## Add last observation points
    ggplot2::geom_point(ggplot2::aes(y=Taxon, x=max_yd), size=3, colour = last) +
    ## Labels
    ggplot2::geom_text(
      data=dplyr::filter(bats_pheno_site, bats_pheno_site$Taxon==tail(levels(bats_pheno_site$Taxon), n=1)),
      ggplot2::aes(x=max_yd, y=Taxon, label="Dernière\nobservation"),
      color=last, size=3, vjust=-0.5, fontface="bold") +
    ggplot2::geom_text(
      data=dplyr::filter(bats_pheno_site, bats_pheno_site$Taxon==tail(levels(bats_pheno_site$Taxon), n=1)),
      ggplot2::aes(x=min_yd, y=Taxon, label="Première\nobservation"),
      color=first, size=3, vjust=-0.5, fontface="bold") +
    ## Add presence column
    ggplot2::geom_rect(
      ggplot2::aes(xmin=275, xmax=350, ymin=-Inf, ymax=Inf), fill="grey") +
    ggplot2::geom_text(
      ggplot2::aes(label=pres, y=Taxon, x=312.5), fontface="bold", size=3) +
    ggplot2::geom_text(
      data=dplyr::filter(bats_pheno_site, bats_pheno_site$Taxon==tail(levels(bats_pheno_site$Taxon), n=1)),
      ggplot2::aes(x=312.5, y=Taxon, label="Jours de présence"),
      color="black", size=3.1, vjust=-2, fontface="bold") +
    ## Add labels to values
    # ggplot2::geom_text(
    #   ggplot2::aes(x=min_wk, y=Taxon, label=min_d),
    #   color=first, size=2.75, vjust=2.5) +
    # ggplot2::geom_text(
    #   ggplot2::aes(x=max_wk, y=Taxon, label=max_d),
    #   color=last, size=2.75, vjust=2.5) +
    # Add presence column
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
      #axis.text.x=ggplot2::element_blank()
    )
}



#' D'un jeu de données, retourne une figure de la phénologie d'une espèce par site
#'
#' @param bat_pheno_sites jeu de données contenant la phénologie d'un taxon pour tous les sites
#'
#' @return
#' 
#' @export
plot_pheno_par_sites <- function(bat_pheno_sites){
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
    )
}


#' D'un jeu de données, retourne une figure de la phénologie d'une espèce pour un site et la compare à un descripteur
#'
#' @param weekly_one_bat jeu de données contenant la phénologie d'un taxon pour un site
#'
#' @return
#' 
#' @export

plot_pheno_et_descr <- function(weekly_one_bat){
  weekly_one_bat |>
    dplyr::select(wk, Taxon) |>
    #Plot
    ggplot2::ggplot(ggplot2::aes(x = wk)) + 
    ggplot2::geom_dotplot(ggplot2::aes(fill = Taxon), colour = "NA", stackgroups = TRUE, binwidth = 1, binpositions = "all", stackratio = 1.05, dotsize = 1) + 
    ggplot2::geom_line(data=dplyr::select(one_site_clim, wk, secVar_scaled),
                       ggplot2::aes(x=wk, y=secVar_scaled), color = "blue") +
    ggplot2::xlab("") +
    ggplot2::scale_x_continuous(breaks = mth_breaks$wk,
                                limits = c(1,53),
                                labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Aout", "Septembre","Octobre", "Novembre", "Décembre"),
                                minor_breaks = c(1:53)[!c(1:53) %in% mth_breaks],
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), 
                                # Features of the first axis
                                name = "Abondance (nombre d'observations)",
                                # Remove y axis ticks and labels
                                breaks = 0,
                                labels = "",
                                # Add a second axis and specify its features
                                sec.axis = ggplot2::sec_axis(trans = ~.*1, breaks = seq(0,2,0.5),
                                                             labels = round(seq(min(one_site_clim$secVar, na.rm=TRUE), max(one_site_clim$secVar, na.rm=TRUE), length.out = 5)), 
                                                             name="Température (°C)")
    ) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    # inspired by ggthemes theme_hc()
    ggplot2::theme(rect = ggplot2::element_rect(linetype = 0, colour = NA),
                   #plot.margin = ggplot2::unit(c(1,1,1,1),"cm"),
                   #text = ggplot2::element_text(size = 16, family = 'sans'),
                   title = ggplot2::element_text(hjust = 0.5),
                   axis.title.x = ggplot2::element_text(vjust = 0.3),
                   axis.text.x = ggplot2::element_text(margin=ggplot2::margin(10,0,0,0), angle = 30, vjust = 1, hjust = 1),
                   axis.ticks.x = ggplot2::element_blank(),
                   #axis.title.y = element_text(hjust = 0.5),
                   #axis.text.y = ggplot2::element_blank(),
                   # axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "#D8D8D8", size = 1),
                   panel.grid.minor.x = ggplot2::element_line(colour = "#D8D8D8", size = 0.1),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none",
                   legend.key = ggplot2::element_rect(fill = "#FFFFFF00"))
}