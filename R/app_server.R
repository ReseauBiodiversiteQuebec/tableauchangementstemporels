#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import mapselector
#' @noRd
app_server <- function( input, output, session ) {
  
  
  
  # regional phenology ------------------------------------------------------
  
  
  sample_data <- select_top_n_df_input(tableauchangementstemporels::data_with_region,
                                       .how_many_top = 7)
  
  ## replace with mapselector::subset_site_df
  acoustique_sites_sf <- rcoleo::download_sites_sf() |> mapselector::add_site_name_df() |> mapselector::subset_site_df(campaign_type = "acoustique")
  
  acoustique_obs <- rcoleo::get_gen('/species_arrival_departure', query=list('campaign_type'='acoustique')) 
  
  bats_pheno <- 
    acoustique_obs |>
    dplyr::mutate(
      ## Get dates
      min_date = lubridate::ymd(min_date),
      max_date = lubridate::ymd(max_date),
      min_yd = lubridate::yday(min_date),
      max_yd = lubridate::yday(max_date),
      min_d = lubridate::day(min_date),
      max_d = lubridate::day(max_date),
      ## Rename taxa name
      Taxon = factor(taxa_name),
      ## Compute presence time
      pres = (lubridate::interval(min_date, max_date) / lubridate::days(1))+1) |>
    ## Add display_name column
    dplyr::left_join(acoustique_sites_sf, by = "site_code") |>
    # Select required columns
    dplyr::select(Taxon, min_yd, max_yd, pres, min_d, max_d, min_date, max_date, site_code, display_name)
  
  chosen_site <- mapselector::mod_map_select_server("bat_map",
                                                    what_to_click = "marker",
                                                    fun = mapselector::plot_rcoleo_sites,
                                                    rcoleo_sites_sf = acoustique_sites_sf,
                                                    site_id_col = "display_name")
  
  # stat cards
  mod_fun_facts_server('fun_facts', acoustique_obs)
  
  # mapselector::mod_modal_make_server("modal_make_ui_1", 
  #                       region = chosen_site,
  #                       title_format_pattern = "La phénologie pour le site %s",
  #                       # here place all the tabs you want in your final modal! 
  #                       ## this can be a function which returns a reactive output (e.g. renderPlotly)
  #                       tabPanel(title = "Visualization",
  #                                plot_both_together(chosen_region, gantt_data, count_data))
  # )
  
  
  # bat sites ---------------------------------------------------------------
  
  ## Figure: Comparison of phenology between sites
  mod_pheno_sites_server('pheno_sites',acoustique_sites_sf, bats_pheno)
  
  clicked_site_name <- reactive({
    req(chosen_site())
    mapselector::make_site_name(got_clicked_site_val = chosen_site(), site_code_lookup)
  })
  
  # Phenology at one site as a lollipop plot
  mod_pheno_species_server('pheno_species', clicked_site_name, chosen_site, acoustique_sites_sf, bats_pheno)
  # hist type bat plot
  mod_hist_species_server('pheno_hist', clicked_site_name, chosen_site, acoustique_sites_sf, bats_pheno)
  # Module
  mapselector::mod_modal_make_server("modal_make_ui_bats", 
                        region = chosen_site,
                        title_format_pattern = "La phénologie pour le site %s",
                        # Pheno as lollipop plot
                        tabPanel(title = "Pédiode d'observation",
                                 mod_pheno_species_ui("pheno_species")),
                        # Pheno as an abundance histogram
                        tabPanel(title = "Répartition annuelle",
                                 mod_pheno_hist_ui("pheno_hist"))
  )
  
}
