
# library(shiny)
testapp_bat_maps <- function(){
  ui <- fluidPage(
    mapselector::mod_map_select_ui("bat_map"),
    tableOutput("sel")
  )
  
  server <-  function(input, output, session) {
    
    rcoleo_sites_sf <- rcoleo::download_sites_sf()
    # filter only for those sites with bat data
    acous <- rcoleo:::query_gen("campaigns", list(type = "acoustique"))
    
    rcoleo_sites_bats <- dplyr::semi_join(rcoleo_sites_sf, acous, by = c("site_code" = "site.site_code"))
    
    
    selsite <- mapselector::mod_map_select_server("bat_map",
                          what_to_click = "marker",
                          fun = mapselector::plot_rcoleo_sites,
                          rcoleo_sites_sf = rcoleo_sites_bats)
    

    ff <- reactive({mapselector:::get_subset_site(site = rcoleo_sites_bats,
                              site_code_sel = selsite())})
    
    output$sel <- renderTable(head(ff()))
  }
  shinyApp(ui, server)
}

testapp_bat_maps()
