
make_leaflet_batmap <- function(){
  leaflet::leaflet(tableauchangementstemporels::sites_to_plot,
                   options = leaflet::leafletOptions(minZoom = 6)) %>%
    leaflet::addTiles() %>% # Affichage du fond de carte
    leaflet::addCircleMarkers(lng = ~long_site, # Positionnement des sites avec les coordonnées long/lat
                     lat = ~lat_site,
                     radius = 8, # taille du cercle
                     # popup = obs_an()$popup_info, # Ajout de fenêtres pop-up
                     color = ~col,
                     label = ~Nom.de.la.cellule,
                     layerId = ~Numero.de.reference.du.site) # Nom.de.la.cellule?
}