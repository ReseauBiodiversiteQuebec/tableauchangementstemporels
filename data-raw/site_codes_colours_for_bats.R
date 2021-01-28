# obtain site codes and colours for the bat dataset
# general and could move to mapselector

sites_to_plot <- readr::read_csv("../../MFFP_chiroptera/obs_an.csv")



usethis::use_data(sites_to_plot, overwrite = TRUE)

