# obtain site codes and colours for the bat dataset
# general and could move to mapselector

sites_to_plot <- readr::read_csv("../../MFFP_chiroptera/obs_an.csv")



usethis::use_data(sites_to_plot, overwrite = TRUE)



common_bats_wk <- readr::read_csv("../../MFFP_chiroptera/common_bats_wk.csv")

usethis::use_data(common_bats_wk, overwrite = TRUE)
