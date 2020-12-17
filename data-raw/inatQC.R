## code to prepare `inatQC` dataset goes here

# read from elsewhere, perhaps later best to get from API directly
inatQC <- readr::read_csv("../../BDQC-shiny-inat-time-series/iNatQC/iNatQC.csv")

library(tidyverse)

glimpse(inatQC)

inatQC <- inatQC %>% 
  select(-X1) 

usethis::use_data(inatQC, overwrite = TRUE)
