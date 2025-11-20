# ecocomDP use demo

# clean out workspace



rm(list = ls()) # OPTIONAL - clear out your environment
gc()            # Uncomment these lines if desired

# load packages
library(tidyverse)
library(neonUtilities)
library(ecocomDP)
install.packages('vegan')


search_result <- ecocomDP::search_data(text = "periphyt|algae")

View(search_result)

dataset_1 <- read_data("edi.193.5")


dataset_2 <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site = c("COMO","HOPB"), # NEON sites
  startdate = "2017-01", # start year-month
  enddate = "2019-12", # end year-month
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

dataset_1$metadata
