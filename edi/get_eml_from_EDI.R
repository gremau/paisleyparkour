library(tidyverse)
library(EDIutils)
library(xml2)

outpath <- "~/Desktop/EDI_eml/"

all <- search_data_packages(query = 'q=*&fl=packageid&fq=-scope:(ecotrends+lter-landsat+lter-landsat-ledaps)')

# Loop through and fetch eml, write to a path
# Would be good to remove fully-embargoed datasets first...
for (id in all[9144:nrow(all),]){
  print(id)
  eml <- read_metadata(id)
  out <- paste0(outpath, id, '.xml')
  message(paste0("Writing eml file ", out))
  write_xml(eml, file=out)
}


