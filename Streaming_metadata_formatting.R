library(tidyverse)

df <- read_csv('~/Downloads/NewMetadata.csv')

datasets <- data.frame('template_name' = unique(df$DatasetName),
                       'category' = 'Data',
                       'field' = 'entityName',
                       'contents' = NA,
                       'temporal' = NA,
                       'obstype' = NA)

splits <- str_split_fixed(datasets$template_name, '_', n=4)

datasets <- datasets %>% mutate(
  temporal = ifelse(str_detect(splits[,4], '30min'), '30-minute',
                    ifelse(str_detect(splits[,4], '1_Hour'), '1-hour',
                            ifelse(str_detect(splits[,4], 'Ppt'), 'Detailed (1-second)',
                                   ifelse(str_detect(splits[,4], '_5min'), '5-minute',
                                          ifelse(str_detect(splits[,4], 'Daily'), 'Daily',
                                                 '15-minute'))))),
  obstype = ifelse(str_detect(splits[,4], 'Moisture'), 'soil moisture',
                   ifelse(str_detect(splits[,4], 'vwc'), 'soil moisture',
                          ifelse(str_detect(splits[,4], 'Ppt'), 'precipitation', 'meteorology'))),
  site = toupper(splits[,3]),
  contents = ifelse(str_detect(splits[,4], 'vwc'), 
                    paste(temporal, obstype, 'data table for the',
                          site, "soil substation", sep=' '),
                    paste(temporal, obstype, 'data table for the', site,
                          "weather station", sep=' '))) %>%
  select(template_name, category, field, contents)


head(datasets)

write_csv(datasets, '~/Downloads/metadata_template_entityName.csv')
