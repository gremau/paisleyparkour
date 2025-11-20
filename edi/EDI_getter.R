library(ecocomDP)
library(EDIutils)
library(emld)

df <- search_data()
df['doi'] <- NA
df['start_yr'] <- NA
df['end_yr'] <- NA
df['citation'] <- NA
df['size'] <- NA

for(i in 1:nrow(df)){
  
  if(df$source[i]=="EDI"){
    doi <- EDIutils::read_data_package_doi(df[[i,'id']], as_url=T, env='production')
    df$doi[i] = doi
    eml <- as_emld(read_metadata(df[[i,'id']]))
    begin <- eml$dataset$coverage$temporalCoverage$rangeOfDates$beginDate$calendarDate
    df$start_yr[i] <- strsplit(begin, '-')[[1]][1]
    end <- eml$dataset$coverage$temporalCoverage$rangeOfDates$endDate$calendarDate
    df$end_yr[i] <- strsplit(end, '-')[[1]][1]
  
    df$citation[i] <- read_data_package_citation(df[[i,'id']])
    
    df$size[i] <- sum(read_data_entity_sizes(df[[i,'id']])$size)/1000
  }
  Sys.sleep(0.5)
}

write.csv(df, '~/Downloads/test.csv')
