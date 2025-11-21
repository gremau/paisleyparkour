library(ggplot2)
library(lubridate)
library(plyr)
library(reshape2)
library(gridExtra)
library(cowplot)
library(knitr)
library(markdown)
library(xtable)
library(zoo)
library(devtools)
library(dplyr)
library(tidyr)
library(vegan)
library(codyn)

###Read in files### read in files

#All plant cover/volume data
Cover <- read.csv ("sev129_nppcorequadrat_20161214.csv", header=TRUE, strip.white = TRUE)
colnames(Cover)[9] <- "SpeciesCode"

#exclude blue grama site
Cover <- Cover[Cover$site != "B",]
head(Cover)

#####calculate total cover
Cover <- unique(ddply(unique(Cover[,c("year", "season", "site", "web", "plot", "quad", "SpeciesCode", "obs", "cover", "height", "count")]), 
                       c("year", "season", "site", "web", "plot", "quad", "SpeciesCode", "obs","height"), function(x)
                         data.frame(tcover = prod(x$cover*x$count, na.rm=T))))
head(Cover)
##Calculations  Now we need to calculate the different metrics of plant production. 
##First we'll calculate "mean.volume," the average volumetric space (cm^3) that each species occupies per quadrat per season. 
# Calculate the number of quads per site per season

quads <- ddply(Cover, c("year", "season", "site"), function(x)
  data.frame(quadnum = length(unique(paste(x$web, x$plot, x$quad)))))

# Merge quadnum column
Cover <- unique(merge(Cover, quads, c("year", "season", "site"), all=T))
head(Cover)

# Calculate all the average volumes /species /quad in the harvest dataset
sumcover <- ddply(unique(Cover[,c("year", "season", "site","web", "plot", "quad", "quadnum", "SpeciesCode", "tcover")]), 
                    c("year", "season","site", "web", "plot", "quad", "quadnum", "SpeciesCode"), function(x)
  data.frame(quad.cover = sum(x$tcover, na.rm=T)))
head(sumcover)

# Now calculate the average volume across all quads /species
meanvolumes <- unique(ddply(unique(sumcover[,c("year", "season", "site","SpeciesCode", "quadnum", "quad.cover")]), 
                     c("year", "season", "site","SpeciesCode"), function(x)
  data.frame(mean.cover = sum(x$quad.cover, na.rm=T)/x$quadnum)))

meanvolumes$season <- as.factor(meanvolumes$season)
head(meanvolumes)

# Revalue season column
meanvolumes$season <- revalue(meanvolumes$season, c(
  "1" = "winter",
  "2" = "1",
  "3" = "2"))
head(meanvolumes)

#exclude winter
meanvolumes <- meanvolumes[meanvolumes$season != "winter",]
head(meanvolumes)

# Taxonomic, PFT information for all species
sev.species <- read.csv("sevilleta_species_list.csv", header=TRUE, strip.white=TRUE)
colnames(sev.species)[1] <- "SpeciesCode"
head(sev.species)

# Merge npp and species data, dates first
Cover.volume <- merge(meanvolumes, sev.species, "SpeciesCode", all.x=T)
head(Cover.volume)

# Make a column of average-season dates which will help make prettier figures
Cover.volume$fakedate <- NA
Cover.volume$fakedate[Cover.volume$season=="1"] <- "4/1"
Cover.volume$fakedate[Cover.volume$season=="2"] <- "10/1"
Cover.volume$fakedate <- as.Date(paste(Cover.volume$fakedate, Cover.volume$year), format="%m/%d %Y") 

head(Cover.volume)
####Plant functional types Since we don't actually care about species-specific numbers, we can condense all of these data into plant functional groups. I have code for traditional groups (creosote, non-creosote C3 plants, C4, and CAM), but am masking it. Including plots for our new PFTs
### Create our own PFT's
##### We should come up with new PFT's!
# Possible categories: LATR, "woody" perennial forbs, annual juicy forbs, perennial juicy forbs, annual grasses, perennial grasses, OR spring-blooming grasses, monsoon-blooming grasses
Cover.volume$PFT <- as.character(NA)
Cover.volume$PFT[Cover.volume$g_f == "g"] <- "C4_grass"
Cover.volume$PFT[Cover.volume$g_f == "g" & Cover.volume$path == "C3"] <- "C3_grass"
Cover.volume$PFT[Cover.volume$g_f == "f"] <- "C3_forb"
Cover.volume$PFT[Cover.volume$g_f == "f" & Cover.volume$path == "C4"] <- "C4_forb"
Cover.volume$PFT[Cover.volume$g_f == "s" & Cover.volume$path == "C3"] <- "C3_shrub_subshrub"
Cover.volume$PFT[Cover.volume$path == "CAM"] <- "CAM"
Cover.volume$PFT[Cover.volume$SpeciesCode == "PHACE"] <- "C3_forb"
Cover.volume$PFT[Cover.volume$SpeciesCode == "ASMIM"] <- "C3_forb"
Cover.volume$PFT[Cover.volume$SpeciesCode == "SCLA6"] <- "C3_forb"
Cover.volume$PFT[Cover.volume$SpeciesCode == "GUWR"] <- "C3_forb"
Cover.volume$PFT[Cover.volume$SpeciesCode == "STEM"] <- "C3_shrub_subshrub"
Cover.volume$PFT[Cover.volume$SpeciesCode == "ATCA2"] <- "C4_forb"
Cover.volume$PFT[Cover.volume$SpeciesCode == "ASMIM"] <- "C3_forb"
Cover.volume$PFT[Cover.volume$SpeciesCode == "ESVIV"] <- "CAM"
Cover.volume$PFT[Cover.volume$SpeciesCode == "PRGLT"] <- "C3_shrub_subshrub"
Cover.volume$PFT[Cover.volume$SpeciesCode == "STEM"] <- "C3_shrub_subshrub"
Cover.volume$PFT[Cover.volume$SpeciesCode == "RANE"] <- "C3_forb"
Cover.volume$PFT <- as.factor(Cover.volume$PFT)
Cover.volume[is.na(Cover.volume$PFT),]
# Fix photo_path gaps and remove unknowns
unique(Cover.volume$SpeciesCode[is.na(Cover.volume$SpeciesCode)])
Cover.volume$SpeciesCode[!is.na(Cover.volume$SpeciesCode) &
                                 Cover.volume$SpeciesCode == "NONE"] 
Cover.volume <- Cover.volume[Cover.volume$SpeciesCode != "-888",]
Cover.volume <- Cover.volume[Cover.volume$SpeciesCode != "-999",]
head(Cover.volume)

# Summarize plant cover by Plant functional type
Cover.volume <- unique(Cover.volume)
PFTseasonsummary <- ddply(unique(Cover.volume[,c("fakedate","year", "season", "site","PFT", "mean.cover")]), 
                         c("fakedate","year", "season", "site", "PFT"), function(x)
  data.frame(mean.coverPFT = sum(x$mean.cover, na.rm=TRUE)))

PFTseasonsummary <- data.frame(summarise(group_by(PFTseasonsummary, fakedate, year, site, season, PFT), mean.coverPFT = sum(mean.coverPFT)))
head(PFTseasonsummary)

###converting to wide format
plants <- spread(PFTseasonsummary, PFT, mean.coverPFT)
plants[is.na(plants)] <- 0
head(plants)

# Met data
met.data <- read.csv("Met_all.csv", header=TRUE, strip.white=TRUE)
# only use station #49 data
met.data <- met.data[met.data$Sta==49,]
# only keep data and precip data
head(met.data)
met.data <- unique(met.data[,c(2, 3, 4, 5, 7, 8, 9, 11)])
head(met.data)
met.data$season <- as.factor(met.data$Month)
met.data$season <- revalue(met.data$season, c("1"="1","2"="1","3"= "1","4"="1","5"="1","6"="1",
                                              "7"="2","8"="2","9"="2","10"="2","11"="1" ,"12"="1"))
colnames(met.data) <- c("mean.date", "month", "day","year", "Avg_Temp", "Max_Temp", "Min_Temp","Precip", "season")
met.data[met.data==-999] <- NA
head(met.data)

# Make a column of average-season dates which will help make prettier figures
# Format date column 
met.data$mean.date <- as.Date(met.data$mean.date, format="%m/%d/%Y")

#making fake date to wrap years
unique(month(met.data$mean.date[met.data$season==1])) 
met.data$fakedate <- NA
met.data$fakedate[met.data$season=="1"] <- "4/1"
met.data$fakedate[met.data$season=="2"] <- "10/1"
met.data$fakedate <- as.Date(paste(met.data$fakedate, met.data$year), format="%m/%d %Y")
met.data$fakedate[met.data$month==11 | met.data$month==12] <- 
met.data$fakedate[met.data$month==11 | met.data$month==12] + years(1)
unique(month(met.data$fakedate[month(met.data$mean.date)==11])) #should be 4/April
unique(met.data$fakedate[month(met.data$mean.date)==11 &
                           year(met.data$mean.date)==2000]) #should be April 2001
head(met.data)
# Calculate monthly precip temp min and max
monthlysums <- unique(ddply(met.data, c("fakedate", "month"), function(x)
  data.frame(month.precip = sum(x$Precip, na.rm=TRUE),
             month.temp = mean(x$Avg_Temp, na.rm=TRUE),
             month.max = max(x$Max_Temp),
             month.min = min (x$Min_Temp))))
head(monthlysums)
monthlysums$temp.date <- as.Date(paste(1, monthlysums$month, monthlysums$year), 
                                 format="%d %m %Y")
#Calculate seasonal precip
seasonsums <- unique(ddply(met.data, c("fakedate", "season"), function(x)
  data.frame(season.precip = sum(x$Precip, na.rm=TRUE),
             season.temp = mean(x$Avg_Temp, na.rm=TRUE),
             minimum.temp = min(x$Min_Temp, na.rm=TRUE),
             maximum.temp = max(x$Max_Temp, na.rm=TRUE))))
head(seasonsums)


## Try to summarize the trapping data, try is the operative word
sev.trap <- read.csv("sev008_rodentpopns_20161027.csv", header=TRUE, strip.white=TRUE)
sev.trap <- sev.trap[sev.trap$location=="5pgrass" | sev.trap$location=="5plarrea",]

# Calculate the number of nights and webs trapped per season
numnight <- ddply(sev.trap, c("year", "location", "season"), function(x)
  data.frame(numnight = length(unique(paste(x$night, x$web)))))
head(numnight)

# Merge numnight column
sev.trap <- unique(merge(sev.trap, numnight, c("year", "location", "season"), all=T))
head(sev.trap)

##select the years that match veg data
sev.trap <- subset(sev.trap, year>="1999") 
head(sev.trap)

###summarising by season
rodent.long <- data.frame(summarise(group_by(sev.trap, year, location, season, species, numnight), abundance = length(species)))

##calculate mice per webs set
rodent.long$miceperwebs <- rodent.long$abundance/rodent.long$numnight
rodent.long$season <- as.factor(rodent.long$season)
unique(rodent.long$season[is.na(rodent.long$season)]) 
#rename seasons to create two bins
rodent.long$season <- revalue(rodent.long$season, c(
  "1" = "1", 
  "2"="2",
  "3" = "2"))
head(rodent.long)

##create a column of average-season dates which will make it line up with veg and met panels
# Make a column of average-season dates which will help make prettier figures
rodent.long$fakedate <- NA
rodent.long$fakedate[rodent.long$season=="1"] <- "4/1"
rodent.long$fakedate[rodent.long$season=="2"] <- "10/1"
rodent.long$fakedate <- as.Date(paste(rodent.long$fakedate, rodent.long$year), format="%m/%d %Y") 

head(rodent.long)

# Combine mice into families

rodent.long$family <- as.character(rodent.long$species)
rodent.long$family <- revalue(rodent.long$family, c(
  "chin" = "Heteromyid", 
  "dime" = "Heteromyid", 
  "dior"= "Heteromyid", 
  "dipo" = "Heteromyid", 
  "disp" = "Heteromyid", 
  "pgfv" = "Heteromyid",
  "neal" = "Cricetid", 
  "nemi" = "Cricetid",
  "onar"= "Cricetid", 
  "onle"= "Cricetid", 
  "onsp"= "Cricetid", 
  "peer"= "Cricetid", 
  "pele"= "Cricetid", 
  "pema"= "Cricetid", 
  "pedi"= "Cricetid", 
  "pmer"= "Cricetid", 
  "pmle"= "Cricetid", 
  "pmtr"= "Cricetid", 
  "pmdi"= "Cricetid", 
  "pmbo"= "Cricetid",
  "remg"= "Cricetid", 
  "sihi"="Cricetid",
  "spsp"= "Scurid",
"pgfl" = "Heteromyid",
"pmma"= "Cricetid", 
"remn"="Cricetid", 
"resp"="Cricetid"))

rodent.long$numnight<-NULL
rodent.long$abundance<-NULL
rodent.long$fakedate <- as.Date(rodent.long$fakedate, format="%Y-%m-%d")
head(rodent.long)
###summarising by feed type
rodent.long <- data.frame(summarise(group_by(rodent.long, year, location, season, family), miceperwebs = sum(miceperwebs)))
head(rodent.long)

rodent.long$location <- revalue(rodent.long$location, c(
  "5pgrass" = "G", 
  "5plarrea"="C"))
colnames(rodent.long) <- c("year", "site", "season","family", "miceperwebs")
head(rodent.long)

###converting to wide format
rodent <- spread(rodent.long, family, miceperwebs)
rodent[is.na(rodent)] <- 0
head(rodent)

model.data <- unique(merge(plants, seasonsums, c("fakedate", "season"), all=T))
head(model.data)
write.table(model.data, file="Veg_Precipa.csv", sep = ",")  

###merge veg, mice, and precip
final.table <- (unique(merge(model.data, rodent, c("year", "site", "season"), all=T)))
head(final.table)

write.table(rodent, file="Mice.csv", sep=",")
write.table(final.table, file="All_Data.csv", sep = ",")
