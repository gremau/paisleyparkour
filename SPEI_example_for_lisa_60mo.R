library('tidyverse') # you probably have this. If not, install

# This installs from my github. Will also install SPEI, and a couple other R packages
devtools::install_github("gremau/rclimtools")
library('rclimtools')

# Set working directory to wherever the script and input data files are. This
# will also be where new files are written
setwd("~/Desktop")

# Read a dataset downloaded from PRISM ()
df <- read_csv('./PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_provisional_800m_191001_202508_32.6184_-106.7424.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 32.618

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 12)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei12mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei12mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_site1_60mo.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)
# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]
# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]


