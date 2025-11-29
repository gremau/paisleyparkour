library(tidyverse)

datapath <- '~/Downloads/volume-20251112.csv'
filt_all <- 'datetime > "2014-12-31 24:00" & datetime < "2025-11-1-01 00:00"'
filt_edi <- 'datetime > "2015-12-31 24:00" & datetime < "2025-11-1-01 00:00" & scope=="edi"'
# Create a dataset for 2016 forward with detailed uploads/publications
# Function to create a cumulative dataset with filtering
get_dfs <- function(datapath, filterstring){
  df <- read_csv(datapath, col_names = c('datetime', 'packageid','bytes')) |>
    mutate(
      scope = case_when(
        grepl('knb-lter', packageid) ~ 'lter',
        grepl('edi', packageid) ~ 'edi',
        .default = 'other')) |>
    filter(eval(str2lang(filterstring))) |>
    mutate(year = year(datetime),
          month = month(datetime),
          decimal_yrs = lubridate::decimal_date(datetime), # Decimal years
          decimal_yrs_diff = c(decimal_yrs[1], diff(decimal_yrs)), # Decimal years between events
          bytes = replace_na(bytes, 0)/1e+06, # Currently in megabytes
          cbytes = cumsum(bytes), # Cumulative bytes
          ccost = decimal_yrs * 1e+06, # Cumulative dollars spent (1M/year)
          ccostperbyte = ccost/cbytes #Cost per byte cumulative
          )
  # Reduce this to a monthly dataset
  df_m <- df |> group_by(year, month) |>
    summarize(
      datetime = max(datetime),
      cbytes = max(cbytes),
      npubs = n_distinct(packageid),
      bytes = sum(bytes)) |>
    ungroup() |> 
    mutate(
      decimal_yrs = lubridate::decimal_date(datetime), # Decimal years
      decimal_yrs_diff = c(decimal_yrs[1]-2016, diff(decimal_yrs)),
      ccost = decimal_yrs * 1e+06, # Cumulative dollars spent (1M/year)
      mcost = decimal_yrs_diff * 1e+06, # Monthly cost
      ccostperbyte = ccost/cbytes,
      costperbyte = mcost/bytes)
  
  return(list(df, df_m))
}

dfs <- get_dfs(datapath, filt_all)
dfa <- dfs[[1]]
dfm <- dfs[[2]]

# Plot change in bytes
ggplot(data = dfa, aes(x=datetime, y=cbytes)) +
  geom_line()# + geom_smooth(method='lm')
# Plot cumulative cost per byte
ggplot(data = dfa[3200:nrow(dfa),], aes(x=datetime, y=ccostperbyte)) +
  geom_line()# + geom_smooth(method='lm')

# Same patterns in Monthly data?
# Plot change in bytes
ggplot(data = dfm, aes(x=datetime, y=cbytes)) +
  geom_line() + geom_smooth(method='lm')
# Plot cumulative cost per byte
ggplot(data = dfm[3:nrow(dfm),], aes(x=datetime, y=ccostperbyte)) +
  geom_line()# + geom_smooth(method='lm')

#Yes

# Plot number of monthly uploads
ggplot(data = dfm, aes(x=datetime, y=npubs)) +
  geom_line() + geom_smooth(method='lm')
# Plot size of monthly uploads over time
ggplot(data = dfm, aes(x=datetime, y=bytes)) +
  geom_line() + geom_smooth(method='lm')
# Plot monthly cost per byte
ggplot(data = dfm[1:nrow(df_m),], aes(x=datetime, y=costperbyte)) +
  geom_line() + geom_smooth(method='lm')



## Look at EDI specifically

dfs_edi <- get_dfs(datapath, filt_edi)
dfa_edi <- dfs_edi[[1]]
dfm_edi <- dfs_edi[[2]]

# Now reduce the dataset to 2022 forward (period of active grant)
dfm_edi_sub <- dfm_edi %>% filter(datetime > "2021-12-31 24:00") |>

require(zoo)
coeff = 50
# Plot number of monthly uploads
pubsfig <- ggplot(data = dfm_edi_sub, aes(x=datetime, y=npubs)) +
  geom_line(aes(y = rollmean(
    npubs, 3, na.pad = TRUE, align = "right"))) + 
  geom_smooth(method='lm') +
  # $80 per pub based on optimistic accounting (1M/5000pubs*yr-1)*.8 of cost is human * 0.5hr/pub
  # geom_line(aes(x=datetime, y=(npubs*80)/coeff)) + 
  # scale_y_continuous(
  #   # Features of the first axis
  #   name = "Monthly publications",
  #   # Add a second axis and specify its features
  #   sec.axis = sec_axis(~.*coeff, name="Monthly curation costs")) +
  # geom_smooth(method = "lm", se = F, formula = y ~ splines::bs(., 2, knots=c('2025-12-31'))) +
  ylab('Monthly publications') + xlab('')

pubsfig


# Function to extend a regression line by n years
predictfor <- function(model, data, years){
  pred_x <- c(max(data$datetime),max(data$datetime)+(years*3.15576e+07))
  pred_lines <- data.frame(datetime=pred_x,
    y=predict(model, data.frame(datetime=pred_x)))
  return(pred_lines)
}

mnpubs <- lm(npubs ~ datetime, data=dfm_edi_sub)
pred_npub <- predictfor(mnpubs, dfm_edi_sub, 4)
pubsfig <- pubsfig + geom_line(data=pred_npub, aes(x=datetime, y=y), lty=2, color='blue') +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title=element_text(size=14),
                     axis.text.x=element_blank()) +
  geom_hline(yintercept=80, lty=3, color='dark green') + #geom_hline(yintercept=75, lty=3, color='red') +
  annotate("text", label = "Cost model 1",
    x = as.POSIXct("2023-02-01 00:00"), y = 75, size = 3.5, colour = "dark green") +
  annotate("text", label = "Cost model 2",
    x = as.POSIXct("2023-02-01 00:00"), y = 85, size = 3.5, colour = "red")

pubsfig
ggsave('~/GD_gmaurer@nmsu/_current/_proposals_and_reports/EDI_2025_PAPPG/pubsfig.png', pubsfig,
  width = 7, height=5)




# Now reduce the dataset to 2022 forward (period of active grant)
dfm_sub <- dfm %>% filter(datetime > "2021-12-31 24:00")# |>

# Plot size of monthly uploads over time
ggplot(data = dfm_sub, aes(x=datetime, y=bytes)) +
  geom_line() + geom_smooth(method='lm') +
  ylab('Monthly avg. MB published') + xlab('')

# Plot monthly cost per byte
ggplot(data = dfm_sub[1:(nrow(dfm_sub)-1),], aes(x=datetime, y=costperbyte)) +
  geom_line() + geom_smooth(method='lm') +
  ylab('Monthly avg. cost per MB') + xlab('')

# Plot cumulative mb over time. Adding commitments from CAP and JRN of 15TB
cbytesfig <- ggplot(data = dfm_sub, aes(x=datetime, y=cbytes/1000 + 15000)) +
  geom_line() + geom_smooth(method='lm') +
  ylab('Cumulative GB') + xlab('')

mgb <- lm(cbytes/1000 + 15000 ~ datetime, data=dfm_sub) # Add CAP/JRN committments here too
# Prediction for next four years
pred_gb <- predictfor(mgb, dfm_sub, 4)
# Predicted with additional storage budget of 2TB/year
pred_gb['y2'] = c(pred_gb$y[1], pred_gb$y[2]+8000)
cbytesfig <- cbytesfig + geom_line(data=pred_gb, aes(x=datetime, y=y), lty=2, colour='blue') +
  geom_line(data=pred_gb, aes(x=datetime, y=y2), lty=2, colour='black') +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title=element_text(size=14)) +
  geom_hline(yintercept=45000, lty=3, color='dark green') + #geom_hline(yintercept=3000, lty=3, color='red') +
  annotate("text", label = "Cost model 1",
    x = as.POSIXct("2023-02-01 00:00"), y = 44000, size = 3.5, colour = "dark green") +
  annotate("text", label = "Cost model 2",
    x = as.POSIXct("2023-02-01 00:00"), y = 46000, size = 3.5, colour = "red")

cbytesfig
library(patchwork)
combfig <- pubsfig / cbytesfig
combfig

ggsave('~/GD_gmaurer@nmsu/_current/_proposals_and_reports/EDI_2025_PAPPG/costfig.png', combfig,
  width =4, height = 5.5)

  