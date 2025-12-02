library(tidyverse)

datapath <- '~/Downloads/volume-20251112.csv'
# Filtering strings
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
      # $160 per pub based on optimistic accounting (1M/5000pubs*yr-1)*.8 of cost is human
      npubcost = npubs * 160,
      ccostperbyte = ccost/cbytes,
      costperbyte = mcost/bytes)
  
  return(list(df, df_m))
}
# Filter by date
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



## Look at EDI specifically (filter by date and scope)
dfs_edi <- get_dfs(datapath, filt_edi)
dfa_edi <- dfs_edi[[1]]
dfm_edi <- dfs_edi[[2]]

# Now reduce the dataset to 2022 forward (period of active grant)
dfm_edi_sub <- dfm_edi %>% filter(datetime > "2021-12-31 24:00")

require(zoo)
# Plot number of monthly uploads
pubsfig <- ggplot(data = dfm_edi_sub, aes(x=datetime, y=npubs)) +
  geom_line(aes(y = zoo::rollmean(
    npubs, 3, na.pad = TRUE, align = "right"))) + 
  geom_smooth(method='lm', linewidth=0.5) +
  ylab('Monthly publications') + xlab('') #+
  # Monthly cost of pubs (this failed)
  # geom_line(aes(x=datetime, y=npubcost/coeff)) + 
  # geom_smooth(aes(x=datetime, y=npubcost/coeff), method = lm, formula = y ~ splines::ns(x, 3), se = FALSE)
  # geom_smooth(aes(x=datetime, y=npubcost/coeff), method = lm, formula = y ~ log(x), se = FALSE)
  # geom_smooth(aes(x=datetime, y=npubcost/coeff), method = lm, formula = y ~ 5000 + 8000*(1-exp(-exp(0.5) * x)), se = FALSE)
  # geom_smooth(aes(x=datetime, y=npubcost/coeff), method="nls", formula=y~SSasympOff(datetime, A, lrc, c0), color="blue", fullrange=T)
  
pubsfig


# Function to extend a regression line by n years
predictfor <- function(model, data, years){
  pred_x <- seq(max(dfm_edi_sub$datetime),
                max(dfm_edi_sub$datetime) + (years*3.15576e+07), by = "month")
  pred_lines <- data.frame(datetime=pred_x,
    y=predict(model, data.frame(datetime=pred_x)))
  return(pred_lines)
}

# Fit a linear model to npubs
mnpubs <- lm(npubs ~ datetime, data=dfm_edi_sub)
pred_npub <- predictfor(mnpubs, dfm_edi_sub, 3.338)

# Add Greg's model - declining pub costs from 160 to ...
pred_npub['t'] <- 1:nrow(pred_npub)
pred_npub['pubcost_dec'] <- 160 * (150/160)^(pred_npub$t/length(pred_npub$t))
pred_npub['pubcost'] <- pred_npub$pubcost_dec * pred_npub$y # Declining cost * npubs

# Now add Pauls cost model to pred_npub
baseCost = 500000 / 12 # $/mo, monthly base cost to keep EDI alive
# notCurated packages
C0 = 10 # current cost per non-curated publication
Cf = 10 # target cost per non-curated publication
# C1, Model for costs per non-curated publication
pred_npub['C1'] <- C0*(Cf/C0)^(pred_npub$t/length(pred_npub$t)) # cost per publication at time T
pred_npub['notCuratedPackages'] <- 250 + pred_npub$t*5
# Curated packages
C0 = 80 # current cost per curated publication ($500k/5000 DOIs)*0.8 (Paul estimated 200 or 50)
Cf = 40 # target cost per curated publication (half of C0)
# C2, Model for costs per curated publication
pred_npub['C2'] = C0*(Cf/C0)^(pred_npub$t/length(pred_npub$t)) # cost per publication at time T

CuratedPackages = 51 + pred_npub$t*2 # This is superseded by the npubs number (linear model)
# Add some total monthly costs, growth costs, and pub costs
pred_npub['TotalMonthlyCosts'] <- baseCost + pred_npub$C1 * 
  pred_npub$notCuratedPackages + pred_npub$C2 * pred_npub$y#CuratedPackages
pred_npub['marginalGrowthCosts'] <- pred_npub$TotalMonthlyCosts - baseCost
pred_npub['CostsPerPub'] <- pred_npub$marginalGrowthCosts / (pred_npub$notCuratedPackages + 
  pred_npub$y)#CuratedPackages)
# Constant pub cost model
# pred_npub['C2_const'] = C0*1^(pred_npub$t/length(pred_npub$t)) # Hold cost per pub constant
# pred_npub['TotalMonthlyCosts_const'] <- baseCost + pred_npub$C1 * 
#   pred_npub$notCuratedPackages + pred_npub$C2_const * pred_npub$y#CuratedPackages
# pred_npub['marginalGrowthCosts_const'] <- pred_npub$TotalMonthlyCosts_const - baseCost

## Add projections to the plot
# Greg's model numbers ($160 * npubs increments)
costlabels <- c("$3.2K", "$6.4K", "$9.6K", "$12.8K")
coeff <- 159
# Paul's model numbers (marginalGrowthCosts increments)
costlabels <- c("$4K", "$6K", "$8K", "$10K")
coeff <- 128
pubsfig <- pubsfig + geom_line(data=pred_npub, aes(x=datetime, y=y),
                               lty=2, color='blue', linewidth=0.5) +
  theme_bw() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        axis.text.x=element_blank()) +
  # This is for Greg's model
  geom_line(data=pred_npub, aes(x=datetime, y=pubcost/coeff),
                               lty=2, color='dark green', linewidth=1) +
  geom_hline(yintercept=20, lty=3, color='dark green') +
  annotate("text", label = costlabels[1],
    x = as.POSIXct("2028-11-01 00:00"), y = 23, size = 3.5, colour = "dark green") +
  geom_hline(yintercept=40, lty=3, color='dark green') +
  annotate("text", label = costlabels[2],
    x = as.POSIXct("2028-11-01 00:00"), y = 43, size = 3.5, colour = "dark green") +
  geom_hline(yintercept=60, lty=3, color='dark green') +
  annotate("text", label = costlabels[3],
    x = as.POSIXct("2028-11-01 00:00"), y = 63, size = 3.5, colour = "dark green") +
  geom_hline(yintercept=80, lty=3, color='dark green') +
  annotate("text", label = "Cost model 1",
    x = as.POSIXct("2023-02-01 00:00"), y = 77, size = 3.5, colour = "black") +
  annotate("text", label = costlabels[4],
    x = as.POSIXct("2028-11-01 00:00"), y = 83, size = 3.5, colour = "dark green") +
  annotate("text", label = "Cost model 2",
    x = as.POSIXct("2023-02-01 00:00"), y = 83, size = 3.5, colour = "red")# +
  # This adds a second y axis (for checking alignment only)
  # scale_y_continuous(
  #   # Features of the first axis
  #   name = "Monthly publications",
  #   # Add a second axis and specify its features
  #   sec.axis = sec_axis(~.*coeff, name="Monthly curation costs")) +
  # This is for paul's cost model
  # geom_line(data=pred_npub, aes(x=datetime, y=marginalGrowthCosts/coeff),
  #                              lty=2, color='dark green', linewidth=1) +
  # geom_hline(yintercept=4000/coeff, lty=3, color='dark green') +
  # annotate("text", label = costlabels[1],
  #   x = as.POSIXct("2028-11-01 00:00"), y = 4400/coeff, size = 3.5, colour = "dark green") +
  # geom_hline(yintercept=6000/coeff, lty=3, color='dark green') +
  # annotate("text", label = costlabels[2],
  #   x = as.POSIXct("2028-11-01 00:00"), y = 6400/coeff, size = 3.5, colour = "dark green") +
  # geom_hline(yintercept=8000/coeff, lty=3, color='dark green') +
  # annotate("text", label = costlabels[3],
  #   x = as.POSIXct("2028-11-01 00:00"), y = 8400/coeff, size = 3.5, colour = "dark green") +
  # geom_hline(yintercept=10000/coeff, lty=3, color='dark green') +
  # annotate("text", label = "Cost model 1",
  #   x = as.POSIXct("2023-02-01 00:00"), y = 9600/coeff, size = 3.5, colour = "black") +
  # annotate("text", label = costlabels[4],
  #   x = as.POSIXct("2028-11-01 00:00"), y = 10400/coeff, size = 3.5, colour = "dark green") +
  # annotate("text", label = "Cost model 2",
  #   x = as.POSIXct("2023-02-01 00:00"), y = 10400/coeff, size = 3.5, colour = "red")

pubsfig
ggsave('~/GD_gmaurer@nmsu/_current/_proposals_and_reports/EDI_2025_PAPPG/pubsfig.png', pubsfig,
  width = 7, height=5)

# Now reduce the full dataset to 2022 forward (period of active grant)
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
cbytesfig <- ggplot(data = dfm_sub, aes(x=datetime, y=cbytes/1000000 + 15)) +
  geom_line() + geom_smooth(method='lm', linewidth=0.5) +
  ylab('Cumulative TB') + xlab('')

# Storage model figure
mgb <- lm(cbytes/1000000 + 15 ~ datetime, data=dfm_sub) # Add CAP/JRN committments here too
# Prediction for next four years
pred_gb <- predictfor(mgb, dfm_sub, 3.338)
# Predicted with additional storage budget of 2TB/year
pred_gb['y2'] = seq(pred_gb$y[1], pred_gb$y[length(pred_gb$y)] + 8, length.out=length(pred_gb$y))

# Create the fugure
cbytesfig <- cbytesfig + geom_line(data=pred_gb, aes(x=datetime, y=y), lty=2, colour='blue') +
  geom_line(data=pred_gb, aes(x=datetime, y=y2), lty=2, colour='dark green', linewidth=1) +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title=element_text(size=14)) +
  geom_hline(yintercept=30, lty=3, color='dark green') +
  annotate("text", label = "$690",
    x = as.POSIXct("2028-11-01 00:00"), y = 31, size = 3.5, colour = "dark green") +
  geom_hline(yintercept=35, lty=3, color='dark green')  +
  annotate("text", label = "$805",
    x = as.POSIXct("2028-11-01 00:00"), y = 36, size = 3.5, colour = "dark green") +
  geom_hline(yintercept=40, lty=3, color='dark green')  +
  annotate("text", label = "$920",
    x = as.POSIXct("2028-04-01 00:00"), y = 41, size = 3.5, colour = "dark green") +
  geom_hline(yintercept=45, lty=3, color='dark green') + #geom_hline(yintercept=3000, lty=3, color='red') +
  annotate("text", label = "Cost model 1",
    x = as.POSIXct("2023-02-01 00:00"), y = 44, size = 3.5, colour = "black") +
  annotate("text", label = "$1,035",
    x = as.POSIXct("2028-11-01 00:00"), y = 46, size = 3.5, colour = "dark green") +
  annotate("text", label = "Cost model 2",
    x = as.POSIXct("2023-02-01 00:00"), y = 46, size = 3.5, colour = "red")

cbytesfig

# Join figures together with patchwork
library(patchwork)
combfig <- pubsfig / cbytesfig  + plot_layout(axes = "collect")
combfig

ggsave('~/GD_gmaurer@nmsu/_current/_proposals_and_reports/EDI_2025_PAPPG/costfig_greg.png', combfig,
  width =4, height = 5.5)

  