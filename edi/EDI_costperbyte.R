library(tidyverse)
df <- read_csv('~/Downloads/volume-20251112.csv', col_names = c('datetime', 'packageid','bytes')) %>%
  mutate(bytes = replace_na(bytes, 0),
         cgbytes = cumsum(bytes/1e+09), # Cumulative GB
         cgbytes2020 = cgbytes - df$cgbytes[1], # Cumulative GB since 2020
         decimal_yrs = lubridate::decimal_date(datetime)-2020, # Decimal years since 2020
         ccost2020 = decimal_yrs * 1e+06, # Cumulative dollars spent (1M/year)
         costpergbyte = cgbytes2020/ccost2020) %>% # Cost per GB
  filter(datetime > "2019-12-31 24:00")


ggplot(data = df, aes(x=datetime, y=cgbytes)) +
  geom_line() + geom_smooth(method='lm')

ggplot(data = df, aes(x=datetime, y=costpergbyte)) +
  geom_line()# + geom_smooth(method='lm')
