library(COVID19)
library(fpp3)
library(dplyr)


dfchina <- covid19(country = 'CHN',level = 1)
dfsuica <- covid19(country = 'CHE', level = 1)


### CHINA

tbchina <- as_tsibble(dfchina,
                      index = date)

difchina <- tbchina%>%
  mutate(confdaily = confirmed - lag(confirmed,default = 0),
         deathdaily = deaths - lag(deaths,default = 0))

autoplot(tbchina,confirmed) # Cumulative cases

autoplot(difchina,confdaily) # Daily cases

autoplot(tbchina,deaths) # Cumulative cases

autoplot(difchina,deathdaily) # Daily cases













### SUÍÇA

tbsuica <- as_tsibble(dfsuica,
                      index = date)

difsuica <- tbsuica %>%
  mutate(confdaily = confirmed - lag(confirmed, default = 0),
         dailydeaths = deaths - lag(deaths,default = 0)) # Há valores NA

autoplot(tbsuica,confirmed) # Cumulative cases

autoplot(difsuica,confdaily) # Daily cases

autoplot(tbsuica,deaths)

autoplot(difsuica,dailydeaths)


