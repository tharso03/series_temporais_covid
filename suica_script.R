#### SUIÇA

library(COVID19)
library(fpp3)
library(dplyr)
library(forecast)


dfsuica <- covid19(country = 'CHE', level = 1)

tbsuica <- as_tsibble(dfsuica,
                      index = date) # Começa a partir do id 34.

tbsuica <- tbsuica %>%
  filter(!is.na(confirmed)) # Removidos valores ausentes

tbsuica%>%
  autoplot(confirmed)+
  xlab('Date')+
  ylab('Casos confirmados')


tbsuica <- tbsuica%>%
  mutate(confdaily = confirmed - lag(confirmed))

tbsuica%>%
  autoplot(confdaily)+
  xlab('Date')+
  ylab('Casos confirmados')

tbsuica <- tbsuica %>%
  filter(!is.na(confdaily))

# STL DECOMPOSITION

swz_cases <- tbsuica %>%
  select(id,date,confdaily) %>%
  filter(!is.na(confdaily))

swz_cases %>%
  model(
    STL(confdaily ~ trend(window = 7)+
          season(window = 'periodic'))
  )%>%
  components()%>%
  autoplot()


#### ETS MODEL #####



ets_fit_swz <- swz_cases %>%
  model(
    AAdA = ETS(confdaily ~ error('A') + trend('Ad') + season('A'))
  )

ets_fit_swz %>% 
  fabletools::forecast(h = '15 days')%>%
  autoplot(swz_cases[swz_cases$date>'2021-07-01',], level = NULL)+
  xlab('Data')+
  ylab('Casos diários')



# Previsões podem assumir valores negativos. Intervalos de previsão também.



################ MODELO ARIMA #####################

unitroot_ndiffs(tbsuica$confdaily)

acf(tbsuica$confdaily - lag(tbsuica$confdaily), na.action = na.pass, main ='')
pacf(tbsuica$confdaily - lag(tbsuica$confdaily), na.action = na.pass, main = '')

# TESTAR MODELOS AR(15) e automático nos dados diferenciados 2x

# TESTAR ARIMA SAZONAL

arima_fit_ch <- tbsuica %>%
  model(
    auto = ARIMA(confdaily ~ pdq(2,0,3) + PDQ(0,1,0))
  )

glance(arima_fit_ch) %>% arrange(AICc) %>% select(.model:BIC)


arima_fit_ch %>% gg_tsresiduals()

ljung_box(resid(arima_fit_ch)$.resid)

shapiro.test(resid(arima_fit_ch)$.resid)


fabletools::forecast(arima_fit_ch, h = 15)%>%
  filter(.model == 'auto')%>%
  autoplot(tbsuica[tbsuica$date>'2021-07-01',])+
  xlab('Date')+
  ylab('Casos diários')







