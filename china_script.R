# SCRIPT CHINA

library(COVID19)
library(fpp3)
library(dplyr)
library(forecast)
library(fable)
library(glarma)

dfchina <- covid19(country = 'CHN',level = 1)

# Os dados têm valores ausentes?

### CHINA

tbchina <- as_tsibble(dfchina,
                      index = date)

tbchina <- tbchina%>%
  mutate(confdaily = confirmed - lag(confirmed))

autoplot(tbchina,confirmed)+
  xlab('Data')+
  ylab('Casos confirmados de COVID-19')

autoplot(tbchina,confdaily)+
  xlab('Data')+
  ylab('Casos confirmados de COVID-19')

# Decomposição STL

china_cases %>%
  model(
    STL(confdaily ~ trend(window = 7)+
          season(window = 'periodic'))
  )%>%
  components()%>%
  autoplot()+
  xlab('Data')


# ETS MODEL

china_cases <- tbchina %>%
  select(id,date,confdaily)

china_cases <- china_cases %>%
  filter(!is.na(confdaily))

ets_fit <- china_cases %>%
  model(
    AAA = ETS(confdaily ~ error('A') + trend('A') + season('A')),
  )

ets_fit %>% 
  forecast::forecast(h = '15 days')%>%
  autoplot(china_cases[china_cases$date>'2021-07-01',], level = NULL)+
  xlab('Data')+
  ylab('Casos confirmados')

ets_fit %>% 
  forecast::forecast(h = '15 days')%>%
  autoplot(china_cases[china_cases$date>'2021-07-01',])+
  xlab('Data')+
  ylab('Casos confirmados')

# Intervalos de previsão muito grandes e que podem assumir valores negativos. Melhor outro modelo.


accuracy(ets_fit)

View(tidy(ets_fit))

# models with multiplicative errors are not stable if the data constains zeros.
# there is trend, but damped, there is seasonality.




########################## SARIMA MODEL###################################

# Precisamos que o modelo seja estacionário

unitroot_ndiffs(tbchina$confirmed) # A série precisa ser diferenciada 2x

tbchina <- tbchina %>%
  mutate(dif1c = confirmed - lag(confirmed,1),
         dif2c = dif1c - lag(dif1c,1))

acf(tbchina$dif2c, na.action = na.pass, main ='')
pacf(tbchina$dif2c, na.action = na.pass, main = '')

tbchina$confirmed%>%
  Arima(order = c(5,2,0))%>%
  residuals() %>% ggtsdisplay()

tbchina$confirmed%>%
  Arima(order = c(0,2,1))%>%
  residuals() %>% ggtsdisplay()

## Os modelos a serem testados são ARIMA(5,2,0), ARIMA(0,2,1) e o automático


##### Com 1 diferença

arima_fit_china_dif <- tbchina %>%
  model(
    auto = ARIMA(dif1c)
  )


arima_fit_china_dif %>% pivot_longer(everything(), names_to = 'Model name',
                                 values_to = 'Orders') # deu o mesmo modelo


arima_fit_china_dif %>% gg_tsresiduals()

ljung_box(resid(arima_fit_china_dif)$.resid)

shapiro.test(resid(arima_fit_china_dif)$.resid)


tbchina <- tbchina %>%
  filter(!is.na(dif1c))


fabletools::forecast(arima_fit_china_dif, h = 15)%>%
  filter(.model == 'auto')%>%
  autoplot(tbchina[tbchina$date>'2021-07-01',])+
  xlab('Date')+
  ylab('Casos diários') # Forecast graph
  
  

######################## GLARMA MODELS #############################



# ARMA POISSON

glarmaPoissonPearson()



# ARMA NEGATIVE BINOMIAL






