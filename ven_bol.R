#install.packages("COVID19")
library(COVID19)
library(tidyverse)
library(dplyr)
#library(ggplot2)
#library(plotly)
library(lubridate) # manipular as datas
library("forecast") # séries

df=covid19(country = c("Venezuela","Bolivia"))


# resumo
#res = df %>% 
#  select(confirmed, recovered, deaths) %>%
#  group_by(id) %>% 
#  summarise(confirmados = max(confirmed, na.rm = T),
#            recuperados = max(recovered, na.rm = T),
#            mortes = max(deaths, na.rm = T))

#res_ven = venezuela %>% 
#  select(confirmed, recovered, deaths) %>%
#  summarise(confirmados = max(confirmed, na.rm = T),
#            recuperados = max(recovered, na.rm = T),
#            mortes = max(deaths, na.rm = T))

#res_bol = bolivia %>% 
#  select(confirmed, recovered, deaths) %>%
#  summarise(confirmados = max(confirmed, na.rm = T),
#            recuperados = max(recovered, na.rm = T),
#            mortes = max(deaths, na.rm = T))


#ggplot(data = venezuela, aes(x=date, y=confirmed))+
#  geom_line()

#ggplot(data = venezuela, aes(x=date, y=recovered))+
#  geom_line()

#ggplot(data = venezuela, aes(x=date, y=deaths))+
#  geom_line()



df1 = df %>% select(id, date, confirmed, recovered, deaths)

df2 = df1 %>%
  group_by(id) %>% 
   mutate(confirmados = confirmed - lag(confirmed, default = 0),
          recuperados = recovered - lag(recovered, default = 0),
          mortalidade = deaths - lag(deaths, default = 0))

graf = ggplot(data = df2, aes(x=date, y=confirmados,filter=id))+
  geom_line()
ggplotly(graf)  

sum(df2$confirmados, na.rm=T)

### NA's
sum(is.na(df2$confirmados))

### Substituir NA's
df2 = df2 %>% drop_na(confirmados)
#df2 = mutate_at(df2, 'confirmados', ~replace(., is.na(.), round(mean(df2$confirmados, na.rm = T),0))) 
#df2 = mutate_at(df2, 'confirmados', ~replace(., 0, round(mean(df2$confirmados, na.rm = T),0))) 
View(df2)

## treino e teste (separei 50 dias de cada país para teste)
df_treino = df2 %>% filter(between(date, ymd('2020-01-22'), ymd('2021-07-17')))
df_test = df2 %>% filter(between(date, ymd('2021-07-17'), ymd('2021-10-31')))

# separar a venezuela e a bolivia
df_treino_ven = df_treino %>%  filter(id == 'VEN')
df_treino_bol = df_treino %>%  filter(id == 'BOL')
df_test_ven = df_test %>%  filter(id == 'VEN')
df_test_bol = df_test %>%  filter(id == 'BOL')

# deixar apenas a coluna de casos confirmados 
df_treino_ven = subset(df_treino_ven, select = -c(id, date, confirmed, recovered, deaths, recuperados, mortalidade))
df_treino_bol = subset(df_treino_bol, select = -c(id, date, confirmed, recovered, deaths, recuperados, mortalidade))
df_test_ven = subset(df_test_ven, select = -c(id, date, confirmed, recovered, deaths, recuperados, mortalidade))
df_test_bol = subset(df_test_bol, select = -c(id, date, confirmed, recovered, deaths, recuperados, mortalidade))

#### criação da série
serie_ven = ts(df_treino_ven, start = c(2020,1), end = c(2021,7), frequency = 365)
plot(serie_ven)


##### Média Móvel #####
media <- ma(serie_ven, order = 7, centre = TRUE)
plot(media)
print(media)
plot(serie_ven, xlab = "Tempo (meses)", col = "blue")
lines(media, col="red")


### AUTOARIMA
modelo_auto_ven <- auto.arima(serie_ven, trace = T, stepwise = F, approximation = F,
                          max.p = 5, max.q = 5, max.P = 2, max.Q = 2)

summary(modelo_auto_ven)


# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_auto_ven)

plot(resid(modelo_auto_ven))

qqnorm(resid(modelo_auto_ven))
qqline(resid(modelo_auto_ven))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(resid(modelo_auto_ven))

acf(resid(modelo_auto_ven))
pacf(resid(modelo_auto_ven))

plot(serie_ven)
lines(serie_ven-modelo_auto_ven$resid, col= "red")

previsao <- forecast(modelo_auto_ven,h=24)
plot(previsao)
lines(serie_ven-modelo_auto_ven$resid, col= "red")

prev_escala <- as.data.frame(previsao)
View(prev_escala)











