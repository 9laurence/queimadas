library(tidyverse)

# ler dado csv pego no get_data
data_raw <- read_csv("data-raw/data.csv") %>% janitor::clean_names()


# Criar vetor com os presidente para cada mes
presidentes <- rep(c("fhc", "lula", "dilma", "temer", "bolsonaro"), 
                   c(12, 8*12, 12*5+4, 8+12*2, 12+4))


# Remover linhas de "total anual" e adicionar vetor dos presidentes
# criar variavel mês e ano e retirar meses sem dados
data <- data_raw %>% filter(!is.na(mes)) %>%  # retirar linhas de total anual
  mutate(ano = as.double(ano), # converter ano em double
         presidente = presidentes, # add coluna dos presidentes
         date = zoo::as.yearmon(paste(ano, mes, sep = "-"))) %>%  # coluna mes-ano
  filter(!is.na(amazonia)) 

# Salvar dados usados
write_csv2(data, "data/dados.csv")


# Criar series temporais para os dados da amazonia
amazonia_ts <- ts(data$amazonia, 
                  start = c(2002, 9),
                  frequency = 12)


# Grafico com os dados para cara presidente
ggplot(data, aes(x = date , y = amazonia))+
  geom_line(aes(color = as.factor(presidente)))


# Criar os períodos de treino e teste para o modelo com intuito de ver
# se ele acerta os dados já existentes
treino_amazonia <- window(amazonia_ts, end = c(2018, 7))
teste_amazonia <- window(amazonia_ts, start = c(2018, 8), end = c(2019, 7))


# Modelos ARIMA e ETS
mod_am_arima <- forecast::auto.arima(treino_amazonia)
a.fcast.arima <- forecast::forecast(mod_am_arima, h = length(teste_amazonia))
a.fcast.arima
forecast::accuracy(a.fcast.arima, teste_amazonia)[2,]

mod_am_ets <- forecast::ets(treino_amazonia)
a.fcast.ets <- forecast::forecast(mod_am_ets, h = length(teste_amazonia))
a.fcast.ets
forecast::accuracy(a.fcast.ets, teste_amazonia)[2,]

# Plotando os dados dos modelos com o dado original para ver qual
# é o melhor modelo
autoplot(teste_amazonia)+
  geom_point()+
  autolayer(a.fcast.arima$mean)+
  autolayer(a.fcast.ets$mean)

# Modelo escolhido foi o modelo ets por conta do RMSE, MAE eMAPE 
# menor e do melhor desempenho na inspeção visual



# criando nova faixa de treino para prever os próximos meses
predicao_amazonia <- window(amazonia_ts, end = c(2019, 7))

# Criando novo modelo ETS para predição futura dos dados
mod_am_ets2 <- forecast::ets(predicao_amazonia)

# Predizendo os próximos 9 meses, até abril de 2020
a.fcast.ets2 <- forecast::forecast(mod_am_ets2, h = 9)
a.fcast.ets2


# Criar ts da amazonia de 2019 e 2020 para melhor visualizacao
amazonia_ts_bolsonaro <- window(amazonia_ts, start = c(2019, 1))


# Visualizacao do modelo e dos seus intervalos de confianca de 80 e 95%
autoplot(amazonia_ts_bolsonaro)+
  geom_point()+
  geom_line(aes(color = 'Dado real'))+
  autolayer(a.fcast.ets2$mean, series="Media")+
  autolayer(a.fcast.ets2$upper)+
  autolayer(a.fcast.ets2$lower)+
  ylab("Área queimada da Amazonia, em km^2")+
  xlab("Tempo, em meses")+
  scale_color_manual('',values = c('red', 'green','black', 'blue'))+
  theme_classic()+
  zoo::scale_x_yearmon(n = 5)

# Salvar grafico
ggsave("data/ts.tiff")
ggsave("data/ts.jpg")

# Resultado: dados de queimada de agosto foram superiores ao esperado
# pelo modelo ETS, porem os meses subsequentes foram dentro do 
# esperado pelo padrão dos anos anteriores. Em 2020, até abril, os 
# dados também foram dentro do esperado .
 
