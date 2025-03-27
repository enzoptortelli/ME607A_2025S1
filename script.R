library(tidyverse)
library(tsibble)
library(fpp3)
library(astsa)
library(TSA)
library(forecast)
library(patchwork)

#--- Lendo os dados ---#
serie <- read_tsv('dados/serie_preco_m2.tsv', 
                  locale = locale(decimal_mark = ',')) %>% #faz o read_tsv entender que a vírgula é separador decimal
  pivot_longer(!UF, names_to = 'data', values_to = 'preco_m2') %>%
  select(!UF) %>%
  mutate(data = # é mais fácil criar um novo vetor de datas do que tentar arrumar o que veio com os dados (que veio com o nome todo irregular)
           seq(from = ym("1986-03"),
               to = ym("2018-06"),
               by = "month") %>% 
           yearmonth()) %>% #precisamos passar esse vetor de datas para yearmonth, para indicar para o tsibble que os dados são mensais (caso contrário ele entenderia que são diários)
  filter(data >= make_yearmonth(1994, 7)) %>%
  as_tsibble(index = data)

serie_ipca <- read_tsv('dados/serie_ipca.tsv')


#--- Corrigindo a série pela inflação ---#
# Ao invés de desinflacionar a série, nós vamos inflacioná-la (trazer todos os preços para os dias atuais)
n <- dim(serie_ipca)[1]
serie_ipca$ipca <- (serie_ipca$ipca / 100) + 1
serie_ipca$ipca[n] <- 1 # o último mês da série tem que ter ipca neutro, pois ele se corrige o preço para o mês seguinte (que não está nos dados)
ipca_acumulado <- vector(mode = 'numeric', length = n)

for(i in 1:n) {
  temp <- serie_ipca$ipca[i:n]
  ipca_acumulado[i] <- prod(temp)
}
rm(temp, n)

serie_original <- serie
serie$preco_m2 <- serie$preco_m2 * ipca_acumulado

#--- Análise descritiva ---#
# gráfico de sequência
plot_seq_original <- ggplot(serie_original, aes(x = data, y = preco_m2)) +
  geom_line() +
  labs(title = 'Preço médio do m² (Construção civil) (Estado de SP)',
       x = 'Mês/ano',
       y = 'Preço') +
  theme_bw()

plot_seq_ajustada <- ggplot(serie, aes(x = data, y = preco_m2)) +
  geom_line() +
  labs(title = 'Preço médio do m² (Construção civil) (Estado de SP)',
       x = 'Mês/ano',
       y = 'Preço') +
  theme_bw()

# gráfico de sequência separado por ano
plot_seq_season <- gg_season(serie, preco_m2, period = 'year') +
  labs(title = 'Preço médio do m² (Construção civil) (Estado de SP)',
       x = 'Mês/ano',
       y = 'Preço') +
  theme_bw()

# correlograma
plot_acf <- ACF(serie, y = preco_m2) %>% autoplot() + theme_bw()


#--- Modelos ---#
# MEAN, NAIVE, SNAIVE, DRIFT
modelo_mean <- serie %>% model(mean = MEAN(preco_m2))
modelo_naive <- serie %>% model(naive = NAIVE(preco_m2))
modelo_snaive <- serie %>% model(snaive = SNAIVE(preco_m2 ~ lag(12)))
modelo_drift <- serie %>% model(drift = RW(preco_m2 ~ drift()))

## Análise resídual
plot_res_modelo_mean <- gg_tsresiduals(modelo_mean)
plot_res_modelo_naive <- gg_tsresiduals(modelo_naive)
plot_res_modelo_snaive <- gg_tsresiduals(modelo_snaive)
plot_res_modelo_drift <- gg_tsresiduals(modelo_drift)

## Análise de predição
modelos_basicos <- serie %>% model(mean = MEAN(preco_m2),
                          naive = NAIVE(preco_m2),
                          snaive = SNAIVE(preco_m2 ~ lag(12)),
                          drift = RW(preco_m2 ~ drift()))
plot_forecast_modelos_basicos <-
  modelos_basicos %>% forecast(h = 5) %>% autoplot(serie,level = NULL) +
  theme_bw()

serie_stretch <- serie %>%
  stretch_tsibble(.init = 50, .step = 1)

serie_stretch %>%
  model(mean = MEAN(preco_m2),
        naive = NAIVE(preco_m2),
        snaive = SNAIVE(preco_m2 ~ lag(12)),
        drift = RW(preco_m2 ~ drift())) %>%
  forecast(h = 3) %>%
  accuracy(serie)


# REGRESSÃO
mts <- ts(serie$preco_m2, frequency = 12, start = c(1994,7)) #Transformando em ts 
# mts_SMA_12 <- SMA(mts, n=12) # "Manualmente" calculando tendência con n = nº de meses num ano
add_mts <- decompose(mts,type = "additive") # Duas decomposições diferentes
mult_mts <- decompose(mts,type = "multiplicative")

periodogram(mts) # Periodograma sem remover tendência 
periodograma <- data.frame(freq = 0:143/144, periodograma = periodogram(na.omit(mts - add_mts$trend),plot = F)$spec)
1/periodograma$freq[which.max(periodograma$periodograma)] #aproximadamente 6 meses o melhor candidato pra frequência de acordo com o periodograma (apesar de eu achar que não é muito válido)

mts %>%
  autoplot()+
  autolayer(fitted(tslm(mts ~ trend + season)),
  color = "red")+
  autolayer(fitted(tslm(mts ~ trend)),
  color = "blue")+
  labs(title = "Modelo de regressão com tendência e \n com tendência + sazonalidade")  #Séries com os dois modelos de regressão

time_plot <-  autoplot(residuals(tslm(mts ~ trend)), 
  color = "red")+
  labs(title = "Série dos resíduos", y = element_blank(), x = "Tempo")
acf_plot <- ggAcf(tslm(mts ~ trend)$residuals, lag.max = 24) +
  labs(title = "ACF dos resíduos modelo \n com tendência") +
  theme_minimal()
hist_plot <- ggplot(data = NULL, aes(x = tslm(mts ~ trend)$residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "tomato1", color = "black") +
  labs(title = "Histograma dos resíduos modelo \n com tendência", x = "Residuos",y = "Densidade") +
  theme_minimal()

(hist_plot | acf_plot) / time_plot #Análise de resíduos do modelo de regressão com tendência
tslm(mts ~ trend)$coefficients

time_plot <- autoplot(residuals(tslm(mts ~ trend + season)),
         color = "blue")+
         labs(title = "Série dos resíduos", y = element_blank(), x = "Tempo")
acf_plot <-ggAcf(tslm(mts ~ trend + season)$residuals, lag.max = 24) +
  labs(title = "ACF dos resíduos modelo \n com tendência + sazonalidade") +
  theme_minimal()
hist_plot <- ggplot(data = NULL, aes(x = tslm(mts ~ trend + season)$residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histograma dos resíduos modelo \n com tendência + sazonalidade", x = "Residuos",y = "Densidade") +
  theme_minimal()

(hist_plot | acf_plot) / time_plot #Análise de resíduos do modelo de regressão com tendência + sazonalidade
tslm(mts ~ trend + season)$coefficients
#teste


#--- Salvando as imagens ---#
lista_imagens <- ls()[str_starts(ls(), pattern = 'plot_')]
sapply(lista_imagens, function(x) {
  ggsave(str_glue(x, '.png'), plot = get(x), device = 'png', path = 'plots')
})
  