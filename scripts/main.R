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
  labs(title = 'Preço médio do m² ajustado pela inflação (Construção civil) (Estado de SP)',
       x = 'Mês/ano',
       y = 'Preço') +
  theme_bw()

plot_box_sazonal <- ggplot(serie %>% as_tibble() %>% mutate(data = month(data, label = TRUE)),
                           aes(x = data, y = preco_m2, fill = data)) +
  geom_violin(draw_quantiles = 0.5) +
  labs(title = 'Boxplot do preço médio do m² agrupados por mês',
       x = 'Mês',
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

#---Previsões---#
source('scripts/rollingWindow.R')
serie$valor <- serie$preco_m2 # a coluna de valores da série precisa chamar 'valor' (vai facilitar nossa vida caso precisemos trocar de série)
previsoes <- rollingWindow(serie, n = 50, h = 6)


#--- Salvando as imagens ---#
lista_imagens <- ls()[str_starts(ls(), pattern = 'plot_')]
sapply(lista_imagens, function(x) {
  ggsave(str_glue(x, '.png'), plot = get(x), device = 'png', path = 'plots')
})
