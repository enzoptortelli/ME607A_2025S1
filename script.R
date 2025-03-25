library(tidyverse)
library(tsibble)
library(fpp3)
library(astsa)

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


ggplot(serie, aes(x = data, y = preco_m2)) +
  geom_line() +
  labs(title = 'Preço médio do m2 (construção civil)',
       x = 'mês/ano',
       y = 'Preço') +
  theme_bw()

mts <- ts(serie$preco_m2, frequency = 12, start = c(1994,7)) #Transformando em ts 
mts_SMA_12 <- SMA(mts, n=12) # "Manualmente" calculando tendência con n = nº de meses num ano
add_mts <- decompose(mts,type = "additive") # Duas decomposições diferentes
mult_mts <- decompose(mts,type = "multiplicative")

periodogram(mts) # Periodograma sem remover tendência 
periodograma <- data.frame(freq = 0:143/144, periodograma = periodogram(na.omit(mts - add_mts$trend),plot = F)$spec)
1/periodograma$freq[which.max(periodograma$periodograma)] #aproximadamente 6 meses o melhor candidato pra frequência de acordo com o periodograma (apesar de eu achar que não é muito válido)
