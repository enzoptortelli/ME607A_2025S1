library(tidyverse)
library(tsibble)
library(fpp3)
library(astsa)

serie <- read_tsv('dados/s1.tsv',
                  locale = locale(decimal_mark = ',')) %>%
  pivot_longer(!UF, names_to = 'data', values_to = 'preco_m2') %>%
  select(!UF) %>%
  mutate(data = seq(from = ym("1986-03"),
                    to = ym("2018-06"),
                    by = "month")) %>%
  filter(data >= ym('1994-07')) %>%
  as_tsibble(index = data)


ggplot(serie, aes(x = data, y = preco_m2)) +
  geom_line() +
  labs(title = 'Preço médio do m2 (construção civil)',
       x = 'mês/ano',
       y = 'Preço') +
  theme_bw()

# decomp <- serie %>%
#   model(STL(preco_m2 ~ season(window = 12))) %>%
#   components()
# 
# autoplot(decomp)