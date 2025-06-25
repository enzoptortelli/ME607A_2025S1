retorno <- c(NA, diff(serie$preco_m2)) / lag(serie$preco_m2)

retorno <- tsibble(data = serie$data, valor = retorno)
retorno <- retorno %>% filter(!is.na(valor))
plot_retorno <- ggplot(retorno, aes(x = data, y = valor)) +
  geom_line() +
  labs(title = 'Retorno do preço médio do m² ajustado pela inflação (Construção civil) (Estado de SP)',
       x = 'Mês/ano',
       y = 'Retorno') +
  theme_bw()

plot_acf_retorno <- ACF(retorno, y = valor) %>% autoplot() +
  labs(title = 'ACF do retorno') +
  theme_bw()

plot_acf_retorno_quadrado <- ACF(retorno, y = valor^2) %>%autoplot() +
  labs(title = 'AFC do retorno²')+
  theme_bw()



#GARCH
spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   distribution = 'norm')

fit_garch <-  ugarchfit(spec, retorno$valor, solver = 'hybrid')

e_hat = fit_garch@fit$residuals/fit_garch@fit$sigma
acf(e_hat)
acf(e_hat^2)