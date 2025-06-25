modelo_naive <- serie %>% model(naive = NAIVE(preco_m2))
plot_res_modelo_naive <- gg_tsresiduals(modelo_naive) + labs(title = 'Análise residual modelo NAIVE')