modelo_drift <- serie %>% model(drift = RW(preco_m2 ~ drift()))
plot_res_modelo_drift <- gg_tsresiduals(modelo_drift) + labs(title = 'Análise residual modelo DRIFT')
