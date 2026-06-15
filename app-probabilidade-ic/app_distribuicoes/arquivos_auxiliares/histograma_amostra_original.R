histograma_amostra_original <- function(input, dados) {
  
  if (is.null(dados)) return(NULL)
  
  df <- data.frame(x = dados)



if (input$dist == "Beta") {
  media_amostral <- mean(df$x)
  
  ggplot(df, aes(x = x)) +
    
    # HISTOGRAMA PADRÃO
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 30,
      fill = cor_histograma,
      color = cor_borda_hist,
      alpha = 0.25
    ) +
    
    # CURVA TEÓRICA
    stat_function(
      fun = dbeta,
      args = list(shape1 = input$alpha_beta,
                  shape2 = input$beta_beta),
      color = cor_densidade_teorica,
      linewidth = 0.8
    ) +
    
    # MÉDIA
    geom_vline(
      xintercept = media_amostral,
      color = cor_media,
      linetype = "dashed",
      linewidth = 0.7
    ) +
    
    labs(
      title = paste("Amostra Beta (α =", input$alpha_beta,
                    ", β =", input$beta_beta, ")"),
      subtitle = "Cinza: amostra | Azul: densidade teórica | Verde: média",
      x = "Valores",
      y = "Densidade"
    ) +
    
    coord_cartesian(xlim = c(0, 1)) +
    
    tema_base_graficos
  
  
} else if (input$dist == "Binomial") {
  
  x_valores <- 0:input$n_bin
  
  prob <- dbinom(
    x_valores,
    size = input$n_bin,
    prob = input$p_bin
  )
  
  media_amostral <- mean(df$x)
  
  df_fp <- data.frame(
    k = x_valores,
    freq_esperada = prob * length(df$x)
  )
  
  ggplot(df, aes(x = x)) +
    
    # HISTOGRAMA PADRÃO
    geom_histogram(
      binwidth = 1,
      fill = cor_histograma,
      color = cor_borda_hist,
      alpha = 0.25
    ) +
    
    # PONTOS (probabilidade esperada)
    geom_point(
      data = df_fp,
      aes(x = k, y = freq_esperada),
      color = cor_densidade_teorica,
      size = 2
    ) +
    
    # MÉDIA
    geom_vline(
      xintercept = media_amostral,
      color = cor_media,
      linetype = "dashed",
      linewidth = 0.7
    ) +
    
    labs(
      title = paste(
        "Amostra Binomial (n =",
        input$n_bin,
        ", p =",
        input$p_bin,
        ")"
      ),
      subtitle = "Cinza: amostra | Azul: probabilidade esperada | Verde: média",
      x = "Valores",
      y = "Frequência"
    ) +
    
    tema_base_graficos

 
} else if (input$dist == "Binomial Negativa") {
  x_valores <- seq(0, max(df$x), by = 1)
  funcao_probabilidade_val <- dnbinom(x_valores,  size = input$r_nbin, prob = input$p_nbin)
  media_amostral <- mean(df$x)
  df_fp <- data.frame(k = x_valores, freq_esperada = funcao_probabilidade_val * length(df$x))
  ggplot(df, aes(x = x)) +
        geom_histogram(binwidth = 1, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
        geom_point(data = df_fp, aes(x = k, y = freq_esperada), color = cor_densidade_teorica, size = 2) +
        geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
        labs( title = paste("Amostra única da Distribuição Binomial Negativa (r =", input$r_nbin, ", p =", input$p_nbin, ")"),
             subtitle = "Pontos azuis: número esperado de acordo com a distribuição Binomial Negativa",
             x = "Valores", y = "Frequência") +
        tema_base_graficos
  
  
} else if(input$dist == "Cauchy") {
  media_amostral <- mean(df$x)
  mediana_amostral <- median(df$x)
  
  ggplot(df, aes(x = x)) +
        geom_histogram( aes(y = after_stat(density)), bins = 30, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
        stat_function(fun = dcauchy, args = list(location = input$theta_cauchy, scale = input$lambda_cauchy), color = cor_densidade_teorica,linewidth = 0.7) +
        geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
        geom_vline(xintercept = mediana_amostral, color = "purple", linetype = "dotted", linewidth = 0.9) +
        labs( title = paste("Amostra única da Distribuição de Cauchy (θ  =", input$theta_cauchy, ", λ =", input$lambda_cauchy,")"),
              subtitle = "Linha tracejada verde: média amostral (instável) | Linha pontilhada roxa: mediana amostral | Curva azul: densidade teórica da distribuição Cauchy",
              x = "Valores", y = "Densidade") +
        tema_base_graficos

  
} else if (input$dist == "Exponencial") {
  media_amostral <- mean(df$x)
  ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, boundary = 0, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
        stat_function(fun = dexp, args = list(rate = input$lambda_exp), color = cor_densidade_teorica, linewidth = 0.7) +
        geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed",linewidth = 0.7) +
        labs(title = paste("Amostra única da Distribuição Exponencial (λ =", input$lambda_exp, ")"),
             subtitle = "Linha tracejada: média amostral | Curva azul: densidade teórica da distribuição Exponencial",
              x = "Valores", y = "Densidade") +
        coord_cartesian(xlim = c(0, NA)) +
        tema_base_graficos
  
 
} else if (input$dist == "Gama") {
  media_amostral <- mean(df$x)
  ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gama, rate = input$beta_gama), color = cor_densidade_teorica, linewidth = 0.7) +
        geom_vline( xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
        labs(title = paste("Amostra única da Distribuição Gama (α =", input$alpha_gama, ", β =", input$beta_gama, ")"),
             subtitle = "Linha tracejada: média amostral | Curva azul: densidade teórica da distribuição Gama",
             x = "Valores", y = "Densidade") +
        coord_cartesian(xlim = c(0, NA)) +
        tema_base_graficos

  
}  else if (input$dist == "Geométrica") {
  x_valores <- seq(0, max(df$x), by = 1)
  funcao_probabilidade_val <- dgeom(x_valores, prob = input$p_geo)
  media_amostral <- mean(df$x)
  df_fp <- data.frame(k = x_valores, freq_esperada = funcao_probabilidade_val * length(df$x))
  ggplot(df, aes(x = x)) +
       geom_histogram(binwidth = 1, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
       geom_point(data = df_fp, aes(x = k, y = freq_esperada), color = cor_densidade_teorica, size = 2) +
       geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
       labs(title = paste("Amostra única da Distribuição Geométrica (p =", input$p_geo, ")"),
            subtitle = "Pontos azuis: número esperado de acordo com a distribuição Geométrica",
            x = "Valores", y = "Frequência") +
       tema_base_graficos
  
  
} else if (input$dist == "Hipergeométrica") {
  x_min <- max(0, input$nn_hip - (input$N_hip - input$K_hip))
  x_max <- min(input$nn_hip, input$K_hip)
  x_valores <- seq(x_min, x_max, by = 1)
  funcao_probabilidade_val <- dhyper(x_valores, m = input$K_hip, n = input$N_hip - input$K_hip, k = input$nn_hip)
  media_amostral <- mean(df$x)
  df_fp <- data.frame(k = x_valores, freq_esperada = funcao_probabilidade_val * length(df$x))
  ggplot(df, aes(x = x)) +
        geom_histogram(binwidth = 1, fill = cor_histograma, color = cor_borda_hist, alpha = 0.6) +
        geom_point(data = df_fp, aes(x = k, y = freq_esperada), color = cor_densidade_teorica, size = 2) +
        geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
        labs(title = paste("Amostra única da Distribuição Hipergeométrica"),
            subtitle = "Pontos azuis: número esperado de acordo com a distribuição Hipergeométrica",
            x = "Valores", y = "Frequência") +
        tema_base_graficos

  
} else if (input$dist == "Normal") {
  media_amostral <- mean(df$x)
  ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
        stat_function(fun = dnorm, args = list(mean = input$mi_norm, sd = sqrt(input$sigma2_norm)), color = cor_densidade_teorica, linewidth = 0.7) +
        geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
        labs(title = paste("Amostra única da Distribuição Normal (μ =", input$mi_norm, ", σ =", input$sigma2_norm, ")"),
             subtitle = "Linha tracejada: média amostral|Curva azul: densidade teórica da distribuição Normal",
             x = "Valores", y = "Densidade") +
        tema_base_graficos
  

} else if (input$dist == "Poisson") {
  x_valores <- seq(min(df$x), max(df$x), by = 1)
  lambda <- input$lambda_pois
  funcao_probabilidade_val <- dpois(x_valores, lambda)
  media_amostral <- mean(df$x)
  df_fp <- data.frame(k = x_valores, freq_esperada = funcao_probabilidade_val * length(df$x))
  ggplot(df, aes(x = x)) +
        geom_histogram(binwidth = 1, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
        geom_point(data = df_fp, aes(x = k, y = freq_esperada), color = cor_densidade_teorica, size = 2) +
        geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
        labs(title = paste("Amostra única da Distribuição Poisson (λ =", input$lambda_pois, ")"),
             subtitle = "Histograma da frequência observada + Nº esperado de acordo com a distribuição (pontos)",
             x = "Valores", y = "Frequência") +
        tema_base_graficos
  
  
} else if (input$dist == "Qui-quadrado") {
  media_amostral <- mean(df$x)
  ggplot(df, aes(x = x)) +
         geom_histogram(aes(y = after_stat(density)), bins = 30, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
         stat_function(fun = dchisq, args = list(df = input$v_chi), color = cor_densidade_teorica, linewidth = 0.7) +
         geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
         labs(title = paste("Amostra única da Distribuição Qui-quadrado (ν =", input$v_chi, ")"),
              subtitle = "Linha tracejada: média amostral| Curva azul: densidade teórica Qui-quadrado",
              x = "Valores", y = "Densidade") +
         coord_cartesian(xlim = c(0, NA)) +
         tema_base_graficos
  
  
} else if (input$dist == "T-Student") {
  media_amostral <- mean(df$x)
  ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
      stat_function(fun = dt, args = list(df = input$v_t), color = cor_densidade_teorica, linewidth = 0.7) +
      geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
      labs( title = paste("Amostra única da Distribuição t-Student (ν =",input$v_t, ")"),
            subtitle = "Linha tracejada: média amostral | Curva azul: densidade teórica t-Student",
            x = "Valores", y = "Densidade") +
      tema_base_graficos
  
} else if (input$dist == "Uniforme") {
  media_amostral <- mean(df$x)
  ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = (input$max_unif - input$min_unif) / 30,
                     boundary = input$min_unif, closed = "left", fill = cor_histograma, color = cor_borda_hist, alpha = 0.25) +
      stat_function(fun = dunif, args = list(min = input$min_unif, max = input$max_unif),color = cor_densidade_teorica, linewidth = 0.7) +
      geom_vline(xintercept = media_amostral, color = cor_media, linetype = "dashed", linewidth = 0.7) +
      labs(title = paste("Amostra única da Distribuição Uniforme (", input$min_unif, ",", input$max_unif, ")"),
          subtitle = "Linha tracejada: média amostral|Curva azul: densidade teórica Uniforme ",
          x = "Valores", y = "Densidade") +
      coord_cartesian(xlim = c(input$min_unif, input$max_unif)) +
      tema_base_graficos
}




}