
estatisticas_teoricas <- function(dist, input) {
  req(dist)
  
  if (dist == "Beta") {
    req(input$alpha_beta, input$beta_beta)
    alpha <- input$alpha_beta
    beta <- input$beta_beta
    media <- alpha / (alpha + beta)
    variancia <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
    moda <- ifelse(alpha > 1 & beta > 1, (alpha - 1) / (alpha + beta - 2), NA)
    mediana <- qbeta(0.5, alpha, beta)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Valor esperado da distribuição Beta.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição Beta.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Existe apenas se α > 1 e β > 1.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "Binomial") {
    req(input$n_bin, input$p_bin)
    n <- input$n_bin
    p <- input$p_bin
    media <- n * p
    variancia <- n * p * (1 - p)
    moda <- floor((n + 1) * p)
    mediana <- qbinom(0.5, size = n, prob = p)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
 
  } else if (dist == "Binomial Negativa") {
    req(input$r_nbin, input$p_nbin)
    r <- input$r_nbin
    p <- input$p_nbin
    
    media <- r * (1 - p) / p
    variancia <- r * (1 - p) / p^2
    moda <- ifelse(r > 1, floor((r - 1) * (1 - p) / p), 0)
    mediana <- qnbinom(0.5, size = r, prob = p)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "Cauchy") {
    req(input$theta_cauchy)
    theta <- input$theta_cauchy
    
    media <- NA
    variancia <- NA
    moda <- theta
    mediana <- qcauchy(0.5, location = theta)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "Exponencial") {
    req(input$lambda_exp)
    lambda <- input$lambda_exp
    
    media <- 1 / lambda
    variancia <- 1 / lambda^2
    moda <- 0
    mediana <- qexp(0.5, rate = lambda)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "Gama") {
    req(input$alpha_gama, input$beta_gama)
    alpha <- input$alpha_gama
    beta <- input$beta_gama
    
    media <- alpha / beta
    variancia <- alpha / beta^2
    moda <- ifelse(alpha >= 1, (alpha - 1) / beta, NA)
    mediana <- qgamma(0.5, shape = alpha, rate = beta)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
    
  } else if (dist == "Geométrica") {
    req(input$p_geo)
    p <- input$p_geo
    
    media <- (1 - p) / p
    variancia <- (1 - p) / p^2
    moda <- 0
    mediana <- qgeom(0.5, prob = p)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
  
  } else if (dist == "Hipergeométrica") {
    req(input$N_hip, input$K_hip, input$nn_hip)
    N <- input$N_hip #população
    K <- input$K_hip # sucessos
    k <- input$nn_hip #numero de tiradas
    
    media <- k * K / N
    variancia <- k * (K / N) * (1 - K / N) * ((N - k) / (N - 1)) # k * p * (1-p) * [(N-k)/(N-1)], onde p = K/N
    moda <- floor((k + 1) * (K + 1) / (N + 2))
    mediana <- qhyper(0.5, m = K, n=  N - K, k = k)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "Normal") {
    req(input$mi_norm, input$sigma2_norm)
    mu <- input$mi_norm
    sigma2 <- input$sigma2_norm
    sd <- sqrt(sigma2)
    
    media <- mu
    variancia <- sigma2
    moda <- mu
    mediana <- qnorm(0.5, mean = mu, sd = sd)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "Poisson") {
    req(input$lambda_pois)
    lambda <- input$lambda_pois
    
    media <- lambda
    variancia <- lambda
    moda <- floor(lambda)
    mediana <- qpois(0.5, lambda)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'> Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'>️ Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "Qui-quadrado") {
    req(input$v_chi)
    v <- input$v_chi
    
    media <- v
    variancia <- 2 * v
    moda <- max(v - 2, 0)
    mediana <- qchisq(0.5, df = v)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'> Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'>️ Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "T-Student") {
    req(input$v_t)
    v <- input$v_t
    
    media <- ifelse(v > 1, 0, NA)
    variancia <- ifelse(v > 2, v / (v - 2), NA)
    moda <- 0
    mediana <- qt(0.5, df = v)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'> Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'>️ Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else if (dist == "Uniforme") {
    req(input$min_unif, input$max_unif)
    a <- input$min_unif
    b <- input$max_unif
    
    media <- (a + b) / 2
    variancia <- (b - a)^2 / 12
    moda <- NA
    mediana <- qunif(0.5, min = a, max = b)
    
    HTML(paste0(
      "<table class='table table-bordered'>",
      "<tr><th>Estatística</th><th>Valor</th></tr>",
      "<tr><td><span title='Esperança matemática da distribuição.' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
      "<tr><td><span title='Variância teórica da distribuição.' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
      "<tr><td><span title='Valor mais provável da distribuição.' style='color:#2F5D50;'> Moda</span></td><td>", round(moda,4), "</td></tr>",
      "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'>️ Mediana</span></td><td>", round(mediana,4), "</td></tr>",
      "</table>"
    ))
    
  } else {
    NULL
  }
}
