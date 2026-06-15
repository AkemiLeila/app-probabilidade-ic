calculo_z_score <- function(input, amostra_original = NULL) {
  
  dist <- input$dist
  
  if (input$dist == "Beta") {
    req(input$n_beta, input$S_beta, input$alpha_beta, input$beta_beta)
    n <- input$n_beta
    S <- input$S_beta
    alpha <- input$alpha_beta
    beta <- input$beta_beta
    
    #momentos teóricos
    mu <- (alpha / (alpha + beta))
    sigma2 <-  ((alpha * beta) /(((alpha + beta) ^ 2) * ( alpha + beta + 1)))
    
    #simulação TCL
    #gera amostra tamanho n
    X <- replicate(S, rbeta(n, shape1 = alpha, shape2 = beta))
    #medias amostrais
    x_barra <- colMeans(X)
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2 / n)
    
    return(list(dist = "Beta", param = list(alpha = alpha, beta = beta),
               mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra,  z = z))
    

  }  else if (input$dist == "Binomial") {
    req(input$n_bsam, input$S_bin, input$n_bin, input$p_bin)
    
    n <- input$n_bsam #tamamno da amostra
    S <- input$S_bin
    n_bin <- input$n_bin #numero de ensaios bernoulli
    p <- input$p_bin
   
    #momentos teoricos
    mu <- n_bin * p
    sigma2 <- n_bin*p*(1-p)
    #SImulação TCL
    #gera amostra tamanho n
    X <- replicate(S, rbinom(n, size = n_bin, prob = p))
    #medias amostrais
    x_barra <- colMeans(X)
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2 / n)
    
    return(list(dist = "Binomial", param = list(n_bin = n_bin, p = p),
                mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z))
    
    
  } else if (input$dist == "Binomial Negativa"){
    req(input$n_nbin, input$S_nbin, input$r_nbin, input$p_nbin)
    
    n <- input$n_nbin
    S <- input$S_nbin
    r <- input$r_nbin
    p <- input$p_nbin
    
    #momentos teóricos
    mu <- (r * (1 - p)) / p
    sigma2 <- (r * (1 - p)) / p^2
    
    #Simulação TCL
    #amostras replicadas S vezes de tamanho n
    X <- replicate(S, rnbinom(n, size = r, prob =  p))
    #medias amostrais
    x_barra <- colMeans(X)
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2 / n)
    
    return(list(dist = "Binomial Negativa", param = list(r = r, p = p),
                mu = mu,  sigma2 = sigma2, n = n,  S = S, medias = x_barra, z = z ))
    
    
  } else if (input$dist == "Exponencial") {
    req(input$n_exp, input$S_exp, input$lambda_exp)
    
    n <- input$n_exp
    S <- input$S_exp
    lambda_val <- input$lambda_exp
  
    #momentos teoricos
    mu <- 1 / lambda_val
    sigma2 <- 1 / lambda_val^2
    #Simulação TCL
    #geração de amostra tamanho n replicada S vezes
    X <- replicate(S, rexp(n, rate = lambda_val))
    #medias amostrais
    x_barra <- colMeans(X)
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2 / n)
    
    
    return(list( dist = "Exponencial",  param  = list(lambda = lambda_val),
                 mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z))
    
  } else if (input$dist == "Gama"){
    req(input$n_gama, input$S_gama, input$alpha_gama, input$beta_gama)
    
    n <- input$n_gama
    S <- input$S_gama
    alpha <- input$alpha_gama
    beta <- input$beta_gama
   
    #momento teóricos
    mu <- alpha / beta
    sigma2 <- alpha / beta^2
    #simulação da TCL
    #geração de amostra tamanho n replicada S vezes
    X <- replicate(S, rgamma(n, shape = alpha, rate = beta))
    #medias amostrais
    x_barra <- colMeans(X)
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2 / n)
    
    return(list( dist = "Gama",  param  = list(alpha = alpha, beta = beta),
                 mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z))
    
    
  } else if (input$dist == "Geométrica") {
    req(input$n_geo, input$S_geo, input$p_geo)
    
    n <- input$n_geo
    S <- input$S_geo
    p <- input$p_geo
    
    #momento teóricos
    mu <- (1 - p) / p
    sigma2 <- (1 - p) / p^2
    #simulação da TCL
    #geração de amostra tamanho n replicada S vezes
    X <- replicate(S, rgeom(n, prob = p))
    #medias amostrais
    x_barra <- colMeans(X)
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2 / n)
    
    return(list( dist = "Geométrica",  param  = list(p = p),
                 mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z))
    
 
  } else if (input$dist == "Hipergeométrica"){
    req(input$n_hip, input$S_hip, input$K_hip, input$N_hip, input$nn_hip)
    
    n_amostra <- input$n_hip   # tamanho da amostra simulada
    S <- input$S_hip           # número de amostras
    K <- input$K_hip           # sucessos na população
    N <- input$N_hip           # tamanho da população
    k <- input$nn_hip          # número de retiradas
     
     #momento teóricos
    mu <- k * (K/N)
    sigma2 <- k * (K/N) * (1 - (K/N)) * ((N - k) / (N - 1))
     #simulação da TCL
     #geração de amostra tamanho n replicada S vezes
    X <- replicate(S, rhyper(nn = n_amostra, m = K, n = N - K, k = k))
    
     #medias amostrais
     x_barra <- colMeans(X)
     #normalização
     z <- (x_barra - mu) / sqrt(sigma2 / n_amostra)
     
     return(list( dist = "Hipergeométrica",  param  = list(K = K, N = N, k = k),
                  mu = mu, sigma2 = sigma2, n = n_amostra, S = S, medias = x_barra, z = z))
     
     
     
   } else if (input$dist == "Normal") {
     req(input$n_norm, input$S_norm, input$mi_norm, input$sigma2_norm)
     
      n <- input$n_norm 
      S <- input$S_norm
      mu <- input$mi_norm
      sigma2 <- input$sigma2_norm
      #silmulação da TCL
      X <- replicate(S, rnorm(n, mean = mu, sd = sqrt(sigma2)))
      #medias amostrais
      x_barra <- colMeans(X)
      #normalização
      z <- (x_barra - mu)/sqrt(sigma2/n)
      
      return(list(dist = "Normal", param = list(mu = mu, sigma2 = sigma2), 
                  mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z ))
      
  } else if (input$dist == "Poisson") {
    req(input$n_pois, input$S_pois, input$lambda_pois)
    
    #parametros comuns
    n <- input$n_pois
    S <- input$S_pois
    lambda <- input$lambda_pois
    #momentos teoricos
    mu <- lambda
    sigma2 <- lambda
    #Simulação TCL
    #gera amostra do tamanho n
    X <- replicate(S, rpois(n, lambda = lambda))
    #medias amostrais
    x_barra <- colMeans(X)  #faz media das colunas da matriz n x S
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2/ n)
    
    return(list(dist = "Poisson", param = list(lambda = lambda),
                mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z))
    
  } else if ( input$dist == "Qui-quadrado"){
    req(input$n_chi, input$S_chi, input$v_chi)
    
    #parametros comuns
    n <- input$n_chi
    S <- input$S_chi
    df <- input$v_chi
    #momentos teoricos
    mu <- df
    sigma2 <- 2 * df
    #Simulação TCL
    #gera amostra do tamanho n
    X <- replicate(S, rchisq(n, df = df, ncp = 0))
    #medias amostrais
    x_barra <- colMeans(X)  #faz media das colunas da matriz n x S
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2/ n)
    
    return(list(dist = "Qui-quadrado", param = list(df = df),
                mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z))
    
  } else if (input$dist == "T-Student"){
    req(input$n_t, input$S_t, input$v_t)
    req(input$v_t > 2)  # importante para variância existir
    
    #parametros comuns
    n <- input$n_t
    S <- input$S_t
    df <- input$v_t
   #momentos teoricos
    mu <- 0
    sigma2 <- df / (df - 2)
    #Simulação TCL
    #gera amostra do tamanho n
    X <- replicate(S, rt(n, df, ncp = 0))
    #medias amostrais
    x_barra <- colMeans(X)  #faz media das colunas da matriz n x S
    #normalização
    z <- (x_barra - mu) / sqrt(sigma2/ n)
  
    return(list(dist = "T-Student", param = list(df = df),
              mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z))
  
  
}  else if (input$dist == "Uniforme") {
  req(input$n_unif, input$S_unif, input$min_unif, input$max_unif)
  
   #parametros comuns
   n <- input$n_unif
   S <- input$S_unif
   a <- input$min_unif
   b <- input$max_unif
   #momentos teoricos
   mu <- (a + b) / 2
   sigma2 <- (b - a)^2 / 12
   #Simulação TCL
   #gera amostra do tamanho n
   X <- replicate(S, runif(n, min = a, max = b))
   #medias amostrais
   x_barra <- colMeans(X)  #faz media das colunas da matriz n x S
   #normalização
   z <- (x_barra - mu) / sqrt(sigma2/ n)
  
   return(list(dist = "Uniforme", param = list(a = a, b = b),
               mu = mu, sigma2 = sigma2, n = n, S = S, medias = x_barra, z = z))
  
  
} else {
  stop("Distribuição não reconhecida. Verifique input$dist.")
 }
}


