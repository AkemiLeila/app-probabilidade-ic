gera_amostra_original <- function(dist, input) {
  
   if (is.null(dist) || dist == "") return(NULL)
  
    if(dist == "Beta") {
    req(input$n_beta, input$alpha_beta, input$beta_beta)
    rbeta(input$n_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta)
  } else if (dist == "Binomial") {
    req(input$n_bsam, input$n_bin, input$p_bin)
    rbinom(input$n_bsam, size = input$n_bin, prob = input$p_bin)
  } else if(dist == "Binomial Negativa"){
    req(input$n_nbin, input$r_nbin, input$p_nbin)
    rnbinom(input$n_nbin, size = input$r_nbin, prob = input$p_nbin)
  } else if (dist == "Cauchy") {
    req(input$n_cauchy, input$theta_cauchy, input$lambda_cauchy)
    rcauchy(input$n_cauchy, location = input$theta_cauchy, scale = input$lambda_cauchy)
  } else if (dist == "Exponencial") {
    req(input$n_exp, input$lambda_exp)
    rexp(input$n_exp, rate = input$lambda_exp)
  } else if(dist == "Gama") {
    req(input$n_gama, input$alpha_gama, input$beta_gama)
    rgamma(input$n_gama, shape = input$alpha_gama, rate = input$beta_gama)
  } else if(dist == "Geométrica"){
    req(input$n_geo, input$p_geo)
    rgeom(input$n_geo, prob = input$p_geo)
  } else if(dist == "Hipergeométrica"){
    req(input$n_hip, input$K_hip, input$N_hip, input$nn_hip)
    req(input$N_hip >= input$K_hip)
    req(input$nn_hip <= input$N_hip)
    rhyper(nn = input$n_hip , m = input$K_hip, n = input$N_hip - input$K_hip, k = input$nn_hip )
  } else if (dist == "Normal") {
    req(input$n_norm, input$mi_norm, input$sigma2_norm)
    rnorm(input$n_norm, mean = input$mi_norm, sd = sqrt(input$sigma2_norm))
  } else if(dist == "Poisson") {
    req(input$n_pois, input$lambda_pois)
    rpois(input$n_pois, input$lambda_pois)
  } else if(dist == "Qui-quadrado"){
    req(input$n_chi, input$v_chi)
    rchisq(input$n_chi, df = input$v_chi)
  } else if(dist == "T-Student"){
    req(input$n_t, input$v_t)
    rt(input$n_t, df = input$v_t)
  } else if(dist == "Uniforme"){
    req(input$n_unif, input$min_unif, input$max_unif)
    runif(input$n_unif, min = input$min_unif, max = input$max_unif)
  } else {
    NULL
  }
}