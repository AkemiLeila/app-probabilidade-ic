validar_parametros_distribuicao <- function(input) {
  
  dist <- input$dist
  
  validate(
    
   
    need(dist != "Beta" || #lógica de "ou não estamos na dist = beta ou vale as condições a seguir
           (input$alpha_beta > 0 && input$beta_beta > 0),
         "⚠️ Na Beta, α e β devem ser positivos."),
    
  
    need(dist != "Binomial" ||
           (input$n_bin >= 0 &&
              input$p_bin >= 0 && input$p_bin <= 1),
         "⚠️ Na Binomial, n ≥ 0 e 0 ≤ p ≤ 1."),
    
 
    need(dist != "Binomial Negativa" ||
           (input$r_nbin > 0 &&
              input$p_nbin > 0 && input$p_nbin <= 1),
         "⚠️ Na Binomial Negativa, r > 0 e 0 < p ≤ 1."),
    
    
    need(dist != "Cauchy" ||
           input$lambda_cauchy > 0,
         "⚠️ Na Cauchy, o parâmetro de escala λ deve ser positivo."),
    
    
    need(dist != "Exponencial" ||
           input$lambda_exp > 0,
         "⚠️ Na Exponencial, λ deve ser positivo."),
    
   
    need(dist != "Gama" ||
           (input$alpha_gama > 0 && input$beta_gama > 0),
         "⚠️ Na Gama, α e β devem ser positivos."),
    
   
    need(dist != "Geométrica" ||
           (input$p_geo > 0 && input$p_geo <= 1),
         "⚠️ Na Geométrica, 0 < p ≤ 1."),
    
   
    need(dist != "Hipergeométrica" ||
           (input$N_hip > 0 &&
              input$K_hip >= 0 &&
              input$nn_hip >= 0 &&
              input$K_hip <= input$N_hip &&
              input$nn_hip <= input$N_hip),
         "⚠️ Na Hipergeométrica, 0 ≤ K ≤ N, 0 ≤ n ≤ N, N > 0."),
    
    
    need(dist != "Normal" ||
           input$sigma2_norm > 0,
         "⚠️ Na Normal, σ² deve ser positivo."),
    
    need(dist != "Poisson" ||
           input$lambda_pois > 0,
         "⚠️ Na Poisson, λ deve ser positivo."),
    
    
    need(dist != "Qui-quadrado" ||
           input$v_chi > 0,
         "⚠️ Na Qui-quadrado, ν deve ser positivo."),
    
    
    need(dist != "T-Student" ||
           input$v_t > 0,
         "⚠️ Na t-Student, ν deve ser positivo."),
    
   
    need(dist != "Uniforme" ||
           input$min_unif < input$max_unif,
         "⚠️ Na Uniforme, a deve ser menor que b.")
    
  )
}
