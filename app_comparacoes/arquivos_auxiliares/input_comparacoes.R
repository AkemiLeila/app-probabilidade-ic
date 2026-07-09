input_comparacoes <- function(id){
  
  
  
  tagList(
    
    selectInput(NS(id,"tipo_comp"), "Compare:",  choices = c("", 
                                                             "Beta x Uniforme", 
                                                             "Binomial x Poisson", 
                                                             "Exponencial x Gama", 
                                                             "Normal x Cauchy x T-Student", 
                                                             "Normal x Qui-quadrado",
                                                             "Qui-quadrado x Gama x Exponencial")),
    
    conditionalPanel(
      paste0("input['", NS(id, "tipo_comp"), "'] == 'Beta x Uniforme'"),
      numericInput(NS(id,"alpha_beta"), "Parâmetro Beta α (forma):", value = 1, min = 0.1, step = 0.1),
      numericInput(NS(id,"beta_beta"), "Parâmetro Beta β (forma):", value = 1, min = 0.1, step = 0.1),
      numericInput(NS(id,"min_unif"), "Limite inferior Uniforme (a):", value = 0, step = 0.1),
      numericInput(NS(id,"max_unif"), "Limite superior Uniforme (b):", value = 1, step = 0.1)
    ),
    
   
    
    conditionalPanel(
      paste0("input['", NS(id, "tipo_comp"), "'] == 'Normal x Cauchy x T-Student'"),
      numericInput((NS(id,"mi_norm")), "Parâmetro Normal μ (média):", value = 0),
      numericInput((NS(id,"sigma2_norm")), "Parâmetro Normal σ² (variância):", value = 1, min = 0),
      numericInput((NS(id,"theta_cauchy")), "Parâmetro Cauchy θ (posição):", value = 0, min = 0.1, step = 0.1),
      numericInput((NS(id,"lambda_cauchy")), "Parâmetro Cauchy λ (escala):", value = 1, min = 0.1, step = 0.1),
      numericInput(NS(id,"v_t"), "Parâmetro T-Student ν (graus de liberdade):", value = 5,  min = 1, step = 1 )
    ),
    
    conditionalPanel(
      paste0("input['", NS(id, "tipo_comp"), "'] == 'Binomial x Poisson'"),
      numericInput((NS(id,"n_bin")), "Parâmetro Binomial n (número de ensaios):", value = 15, min = 1, step = 1),
      numericInput((NS(id,"lambda_pois")), "Parâmetro λ da Poisson (taxa):", value = 3, min = 0.01, step = 0.01)
    ),
    
    
    conditionalPanel(
      paste0("input['", NS(id, "tipo_comp"), "'] == 'Exponencial x Gama'"),
      numericInput((NS(id,"lambda_exp")), "Parâmetro Exponencial λ:", value = 1, min = 0.1, step = 0.1),
      numericInput((NS(id,"alpha_gama")), "Parâmetro Gama α (forma):", value = 1, min = 0.1, step = 0.1),
      numericInput((NS(id,"beta_gama")), "Parâmetro Gama β (taxa):", value = 1, min = 0.1, step = 0.1)
      
    ),
    
    conditionalPanel(
      paste0("input['", NS(id, "tipo_comp"), "'] == 'Normal x Qui-quadrado'"),
      numericInput((NS(id,"v_chi")), "Graus de liberdade (ν):", value = 1, min = 1)
      
    ),
    
    conditionalPanel(
      paste0("input['", NS(id, "tipo_comp"), "'] == 'Qui-quadrado x Gama x Exponencial'"),
      numericInput( NS(id, "v_chi_2"),"Graus de liberdade da Qui-Quadrado (k):",value = 3, min = 1,  step = 0.1),
      helpText("Relações utilizadas:"," α = k/2, β = 1/2 e λ = 1/2" )
      
    )
    
  )
  
  
  
}