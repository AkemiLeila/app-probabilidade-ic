
entradas_distribuicoes <- function(id){
  
  tagList(
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Beta'"),
    numericInput((NS(id,"alpha_beta")), "Parâmetro α (forma):", value = 5, min = 0.1, step = 0.1),
    numericInput((NS(id,"beta_beta")), "Parâmetro β (forma):", value = 5, min = 0.1, step = 0.1),
    numericInput((NS(id,"n_beta")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_beta")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Binomial'"),
    numericInput((NS(id,"n_bin")), "Número de ensaios (n parâmetro da Binomial):", value = 10, min = 1, step = 1),
    numericInput((NS(id,"p_bin")), "Probabilidade de sucesso (p):", value = 0.5, min = 0.01, max = 0.99, step = 0.01),
    numericInput((NS(id,"n_bsam")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_bin")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Binomial Negativa'"),
    numericInput((NS(id,"r_nbin")), "Número de sucessos (r):", value = 5, min = 1, step = 1),
    numericInput((NS(id,"p_nbin")), "Probabilidade de sucesso (p):", value = 0.5, min = 0.01, max = 0.99, step = 0.01),
    numericInput((NS(id,"n_nbin")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_nbin")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Cauchy'"),
    numericInput((NS(id,"theta_cauchy")), "Digite o valor de θ (posição):", value = 0, min = 0.1, step = 0.1),
    numericInput((NS(id,"lambda_cauchy")), "Digite o valor de λ (escala):", value = 1, min = 0.1, step = 0.1),
    numericInput((NS(id,"n_cauchy")), "Tamanho de cada amostra (n):", value = 20, min = 1),
    numericInput((NS(id,"S_cauchy")), "Número de amostras (S):", value = 1000, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Exponencial'"),
    numericInput((NS(id,"lambda_exp")), "Digite o valor de λ:", value = 0.5, min = 0.1, step = 0.1),
    numericInput((NS(id,"n_exp")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_exp")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Gama'"),
    numericInput((NS(id,"alpha_gama")), "Parâmetro de forma (α):", value = 2, min = 0.1, step = 0.1),
    numericInput((NS(id,"beta_gama")), "Parâmetro de taxa (β):", value = 2, min = 0.1, step = 0.1),
    numericInput((NS(id,"n_gama")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_gama")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Geométrica'"),
    numericInput((NS(id,"p_geo")), "Probabilidade de sucesso (p):", value = 0.3, min = 0.01, max = 0.99, step = 0.01),
    numericInput((NS(id,"n_geo")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_geo")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Hipergeométrica'"),
    numericInput((NS(id,"K_hip")), "Número de sucessos na população (K):", value = 20, min = 0),
    numericInput((NS(id,"N_hip")), "Tamanho da população (N):", value = 50, min = 1),
    numericInput((NS(id,"nn_hip")), "Tamanho da amostra Hipergeométrica (k):", value = 25, min = 1),
    numericInput((NS(id,"n_hip")), "Tamanho da amostra (n):", value = 1000, min = 0),
    numericInput((NS(id,"S_hip")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Normal'"),
    numericInput((NS(id,"mi_norm")), "Digite um valor para μ (média):", value = 0),
    numericInput((NS(id,"sigma2_norm")), "Digite um valor para σ² (variância):", value = 1, min = 0),
    numericInput((NS(id,"n_norm")), "Tamanho de cada amostra (n):", value = 1000),
    numericInput((NS(id,"S_norm")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Poisson'"),
    numericInput((NS(id,"lambda_pois")), "Digite o valor de λ:", value = 5, min = 0.1, step = 0.1),
    numericInput((NS(id,"n_pois")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_pois")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Qui-quadrado'"),
    numericInput((NS(id,"v_chi")), "Graus de liberdade (ν):", value = 5, min = 1),
    numericInput((NS(id,"n_chi")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_chi")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'T-Student'"),
    numericInput((NS(id,"v_t")), "Graus de liberdade (ν):", value = 5, min = 1),
    numericInput((NS(id,"n_t")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_t")), "Número de amostras (S):", value = 100, min = 1)
  ),
  
  conditionalPanel(
    condition = paste0("input['", (NS(id,"dist")), "'] == 'Uniforme'"),
    numericInput((NS(id,"min_unif")), "Limite inferior (a):", value = 0),
    numericInput((NS(id,"max_unif")), "Limite superior (b):", value = 1),
    numericInput((NS(id,"n_unif")), "Tamanho de cada amostra (n):", value = 1000, min = 1),
    numericInput((NS(id,"S_unif")), "Número de amostras (S):", value = 100, min = 1)
  )
)

}
