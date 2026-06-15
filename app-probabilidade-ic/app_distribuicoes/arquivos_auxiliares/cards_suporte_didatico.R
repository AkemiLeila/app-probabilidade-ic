cards_suporte_didatico <- function(dist, ns){
  
  conteudo <- switch(dist,
         
         "Beta" = tagList(

                h5("Distribuição Beta e o TCL"),
                
                exemplo_card(ns("exemplo_beta"),
                titulo = "TCL: aproximação pela Normal quando n é grande",
                descricao = "Mesmo sendo limitada ao intervalo [0, 1] e podendo assumir diferentes formas, a distribuição das médias amostrais converge para a Normal quando n é grande."),
                
                hr(),
                h5("Interpretação dos Parâmetros α e β:"),
                exemplo_card(ns("beta_peso_1"),
                  titulo = "α > β : Concentração próxima de 1",
                  descricao = "A massa de probabilidade se desloca para a direita. Ex: Sucesso muito provável."
                ),
                exemplo_card(ns( "beta_peso_0"),
                  titulo = "β > α : Concentração próxima de 0",
                  descricao = "A massa de probabilidade se desloca para a esquerda. Ex: Falha muito provável."
                ),
                exemplo_card(ns( "beta_concentrada"),
                  titulo = "α e β Grandes: Menor Incerteza",
                  descricao = "Parâmetros elevados tornam a distribuição mais 'estreita' e concentrada em torno da média.")
                
    ),
    
         "Binomial" = tagList(
    
                h5("Distribuição Binomial e o TCL"),
      
                 exemplo_card(ns( "exemplo_bin"),
                 titulo = "TCL: aproximação pela Normal quando n é grande",
                 descricao = "Apesar de discreta, a distribuição das médias amostrais da Binomial aproxima-se da Normal quando n é grande."),
                 hr(),
                 h5("Propriedades da Binomial:"),
                 exemplo_card(ns("bin_simetrica"),
                 titulo = "Simetria Perfeita (p = 0.5)",
                 descricao = "Quando a chance de sucesso e falha são iguais, a distribuição é perfeitamente simétrica em torno da média."
                 ),
                 exemplo_card(ns("bin_assimetrica_pos"),
                 titulo = "Assimetria Positiva (p pequeno)",
                 descricao = "Com p próximo de 0, a massa de probabilidade se concentra à esquerda, embora algum evento raro possa ser observado, o que justifica a cauda alongada. Ex: Ganhar na loteria."
    )
  ),
    
        "Binomial Negativa" = tagList(
          
                h5("Distribuição Binomial Negativa e o TCL"),
      
                exemplo_card(ns( "exemplo_nbin"),
                 titulo = "TCL: aproximação pela Normal quando n é grande",
                 descricao = "Mesmo com forte assimetria para certos parâmetros, a distribuição das médias amostrais converge para a Normal quando n é grande"),
              
                hr(),
                h5("Comportamento dos Parâmetros:"),
                exemplo_card(ns( "nbin_r_grande"),
                  titulo = "Aumento de r: Deslocamento e Simetria",
                  descricao = "À medida que precisamos de mais sucessos (r aumenta), o gráfico se desloca para a direita e começa a ganhar uma forma de 'sino' (Normal)."
                )
      ),
    
         "Cauchy" = tagList(
           
                h5("Propriedades da Cauchy"),
                exemplo_card(ns( "cauchy_vs_normal"),
                  titulo = "Cauchy vs. Normal: Caudas Pesadas",
                  descricao = "A Cauchy possui 'caudas pesadas'. Isso significa que valores extremos (outliers) são muito mais frequentes do que na Normal."
                ),
                hr(),
                h5("O problema da média (Instabilidade)"),
      
                exemplo_card(ns( "cauchy_media_inexistente"),
                  titulo = "O Paradoxo da Média",
                  descricao = "Diferente de outras distribuições, a média aritmética de uma amostra Cauchy não converge. Veja a dispersão dos dados simulados."
                )
    ),
    
          "Exponencial" = tagList(
            
                h5("Distribuição Exponencial e o TCL"),
                exemplo_card(ns( "exemplo_exp"), 
                titulo = "TCL: aproximação pela Normal quando n é grande",
                descricao = "A distribuição das médias amostrais se aproxima da Normal quando n é grande, mesmo partindo de uma distribuição fortemente assimétrica." ),
                hr(),
                h5("Lei dos Grandes Números"),
                exemplo_card(ns( "exemplo1_exp"), 
                             titulo = "Distribuição Original: efeito de n grande",
                             descricao = "Veja como n altera forma." ), 
                hr(),
                h5("Conexão e forma"),
                exemplo_card(ns("exp_impacto_lambda"),
                  titulo = "Taxa de Ocorrência (λ)",
                  descricao = "Veja como o aumento da taxa faz o tempo de espera diminuir, o que concentra a probabilidade perto do zero."
                )
                
    ),
    
          "Gama" = tagList(
            
                h5("Distribuição Gama eo TCL"),
                exemplo_card(ns( "exemplo_gama"),
                titulo = "TCL: aproximação pela Normal quando n é grande",
                descricao = "Distribuições assimétricas e contínuas também apresentam convergência das médias amostrais para a Normal quando n é grande"),
                hr(),
                h5("Flexibilidade de Formas:"),
                exemplo_card(ns( "gama_formato_sino"),
                  titulo = "Transição de Forma ",
                  descricao = "Quando alpha aumenta, a Gama assume um formato de sino assimétrico, deslocando-se para a direita."
                ),
                hr(),
                h5("Soma de exponenciais"),
                exemplo_card(ns("gama_soma_exp"),
                  titulo = "Soma de Tempos de Espera",
                  descricao = "A Gama representa o tempo total de espera para 'alpha' eventos ocorrerem."
                ),
                hr(),
                h5("Relação com a Qui-quadrado"),
                exemplo_card(ns("gama_qui_quadrado"),
                  titulo = "Caso Especial: Qui-Quadrado",
                  descricao = "Ao fixar parâmetros específicos, a Gama torna-se uma Qui-Quadrado."
                )
    ),
    
         "Geométrica" = tagList(
           
                h5("Distribuição Geométrica e o TCL"),
                exemplo_card(ns( "exemplo_geo"),
                titulo = "TCL: aproximação pela Normal quando n é grande",
                descricao = "Apesar de discreta e fortemente assimétrica, a distribuição das médias amostrais converge para a Normal quando n é grande"),
                hr(),
                h5("Naturexa do decaimento"),
                exemplo_card(ns( "geo_decaimento"),
                  titulo = "O Primeiro Sucesso",
                  descricao = "A probabilidade é máxima na primeira tentativa e diminui conforme precisamos de mais ensaios. "
                ),
                hr(),
                h5("Propriedades Fundamentais:"),
                exemplo_card(ns( "geo_impacto_p"),
                  titulo = "Dificuldade do Evento",
                  descricao = "Se o sucesso é difícil (p pequeno), o gráfico desce devagar, indicando que esperar muitos turnos é comum."
                )
    ),
    
    
           "Hipergeométrica" = tagList(
             
                h5("Distribuição Hipergeométrica e o TCL"),
                exemplo_card(ns( "exemplo_hip"),
                titulo = "TCL: aproximação pela Normal quando n é grande",
                descricao = "Mesmo sem independência entre as observações, a distribuição das médias amostrais aproxima-se da Normal quando n é grande"),
                hr(),
                h5("Conceitos e Aproximações:"),
                exemplo_card(ns( "hiper_dependencia"),
                  titulo = "Dependência: População Pequena",
                  descricao = "Com poucos itens, cada retirada altera drasticamente a chance da próxima. Os ensaios não são independentes."
                ),
                exemplo_card(ns( "hiper_approx_bin"),
                  titulo = "Aproximação pela Binomial (N grande)",
                  descricao = "Quando a população (N) é muito grande frente à amostra (n), a dependência torna-se desprezível e o gráfico se assemelha à Binomial."
                )
                
                
                
    ),
    
          "Normal" =  tagList(
                
                 h5("Distribuição Normal E O TCL"),
                 exemplo_card(ns( "exemplo_norm"), 
                 titulo = "TCL: aproximação pela Normal quando n é grande",
                 descricao = "A distribuição das médias amostrais é Normal para qualquer n, pois a população já segue uma Normal."),
                 hr(),
                 h5("Conceitos Fundamentais:"),
                 exemplo_card(ns("norm_regra_empirica"),
                   titulo = "Regra 68-95-99.7",
                   descricao = "Em uma Normal, quase todos os dados estão a 3 desvios padrão da média. Veja as áreas de probabilidade."
                 ),
                 exemplo_card(ns("norm_impacto_sigma"),
                   titulo = "Variabilidade e Achatamento",
                   descricao = "Um sigma pequeno torna a curva 'pontuda'; um sigma grande a torna 'achatada'. A área total sob a curva é sempre 1."
                 )
    ),
    
           "Poisson" = tagList(
             
                h5("Distribuição Poisson e o TCL"),
                exemplo_card(ns( "exemplo_pois"), 
                titulo = "TCL: aproximação pela Normal quando n é grande",
                descricao = "À medida que n aumenta, a distribuição das médias torna-se aproximadamente Normal, mesmo para contagens discretas."),
                hr(),
                h5("Comportamento do Parâmetro λ (Lambda):"),
                exemplo_card(ns("poisson_raro"),
                  titulo = "Eventos Raros (λ < 1)",
                  descricao = "Quando a taxa de ocorrência é muito baixa, a maior probabilidade se concentra no zero. Veja a assimetria positiva extrema."
                ),
                exemplo_card(ns("poisson_media_var"),
                  titulo = "Média igual à Variância",
                  descricao = "Uma propriedade única: em uma Poisson, o valor esperado é exatamente igual à sua variância. Veja como o gráfico se espalha conforme λ cresce."
                )
    ),
    
            "Qui-quadrado" = tagList(
              
                h5("Distribuição Qui-quadrado e o TCL"),
                exemplo_card(ns( "exemplo_chi"),
                titulo = "TCL: aproximação pela Normal quando n é grande",
                descricao = "Embora seja assimétrica e definida apenas para valores positivos, a distribuição das médias amostrais converge para a Normal quando n é grande"),
                hr(),
                h5("Origem e Relações:"),
                exemplo_card(ns("q_quad_origem"),
                  titulo = "A Normal ao Quadrado",
                  descricao = "A Qui-Quadrado com 1 grau de liberdade é exatamente o quadrado de uma Normal Padrão. Entenda por que ela só assume valores positivos."
                ),
                exemplo_card(ns("q_quad_soma"),
                  titulo = "Soma de Quadrados (ν)",
                  descricao = "À medida que somamos mais Normais ao quadrado, os graus de liberdade aumentam e a curva se desloca para a direita."
                ),
                exemplo_card(ns("q_quad_gama"),
                  titulo = "Relação com a distribuição Gama",
                  descricao = "A Qui-quadrado é um caso especial da Distribuição Gama. Entenda como os parâmetros se conectam matematicamente."
                )
    ),
    
           "T-Student" = tagList(
             
                h5("Distribuição T-Student e o TCL"),
                exemplo_card(ns( "exemplo_t"),
                titulo = "TCL: aproximação pela Normal quando n é grande",
                descricao = "À medida que n aumenta, a distribuição das médias amostrais aproxima-se da Normal, especialmente quando os graus de liberdade são maiores"),
                hr(),
                h5("Interpretações dos graus de liberdade:"),
                exemplo_card(ns( "t_caudas_pesadas"),
                  titulo = "Caudas Pesadas (v pequeno)",
                  descricao = "Com poucos graus de liberdade, a T-Student tem caudas mais volumosas que a Normal, aceitando mais valores extremos."
                ),
                exemplo_card(ns("t_v10"),
                  titulo = "Amostras Pequenas (ν = 10)",
                  descricao = "Com 10 graus de liberdade, a distribuição ainda é mais larga que a Normal. ."
                ),
                
                # Card v=30
                exemplo_card(ns( "t_v30"),
                  titulo = "A Fronteira da Normalidade (ν = 30)",
                  descricao = "O ponto clássico da estatística: com 30 graus de liberdade, a curva T torna-se visualmente muito próxima da Normal."
                )
    ),
    
       "Uniforme" = tagList(
         
                h5("Distribuição Uniforme e o TCL"),
               exemplo_card(ns( "exemplo_unif"),
               titulo = "TCL: aproximação pela Normal quando n é grande",
               descricao = "Apesar da distribuição original ser limitada, as médias amostrais convergem para a Normal com n grande."),
               hr(),
               h5("Propriedades"),
               exemplo_card(ns("uni_caixa"),
                 titulo = "A Geometria da Uniforme",
                 descricao = "A probabilidade é a área de um retângulo. A altura do gráfico se ajusta para manter a área total igual a 1."
               ),
               exemplo_card(ns("uni_simetria"),
                 titulo = "Centro e Dispersão",
                 descricao = "A média está sempre no ponto médio (a+b)/2. O gráfico se desloca mantendo sua forma constante."
               )
    ),
    
  NULL
  )
  
  if (is.null(conteudo)) return(NULL)
  
  tagList(
    conteudo,
    hr(),
    p(
      strong("Nota didática: "),
      "S indica o número de médias amostrais simuladas. ",
      "Valores maiores de S não alteram o resultado teórico, ",
      "mas tornam a aproximação pela Normal mais visível."
    )
  )
}