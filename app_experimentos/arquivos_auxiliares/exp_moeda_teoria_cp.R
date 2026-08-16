exp_moeda_teoria_cp <- function(input, output, session){
  
  tagList(
    
    withMathJax(),
    
    tags$style(HTML("
  
    /* =====================================================
       ESTILO GERAL
       ===================================================== */
    
    .teoria-moeda {
      max-width: 1000px;
      margin: 0 auto;
      padding: 20px 35px 50px;
      color: #343a40;
      line-height: 1.6;
    }
    
    .teoria-moeda h3 {
      font-size: 22px;
      font-weight: 600;
      margin-top: 38px;
      margin-bottom: 16px;
      color: #343a40;
    }
    
    .teoria-moeda h4 {
      font-size: 18px;
      font-weight: 600;
      margin-top: 25px;
      margin-bottom: 10px;
      color: #343a40;
    }
    
    .teoria-moeda p {
      margin-bottom: 15px;
      text-align: justify;
    }
    
    
    /* =====================================================
       TÍTULO
       ===================================================== */
    
    .titulo-moeda {
      text-align: center;
      margin-bottom: 35px;
    }
    
    .titulo-moeda h3 {
      margin-top: 10px;
      margin-bottom: 8px;
      font-size: 26px;
    }
    
    .titulo-moeda p {
      text-align: center;
      color: #6c757d;
      margin-bottom: 0;
    }
    
    
    /* =====================================================
       LINHA DIVISÓRIA
       ===================================================== */
    
    .divisoria-moeda {
      border: 0;
      border-top: 1px solid #dee2e6;
      margin: 30px 0;
    }
    
    
    /* =====================================================
       IDEIA IMPORTANTE
       ===================================================== */
    
    .ideia-moeda {
      margin: 25px 0;
      padding: 15px 20px;
      background-color: #f5f6f5;
      border-radius: 6px;
    }
    
    .ideia-moeda p {
      margin-bottom: 8px;
    }
    
    .ideia-moeda p:last-child {
      margin-bottom: 0;
    }
    
    
    /* =====================================================
       FÓRMULAS
       ===================================================== */
    
    .formula-moeda {
      text-align: center;
      margin: 22px 0;
      font-size: 18px;
    }
    
    
    /* =====================================================
       LISTA
       ===================================================== */
    
    .lista-moeda {
      margin: 15px 0 20px 20px;
      padding-left: 20px;
    }
    
    .lista-moeda li {
      margin-bottom: 8px;
    }
    
    
    /* =====================================================
       CONCLUSÃO
       ===================================================== */
    
    .conclusao-moeda {
      margin: 30px 0;
      padding: 18px 20px;
      background-color: #faf7f7;
      border-radius: 6px;
      text-align: center;
    }
    
    .conclusao-moeda p {
      text-align: center;
    }
    
    
    /* =====================================================
       REFERÊNCIA
       ===================================================== */
    
    .referencia-moeda {
      margin-top: 40px;
      padding-top: 15px;
      border-top: 1px solid #dee2e6;
      font-size: 13px;
      color: #6c757d;
    }
    
    .referencia-moeda p {
      text-align: left;
    }
    
    
    /* =====================================================
       RESPONSIVIDADE
       ===================================================== */
    
    @media (max-width: 650px) {
      
      .teoria-moeda {
        padding: 15px 18px 40px;
      }
      
      .titulo-moeda h3 {
        font-size: 23px;
      }
      
    }
  
  ")),
    
    
    div(
      class = "teoria-moeda",
      
      
      # =====================================================
      # TÍTULO
      # =====================================================
      
      div(
        class = "titulo-moeda",
        
        h3("Lei dos Grandes Números"),
        
        p(
          "A convergência da proporção observada de caras em lançamentos sucessivos de uma moeda."
        )
      ),
      
      
      # =====================================================
      # 1. EXPERIMENTO
      # =====================================================
      
      h3("O experimento"),
      
      p(
        "Considere o lançamento sucessivo de uma moeda honesta. Em cada lançamento, ",
        "a probabilidade de obter cara é igual a 0,5."
      ),
      
      div(
        class = "formula-moeda",
        
        "$$P(\\text{cara}) = p = 0,5$$"
      ),
      
      p(
        "Após n lançamentos, podemos contar quantas vezes o resultado cara foi observado. ",
        "A proporção acumulada de caras é definida pela razão entre o número de caras ",
        "observadas e o número total de lançamentos."
      ),
      
      div(
        class = "formula-moeda",
        
        "$$\\hat{p}_n = \\frac{\\text{Número de caras observadas}}{n}$$"
      ),
      
      
      hr(class = "divisoria-moeda"),
      
      
      # =====================================================
      # 2. LEI FRACA DOS GRANDES NÚMEROS
      # =====================================================
      
      h3("Lei Fraca dos Grandes Números"),
      
      p(
        "A Lei Fraca dos Grandes Números estabelece que, à medida que o número ",
        "de lançamentos aumenta, a proporção observada de caras se aproxima, ",
        "em probabilidade, da probabilidade teórica de obter cara."
      ),
      
      
      div(
        class = "formula-moeda",
        "$$\\lim_{n \\to \\infty} P\\left( \\left|\\hat{p}_n - p\\right| \\ge \\varepsilon \\right) = 0, \\qquad \\forall\\,\\varepsilon > 0$$"
      ),
      
        
        p(
          "Ou seja, se escolhermos uma margem de erro, por exemplo, ",
          "uma pequena distância em torno de 0,5, a probabilidade de a proporção ",
          "observada ficar fora desse intervalo torna-se cada vez menor conforme ",
          "a quantidade de lançamentos aumenta."
        ),
      
      
      
      
      
      # =====================================================
      # 4. CONVERGÊNCIA EM PROBABILIDADE
      # =====================================================
      
      h3("Convergência em probabilidade"),
      
      p(
        "A relação descrita pela Lei Fraca dos Grandes Números pode ser expressa ",
        "por meio da notação de convergência em probabilidade."
      ),
      
      div(
        class = "formula-moeda",
        
        "$$\\hat{p}_n \\xrightarrow{P} 0,5$$"
      ),
      
      
      div(
        class = "conclusao-moeda",
        
        p(
          "A Lei Fraca dos Grandes Números afirma que, quando o número de ",
          "lançamentos aumenta, a proporção observada tende a se aproximar do valor ",
          "teórico de 0,5 em probabilidade."
        )
        
      ),
      
      
      
      
      # =====================================================
      # REFERÊNCIA
      # =====================================================
      
      div(
        class = "referencia-moeda",
        
        p(
          strong("Referência teórica:")
        ),
        
        p(
          "ROSS, Sheldon M. ",
          em("Probabilidade: um curso moderno com aplicações"),
          ". 8ª ed. Porto Alegre: Bookman, 2010."
        )
      )
      
    )
  )

  
}