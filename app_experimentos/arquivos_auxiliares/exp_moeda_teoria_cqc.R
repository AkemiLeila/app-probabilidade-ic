exp_moeda_teoria_cqc <- function(input, output, session){
  
  tagList(
    
    withMathJax(),
    
    tags$style(HTML("
    
      /* =====================================================
         ESTILO GERAL
         ===================================================== */
      
      .teoria-quase-certa {
        max-width: 1000px;
        margin: 0 auto;
        padding: 20px 35px 50px;
        color: #343a40;
        line-height: 1.6;
      }
      
      .teoria-quase-certa h3 {
        font-size: 22px;
        font-weight: 600;
        margin-top: 38px;
        margin-bottom: 16px;
        color: #343a40;
      }
      
      .teoria-quase-certa h4 {
        font-size: 18px;
        font-weight: 600;
        margin-top: 25px;
        margin-bottom: 10px;
        color: #343a40;
      }
      
      .teoria-quase-certa p {
        margin-bottom: 15px;
        text-align: justify;
      }
      
      
      /* =====================================================
         TÍTULO
         ===================================================== */
      
      .titulo-quase-certa {
        text-align: center;
        margin-bottom: 35px;
      }
      
      .titulo-quase-certa h3 {
        margin-top: 10px;
        margin-bottom: 8px;
        font-size: 26px;
      }
      
      .titulo-quase-certa p {
        text-align: center;
        color: #6c757d;
        margin-bottom: 0;
      }
      
      
      /* =====================================================
         LINHA DIVISÓRIA
         ===================================================== */
      
      .divisoria-quase-certa {
        border: 0;
        border-top: 1px solid #dee2e6;
        margin: 30px 0;
      }
      
      
      /* =====================================================
         IDEIA IMPORTANTE
         ===================================================== */
      
      .ideia-quase-certa {
        margin: 25px 0;
        padding: 15px 20px;
        background-color: #f5f6f5;
        border-radius: 6px;
      }
      
      .ideia-quase-certa p {
        margin-bottom: 8px;
      }
      
      .ideia-quase-certa p:last-child {
        margin-bottom: 0;
      }
      
      
      /* =====================================================
         FÓRMULAS
         ===================================================== */
      
      .formula-quase-certa {
        text-align: center;
        margin: 22px 0;
        font-size: 18px;
      }
      
      
      /* =====================================================
         CONCLUSÃO
         ===================================================== */
      
      .conclusao-quase-certa {
        margin: 30px 0;
        padding: 18px 20px;
        background-color: #faf7f7;
        border-radius: 6px;
        text-align: center;
      }
      
      .conclusao-quase-certa p {
        text-align: center;
      }
      
      
      /* =====================================================
         REFERÊNCIA
         ===================================================== */
      
      .referencia-quase-certa {
        margin-top: 40px;
        padding-top: 15px;
        border-top: 1px solid #dee2e6;
        font-size: 13px;
        color: #6c757d;
      }
      
      .referencia-quase-certa p {
        text-align: left;
      }
      
      
      /* =====================================================
         RESPONSIVIDADE
         ===================================================== */
      
      @media (max-width: 650px) {
        
        .teoria-quase-certa {
          padding: 15px 18px 40px;
        }
        
        .titulo-quase-certa h3 {
          font-size: 23px;
        }
        
      }
    
    ")),
    
    
    div(
      class = "teoria-quase-certa",
      
      
      # =====================================================
      # TÍTULO
      # =====================================================
      
      div(
        class = "titulo-quase-certa",
        
        h3("Convergência quase certa"),
        
        p(
          "Uma forma mais forte de descrever o comportamento de uma sequência de variáveis aleatórias."
        )
      ),
      
      
      
      p(
        "A convergência quase certa descreve uma forma mais forte de convergência. ",
        "Em vez de considerar apenas a probabilidade de a proporção estar distante ",
        "do valor teórico, analisamos o comportamento das sequências de resultados ",
        "ao longo das realizações do experimento."
      ),
      
      p(
        "Para cada realização da sequência de lançamentos, obtemos uma sequência ",
        "de proporções acumuladas:"
      ),
      
      div(
        class = "formula-quase-certa",
        
        "$$\\hat{p}_1,\\ \\hat{p}_2,\\ \\hat{p}_3,\\ldots,\\hat{p}_n,\\ldots$$"
      ),
      
    
  
      
      p(
        "Dizemos que uma sequência de variáveis aleatórias Xn converge quase ",
        "certamente para uma variável aleatória X quando, com probabilidade 1, ",
        "a sequência dos valores observados converge para X."
      ),
      
      div(
        class = "formula-quase-certa",
        
        "$$X_n \\xrightarrow{q.c.} X$$"
      ),
      
      p(
        "Uma forma de escrever essa ideia é:"
      ),
      
      div(
        class = "formula-quase-certa",
        
        "$$P\\left(
        \\lim_{n\\to\\infty} X_n = X
        \\right) = 1$$"
      ),
      
     
      
      
      
      # =====================================================
      # 5. RELAÇÃO COM A LEI FORTE
      # =====================================================
      
      h3("A Lei Forte dos Grandes Números"),
      
      p(
        "A conexão entre a convergência quase certa e o experimento da moeda aparece ",
        "na chamada Lei Forte dos Grandes Números."
      ),
      
      p(
        "Para uma sequência de lançamentos independentes de uma moeda com probabilidade ",
        "p de resultar em cara, a proporção de caras converge quase certamente para p."
      ),
      
      div(
        class = "formula-quase-certa",
        
        "$$\\hat{p}_n \\xrightarrow{q.c.} p$$"
      ),
      
      p(
        "No caso de uma moeda honesta, p = 0,5, portanto:"
      ),
      
      div(
        class = "formula-quase-certa",
        
        "$$\\hat{p}_n \\xrightarrow{q.c.} 0,5$$"
      ),
      
      
      
      p(
        "Imagine que o experimento da moeda seja repetido infinitamente. ",
        "Cada sequência completa de resultados constitui uma realização possível ",
        "do experimento."
      ),
      
      p(
        "A convergência quase certa afirma que, para praticamente todas essas ",
        "realizações , ou seja, para um conjunto de probabilidade 1, a proporção ",
        "acumulada de caras se aproxima de 0,5 conforme o número de lançamentos cresce."
      ),
      
      # =====================================================
      # REFERÊNCIA
      # =====================================================
      
      div(
        class = "referencia-quase-certa",
        
        p(
          strong("Observação sobre a referência:")
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