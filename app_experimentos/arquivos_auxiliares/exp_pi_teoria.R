exp_pi_teoria <- function(input, output, session) {
  
  tagList(
    tagList(
      
      withMathJax(),
      
      tags$style(HTML("
    
    /* =====================================================
       ESTILO GERAL
       ===================================================== */

    .teoria-pi {
      max-width: 1000px;
      margin: 0 auto;
      padding: 20px 30px 50px;
      color: #343a40;
      line-height: 1.6;
    }

    .teoria-pi h3 {
      font-size: 22px;
      font-weight: 600;
      margin-top: 38px;
      margin-bottom: 16px;
      color: #343a40;
    }

    .teoria-pi h4 {
      font-size: 18px;
      font-weight: 600;
      margin-top: 25px;
      margin-bottom: 10px;
      color: #343a40;
    }

    .teoria-pi p {
      margin-bottom: 15px;
      text-align: justify;
    }


    /* =====================================================
       TÍTULO INICIAL
       ===================================================== */

    .titulo-teoria-pi {
      text-align: center;
      margin-bottom: 35px;
    }

    .titulo-teoria-pi h3 {
      margin-top: 10px;
      margin-bottom: 8px;
      font-size: 26px;
    }

    .titulo-teoria-pi p {
      text-align: center;
      color: #6c757d;
      margin-bottom: 0;
    }


    /* =====================================================
       LINHAS DIVISÓRIAS
       ===================================================== */

    .divisoria-pi {
      border: 0;
      border-top: 1px solid #dee2e6;
      margin: 30px 0;
    }


    /* =====================================================
       IDEIA IMPORTANTE
       ===================================================== */

    .ideia-importante-pi {
      margin: 25px 0;
      padding: 15px 20px;
      background-color: #f5f6f5;
      border-radius: 6px;
    }

    .ideia-importante-pi p {
      margin-bottom: 8px;
    }

    .ideia-importante-pi p:last-child {
      margin-bottom: 0;
    }


    /* =====================================================
       CONCLUSÃO
       ===================================================== */

    .conclusao-pi {
      margin: 30px 0;
      padding: 18px 20px;
      background-color: #faf7f7;
      border-radius: 6px;
      text-align: center;
    }

    .conclusao-pi p {
      text-align: center;
    }


    /* =====================================================
       FÓRMULAS
       ===================================================== */

    .formula-pi {
      text-align: center;
      margin: 22px 0;
      font-size: 18px;
    }


    /* =====================================================
       REFERÊNCIA
       ===================================================== */

    .referencia-pi {
      margin-top: 40px;
      padding-top: 15px;
      border-top: 1px solid #dee2e6;
      font-size: 13px;
      color: #6c757d;
    }

    .referencia-pi p {
      text-align: left;
    }


    /* =====================================================
       RESPONSIVIDADE
       ===================================================== */

    @media (max-width: 650px) {

      .teoria-pi {
        padding: 15px 18px 40px;
      }

      .titulo-teoria-pi h3 {
        font-size: 23px;
      }

    }

  ")),
      
      
      div(
        class = "teoria-pi",
        
        
        # =====================================================
        # TÍTULO
        # =====================================================
        
        div(
          class = "titulo-teoria-pi",
          
          h3("Método de Monte Carlo"),
          
          p(
            "Uma abordagem probabilística para estimar o valor de π."
          )
        ),
        
   
        
        p(
          "Considere um quadrado de lado 2, centrado na origem. ",
          "Seus limites são dados por ",
          strong("[-1,1] × [-1,1]"),
          ". Portanto, a área do quadrado é:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$A_{quadrado} = 2 \\times 2 = 4$$"
        ),
        
        p(
          "Dentro desse quadrado, considere um círculo de raio 1, ",
          "centrado na origem. A área desse círculo é:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$A_{círculo} = \\pi r^2 = \\pi$$"
        ),
        
        
        hr(class = "divisoria-pi"),
        

        
        p(
          "A razão entre a área do círculo e a área do quadrado é:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
        \\frac{A_{círculo}}{A_{quadrado}}
        =
        \\frac{\\pi}{4}
        $$"
        ),
        
        p(
          "Essa relação geométrica permite construir uma estimativa de π ",
          "por meio de pontos gerados aleatoriamente."
        ),
        
        
        div(
          class = "ideia-importante-pi",
          
          
          p(
            "Se os pontos forem distribuídos uniformemente dentro do quadrado, ",
            "a proporção de pontos que caem dentro do círculo tende a ser igual ",
            "à proporção entre a área do círculo e a área do quadrado."
          )
        ),
        
        
        # =====================================================
        # 3. GERAÇÃO DOS PONTOS
        # =====================================================
        
        h3("Geração dos pontos"),
        
        p(
          "Geramos pontos aleatórios ",
          "(x,y)",
          " uniformemente dentro do quadrado. ",
          "Um ponto estará dentro do círculo quando sua distância até a origem ",
          "for menor ou igual ao raio 1."
        ),
        
        div(
          class = "formula-pi",
          
          "$$
        x^2 + y^2 \\leq 1
        $$"
        ),
        
        p(
          "Assim, podemos classificar cada ponto como pertencente ou não ao círculo."
        ),
        
        
        hr(class = "divisoria-pi"),
        
        
        # =====================================================
        # 4. ESTIMATIVA
        # =====================================================
        
        h3("Estimativa de π"),
        
        p(
          "Seja ",
          strong("N"),
          " o número total de pontos gerados e ",
          strong("N"),
          "<sub>círculo</sub>",
          " o número de pontos que caíram dentro do círculo."
        ),
        
        p(
          "A proporção de pontos dentro do círculo pode ser utilizada ",
          "para aproximar a razão entre as áreas:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
        \\frac{N_{círculo}}{N}
        \\approx
        \\frac{A_{círculo}}{A_{quadrado}}
        =
        \\frac{\\pi}{4}
        $$"
        ),
        
        p(
          "Multiplicando ambos os lados por 4, obtemos uma estimativa para π:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
        \\boxed{
        \\pi \\approx
        4\\frac{N_{círculo}}{N}
        }
        $$"
        ),
        
        
        div(
          class = "conclusao-pi",
          
          p(
            strong(
              "Quanto maior o número de pontos, maior tende a ser a precisão da estimativa."
            )
          ),
          
          div(
            class = "formula-pi",
            
            "$$
          \\pi \\approx
          4\\frac{N_{círculo}}{N}
          $$"
          )
        ),
        
        # =====================================================
        # 5. CONVERGÊNCIA
        # =====================================================
        
        h3("Convergência"),
        
        p(
          "O método de Monte Carlo utiliza a aleatoriedade para obter ",
          "aproximações numéricas. Conforme aumentamos o número de pontos, ",
          "a proporção observada tende a se aproximar da proporção teórica."
        ),
        
        p(
          "Considere a variável aleatória Xi, que representa o resultado do ",
          "i-ésimo ponto gerado:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
  X_i =
  \\begin{cases}
    1, & \\text{se o ponto } i \\text{ cair no círculo},\\\\
    0, & \\text{caso contrário}.
  \\end{cases}
  $$"
        ),
        
        p(
          "A proporção de pontos que caem dentro do círculo, após a geração de ",
          "n pontos, pode ser escrita como:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
  \\hat{p}_n =
  \\frac{1}{n}\\sum_{i=1}^{n}X_i
  $$"
        ),
        
        p(
          "Nessa expressão, n representa o número de pontos utilizados, ",
          "Xi representa o resultado do i-ésimo ponto e ",
          "p chapéu representa a proporção observada."
        ),
        
        p(
          "Como a probabilidade de um ponto cair dentro do círculo é dada pela ",
          "razão entre a área do círculo e a área do quadrado, temos:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
  P(X_i = 1) =
  \\frac{\\pi}{4}
  $$"
        ),
        
        p(
          "A estimativa de π é então obtida multiplicando a proporção observada por 4:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
  \\hat{\\pi}_n = 4\\hat{p}_n
  $$"
        ),
        
        p(
          "A desigualdade de Chebyshev fornece uma justificativa matemática para ",
          "essa convergência. Para qualquer margem de erro ε > 0, ela mostra que ",
          "a probabilidade de a proporção observada diferir da proporção teórica ",
          "π/4 por mais que ε tende a zero à medida que o número de pontos aumenta:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
  P\\left(
    \\left|
      \\hat{p}_n - \\frac{\\pi}{4}
    \\right|
    >
    \\varepsilon
  \\right)
  \\xrightarrow[n \\to \\infty]{}
  0
  $$"
        ),
        
        p(
          "Isso significa que a proporção observada converge em probabilidade ",
          "para a proporção teórica:"
        ),
        
        div(
          class = "formula-pi",
          
          "$$
  \\hat{p}_n
  \\xrightarrow{P}
  \\frac{\\pi}{4}
  $$"
        ),
        
        p(
          "Consequentemente, estimativa de π (π chapéu) é obtida multiplicando a proporção ",
          "observada por 4, ela também converge em probabilidade para π:"
          
        ),
        
        div(
          class = "formula-pi",
          
          "$$
  \\hat{\\pi}_n
  \\xrightarrow{P}
  \\pi
  $$"
        ),
        
        
        # =====================================================
        # REFERÊNCIA
        # =====================================================
        
        div(
          class = "referencia-pi",
          
          p(
            strong("Referência teórica:")
          ),
          
          p(
            "Wagaman A. S.; Dobrow, R. P. Probability: with Applications and R. 2 ed., Wiley, 2021. ",
            
          )
        )
        
      )
    )
  )
  
}