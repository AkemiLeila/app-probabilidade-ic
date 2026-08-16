exp_montyhall_teoria <- function(input,output, session){
 
  tagList(
    
    withMathJax(),
    
    tags$style(HTML("

    /* =====================================================
       ESTILO GERAL
       ===================================================== */

    .teoria-montyhall {
      max-width: 1000px;
      margin: 0 auto;
      padding: 20px 30px 50px;
      color: #343a40;
      line-height: 1.6;
    }

    .teoria-montyhall h3 {
      font-size: 22px;
      font-weight: 600;
      margin-top: 38px;
      margin-bottom: 16px;
      color: #343a40;
    }

    .teoria-montyhall h4 {
      font-size: 18px;
      font-weight: 600;
      margin-top: 25px;
      margin-bottom: 10px;
      color: #343a40;
    }

    .teoria-montyhall p {
      margin-bottom: 15px;
      text-align: justify;
    }


    /* =====================================================
       TÍTULO INICIAL
       ===================================================== */

    .titulo-teoria {
      text-align: center;
      margin-bottom: 35px;
    }

    .titulo-teoria h3 {
      margin-top: 10px;
      margin-bottom: 8px;
      font-size: 26px;
    }

    .titulo-teoria p {
      text-align: center;
      color: #6c757d;
      margin-bottom: 0;
    }


    /* =====================================================
       LINHAS DIVISÓRIAS
       ===================================================== */

    .divisoria-teoria {
      border: 0;
      border-top: 1px solid #dee2e6;
      margin: 30px 0;
    }


    /* =====================================================
       PERGUNTA PRINCIPAL
       ===================================================== */

   .pergunta-monty {
  margin: 25px 0 30px;
  padding: 15px 20px;
  background-color: #f5f6f5;
  border-radius: 6px;
} 

    .pergunta-monty p {
      margin: 0;
      text-align: left;
    }


    /* =====================================================
       IDEIA IMPORTANTE
       ===================================================== */

    .ideia-importante {
  margin: 25px 0;
  padding: 15px 20px;
  background-color: #f5f6f5;
  border-radius: 6px;
}

    .ideia-importante p {
      margin-bottom: 8px;
    }

    .ideia-importante p:last-child {
      margin-bottom: 0;
    }


    /* =====================================================
       CONCLUSÃO
       ===================================================== */

   .conclusao-monty {
  margin: 30px 0;
  padding: 18px 20px;
  background-color: #faf7f7;
  border-radius: 6px;
  text-align: center;
}

    .conclusao-monty p {
      text-align: center;
    }


    /* =====================================================
       FÓRMULAS
       ===================================================== */

    .formula-monty {
      text-align: center;
      margin: 22px 0;
      font-size: 18px;
    }


    /* =====================================================
       REFERÊNCIA
       ===================================================== */

    .referencia-teoria {
      margin-top: 40px;
      padding-top: 15px;
      border-top: 1px solid #dee2e6;
      font-size: 13px;
      color: #6c757d;
    }

    .referencia-teoria p {
      text-align: left;
    }


    /* =====================================================
       RESPONSIVIDADE
       ===================================================== */

    @media (max-width: 650px) {

      .teoria-montyhall {
        padding: 15px 18px 40px;
      }

      .titulo-teoria h3 {
        font-size: 23px;
      }

      .arvore-ramos {
        flex-direction: column;
      }

      .arvore-ramos::before {
        display: none;
      }

      .arvore-ramo {
        width: 100%;
        padding-top: 30px;
      }

    }

  ")),
    
    
    div(
      class = "teoria-montyhall",
      
      
      # =====================================================
      # TÍTULO
      # =====================================================
      
      div(
        class = "titulo-teoria",
        
        h3("O problema de Monty Hall"),
        
        p(
          "Uma introdução à probabilidade condicional por meio de um problema clássico."
        )
      ),
      
      
      # =====================================================
      # 1. PROBLEMA
      # =====================================================
      
      h3("O problema"),
      
      p(
        "Considere um jogo com três portas. Atrás de uma delas existe um prêmio, ",
        "enquanto atrás das outras duas existem cabras."
      ),
      
      p(
        "O participante escolhe inicialmente uma das portas. Depois de sua escolha, ",
        "o apresentador, que conhece a localização do prêmio, abre uma das duas portas ",
        "que não foram escolhidas, revelando uma cabra."
      ),
      
      p(
        "Restam então duas portas fechadas: a porta escolhida inicialmente e uma ",
        "segunda porta. O participante deve decidir se mantém sua escolha inicial ",
        "ou se troca de porta. Trocar de porta aumenta a probabilidade de ganhar?"
      ),
      
      hr(class = "divisoria-teoria"),
      
      
      # =====================================================
      # 2. HISTÓRIA
      # =====================================================
      
      h3("A história"),
      
      p(
        "O problema ficou conhecido por sua relação com o programa de televisão ",
        em("Let's Make a Deal"),
        ", apresentado por Monty Hall. O programa envolvia participantes que ",
        "escolhiam entre diferentes portas e alternativas para tentar ganhar prêmios."
      ),
      
      p(
        "A versão matemática do problema tornou-se posteriormente um exemplo clássico ",
        "de raciocínio probabilístico, principalmente porque sua solução entra em conflito ",
        "com uma intuição bastante comum."
      ),
      
      p(
        "Depois que Monty abre uma porta e mostra uma cabra, é natural pensar que ",
        "as duas portas restantes possuem a mesma probabilidade de conter o prêmio."
      ),
      
      div(
        class = "ideia-importante",
        
        p(
          "No entanto, Monty sabe onde está o prêmio e não escolhe aleatoriamente ",
          "qual porta abrir. Ele sempre abre uma porta que contém uma cabra."
        )
      ),
      
      
      # =====================================================
      # 3. ESCOLHA INICIAL
      # =====================================================
      
      h3("A escolha inicial"),
      
      p(
        "Para entender o problema, suponha que o jogador escolha inicialmente a ",
        strong("Porta 1"),
        "."
      ),
      
      p(
        "Antes de Monty abrir qualquer porta, o prêmio possui a mesma probabilidade ",
        "de estar atrás de cada uma das três portas."
      ),
      
      div(
        class = "formula-monty",
        
        "$$P(\\text{prêmio na Porta 1}) = \\frac{1}{3}$$"
      ),
      
      p(
        "Consequentemente, existe uma probabilidade de 2/3 de que o prêmio esteja ",
        "em uma das duas portas que o jogador não escolheu."
      ),
      
      div(
        class = "formula-monty",
        
        "$$P(\\text{prêmio nas portas não escolhidas}) = \\frac{2}{3}$$"
      ),
      
      div(
        class = "ideia-importante",
        
        p(
          strong("A escolha inicial não muda de probabilidade.")
        ),
        
        p(
          "A porta escolhida continua tendo probabilidade de 1/3 de conter o prêmio. ",
          "As outras duas portas, juntas, representam uma probabilidade de 2/3."
        )
      ),
      
      
      # =====================================================
      # 4. PROBABILIDADE CONDICIONAL
      # =====================================================
      
      h3("Probabilidade condicional"),
      
      p(
        "A abertura de uma porta por Monty fornece uma informação adicional sobre ",
        "o experimento. Para compreender seu efeito, podemos utilizar o conceito ",
        "de probabilidade condicional."
      ),
      
      p(
        "A probabilidade condicional descreve a probabilidade de um evento quando ",
        "sabemos que outro evento já ocorreu."
      ),
      
      div(
        class = "formula-monty",
        
        "$$P(A\\mid B) = \\frac{P(A\\cap B)}{P(B)}$$"
      ),
      
      p(
        "No problema de Monty Hall, a informação fornecida pelo apresentador não é ",
        "independente da localização do prêmio. Monty conhece o resultado e escolhe ",
        "deliberadamente uma porta que contém uma cabra."
      ),
      
      div(
        class = "ideia-importante",
        
        p(
          strong("Por isso, as duas portas restantes não se tornam equivalentes.")
        ),
        
        p(
          "A porta escolhida inicialmente continua associada à probabilidade de 1/3. ",
          "A informação fornecida por Monty permite concentrar a probabilidade de 2/3 ",
          "das portas inicialmente não escolhidas na única porta que permanece fechada."
        )
      ),
      
      p(
        "Assim, a estratégia de trocar corresponde ao evento de a escolha inicial ",
        "estar errada."
      ),
      
      div(
        class = "formula-monty",
        
        "$$P(\\text{ganhar trocando}) = P(\\text{escolha inicial errada}) = \\frac{2}{3}$$"
      ),
      
      p(
        "Já manter a escolha corresponde ao evento de a escolha inicial estar correta."
      ),
      
      div(
        class = "formula-monty",
        
        "$$P(\\text{ganhar mantendo}) = P(\\text{escolha inicial correta}) = \\frac{1}{3}$$"
      ),
      
      
      div(
        class = "conclusao-monty",
        
        p(
          strong(
            "Portanto, trocar de porta oferece o dobro da probabilidade de ganhar."
          )
        ),
        
        div(
          class = "formula-monty",
          
          "$$P(\\text{ganhar trocando}) = \\frac{2}{3}
      \\quad > \\quad
      P(\\text{ganhar mantendo}) = \\frac{1}{3}$$"
        )
      ),
      
      
      # =====================================================
      # 5. INTUIÇÃO
      # =====================================================
      
      h3("Por que a informação de Monty importa?"),
      
      p(
        "O ponto central do problema está no comportamento do apresentador. ",
        "Se Monty simplesmente escolhesse uma das portas restantes ao acaso, ",
        "a informação obtida ao abrir uma porta teria outra interpretação."
      ),
      
      p(
        "No problema de Monty Hall, porém, o apresentador conhece a localização ",
        "do prêmio e segue uma regra específica: ele nunca abre a porta escolhida ",
        "pelo jogador e nunca abre a porta que contém o prêmio."
      ),
      
      p(
        "Portanto, a porta aberta por Monty não representa apenas uma porta que ",
        "foi eliminada aleatoriamente. Sua ação transmite informação sobre onde ",
        "o prêmio pode estar."
      ),
      
      
      # =====================================================
      # 6. EXPERIMENTO
      # =====================================================
      
      h3("O experimento"),
      
      p(
        "Em cada partida, o prêmio é sorteado aleatoriamente. Você escolhe uma porta, ",
        "Monty Hall abre uma porta com cabra e, em seguida, você decide se deseja ",
        "manter ou trocar sua escolha."
      ),
      
      p(
        "Ao repetir o experimento várias vezes, podemos comparar as frequências ",
        "observadas com as probabilidades teóricas."
      ),
      
      div(
        class = "ideia-importante",
        
        p(
          strong("Teoricamente:")
        ),
        
        div(
          class = "formula-monty",
          
          "$$P(\\text{ganhar mantendo}) = \\frac{1}{3}$$"
        ),
        
        div(
          class = "formula-monty",
          
          "$$P(\\text{ganhar trocando}) = \\frac{2}{3}$$"
        ),
        
        p(
          " Conforme o número de partidas aumenta, espera-se que as ",
          "frequências relativas se aproximem das probabilidades teóricas."
        )
      ),
      
      
      # =====================================================
      # REFERÊNCIA
      # =====================================================
      
      div(
        class = "referencia-teoria",
        
        p(
          strong("Referência teórica:")
        ),
        
        p(
          "ROSS, Sheldon M. ",
          em("Probabilidade: um curso moderno com aplicações"),
          ". 8ª ed. Porto Alegre: Bookman, 2010. ",
          "Capítulo 3 — Probabilidade Condicional e Independência."
        )
      )
      
    )
  )
}