exp_moeda <- function(input, output, session){
  
  
  #definições
   
  resultado_atual <- reactiveVal(NULL)
  historico_resultados <- reactiveVal(character(0)) 
  
  
  #primeiro lançamento
  observe({   
    
    if(length(historico_resultados()) == 0){
      lancamento <- sample(
        c("cara", "coroa"),
        size = 1,
        replace = TRUE
      )
      resultado_atual(lancamento)
      historico_resultados(lancamento)
      
    }
    
  })
  
  #outros lançamentos a partir do botao
  observeEvent(input$novo_lancamento, {
    
                   lancamento <- sample(
                                     c("cara", "coroa"),
                                     size = 1,
                                     replace = TRUE
                                     )
                   resultado_atual(lancamento)
                   historico_resultados(
                                         c(historico_resultados(), lancamento)
                   )
  })
  
  
  
  dados_convergencia <- reactive({
                                  hist <- historico_resultados()
                                  n <- length(hist)
                                  req(n > 0)
                                  p_cara <- cumsum(hist == "cara") / seq_along(hist)
                                  data.frame(
                                            n = seq_along(hist),
                                            p = p_cara
    )
  })
  
  
  
 #_____________OUTPUTS
  
  output$resultado_moeda <- renderText({
    req(resultado_atual())
    paste("Último lançamento:", resultado_atual())

  })
  
  
  output$historico_moeda <- renderText({
    req(length(historico_resultados()) > 0)
    paste(historico_resultados(), collapse = " | ")
    
  })
  
  output$tabela_moeda <- renderTable({
    req(length(historico_resultados()) > 0)
    data.frame(
      Resultado = c("cara", "coroa"),
      Frequência = c(
        sum(historico_resultados() == "cara"),
        sum(historico_resultados() == "coroa")
    ))
    
  })
  
  
  output$grafico_convergencia <- renderPlot({
    df <- dados_convergencia()
    plot(
      df$n, df$p,
      type = "l",
      col = "#2F5D50",
      lwd = 2,
      ylim = c(0, 1),
      xlab = "Número de lançamentos",
      ylab = "P(cara) acumulada",
      main = "Convergência da probabilidade da moeda"
    )
      abline(h = 0.5, col = "#67161C", lty = 2, lwd = 2)
    
  })
  
  
  #taglist com textos e diagramação
  tagList(
    
    h3("Lançamento de uma Moeda Honesta"),
    br(), 
   
    textOutput(
      session$ns("resultado_moeda")
    ),
    br(),
    fluidRow(
      
      column(
        width = 6,
        tableOutput(
          session$ns("tabela_moeda")
        )
      ),
      
      column(
        width = 6,
        br(),
        actionButton(
          session$ns("encerrar_partida"),
          "Encerrar Partida",
          class = "btn-danger"
        )
      )
      
    ),
    plotOutput(session$ns("grafico_convergencia"), height = "350px"),
    br(),
    h3("Convergência em Probabilidade"),
    br(), 
    withMathJax(
      HTML("
  <p>
  Ao lançar uma moeda honesta sucessivamente, onde a probabilidade teórica de sair 'cara' é 
  \\(p = 0,5\\) a proporção observada de caras após \\(n\\) lançamentos é definida como:
  </p>
  
  $$\\hat{p}_n = \\frac{\\text{Número de caras}}{n}$$
  
  <p>
  A <strong>Lei Fraca dos Grandes Números</strong> estabelece que esta proporção 
  converge em probabilidade para a probabilidade teórica:
  </p>
  
  $$\\lim_{n\\to\\infty} P(|\\hat{p}_n - p| > \\varepsilon) = 0, \\quad \\forall \\varepsilon > 0$$
  
  <p>
  Ou seja, à medida que aumentamos o número de lançamentos, a probabilidade de a 
  proporção observada se afastar do valor verdadeiro (0,5) por mais do que uma pequena 
  margem \\(\\varepsilon\\) tende a zero.
  </p>
  
  <p>
  No gráfico abaixo, a linha verde representa \\(\\hat{p}_n\\) (proporção acumulada de caras) 
  e a linha vermelha tracejada indica o valor teórico \\(p = 0,5\\). Observamos que, 
  conforme \\(n\\) cresce:
  </p>
  
  <ul>
    <li>As oscilações de \\(\\hat{p}_n\\) diminuem de amplitude</li>
    <li>\\(\\hat{p}_n\\) se aproxima cada vez mais de 0,5</li>
    <li>A probabilidade de grandes desvios torna-se cada vez menor</li>
  </ul>
  
  <p>
  Matematicamente, dizemos que:
  </p>
  
  $$\\hat{p}_n \\xrightarrow{P} 0,5$$
  
  <p>
  onde \\(\\xrightarrow{P}\\) denota convergência em probabilidade.
  </p>
  ")
    )
    
  )
  

  
  
}