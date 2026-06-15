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
    plotOutput(session$ns("grafico_convergencia"), height = "350px")
    
  )
  

  
  
}