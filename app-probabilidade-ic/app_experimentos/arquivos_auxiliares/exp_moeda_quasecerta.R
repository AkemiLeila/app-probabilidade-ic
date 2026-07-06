exp_moeda_cqc <- function(input, output, session){
  
  
  
  dados_cqc <- reactiveVal(NULL)
  

  
  observeEvent(input$nova_simulacao, {
    
    trajetorias <- replicate(6,
                             {
                               lancamentos <- rbinom(n = 1000, size = 1, prob = 0.5)
                               caras_acumulada <- cumsum(lancamentos)
                               frequencia <- caras_acumulada/seq_along(lancamentos)
                               frequencia
                             })
     dados_cqc(
      list(
        n = seq_len(1000),
        trajetorias = trajetorias)
    )
    
     
     
     updateSliderInput(
       session,
       "progresso",
       value = 1
     )
     
     
  })
  
  
  
  #outputs
  
  output$grafico_cqc <- renderPlot({
    
    req(dados_cqc())
    
    df <- dados_cqc()
    
    k <- input$progresso
    
    cores <- c(
      "#2F5D50", # verde institucional
      "#2E4F73", # azul institucional
      "#A4804C", # dourado
      "#67161C", # vinho
      "#7B2CBF", # roxo
      "#D95F02"  # laranja queimado
    )
    
    nomes <- paste("Amostra", 1:6)
    
    matplot(
      x = df$n[1:k],
      y = df$trajetorias[1:k, , drop = FALSE],
      type = "l",
      col = cores,
      lty = 1,
      lwd = 2,
      ylim = c(0, 1),
      xlab = "Número de lançamentos",
      ylab = "Frequência acumulada de caras",
      main = "Convergência Quase Certa"
    )
    
    abline(
      h = 0.5,
      col = "#67161C",
      lty = 2,
      lwd = 2,
      
    )
    
    legend(
      "topright",
      legend = c(
        paste("Amostra", 1:6),
        expression(p == 0.5)
      ),
      col = c(cores, "#67161C"),
      lty = c(rep(1, 6), 2),
      lwd = 2,
      bty = "n"
    )
    
  })
  
  
  
  #tagList
  
  
  tagList(
    br(),
    
    actionButton(session$ns("nova_simulacao"),
                 "Nova Simulação"),
    
    br(),
    br(),
    sliderInput(
      session$ns("progresso"),
      "Número de lançamentos",
      min = 1,
      max = 1000,
      value = 1,
      step = 1,
      animate = TRUE
    ),
    
    br(),
    plotOutput(session$ns("grafico_cqc"), height = "600px")
  )
  
}