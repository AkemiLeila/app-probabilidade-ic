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
    
  })
  
  
  
  #outputs
  
  output$grafico_cqc <- renderPlot({
    
    req(dados_cqc())
    
    df <- dados_cqc()
    cores <- c(
      "#2F5D50", # verde institucional
      "#2E4F73", # azul institucional
      "#A4804C", # dourado
      "#67161C", # vinho
      "#7B2CBF", # roxo
      "#D95F02"  # laranja queimado
    )
    
    matplot(
      x = df$n,
      y = df$trajetorias,
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
      lwd = 2
    )
    
  })
  
  
  
  #tagList
  
  
  tagList(
    br(),
    
    actionButton(session$ns("nova_simulacao"),
                 "Nova Simulação"),
    
    br(),
    br(),
    plotOutput(session$ns("grafico_cqc"), height = "600px")
  )
  
}