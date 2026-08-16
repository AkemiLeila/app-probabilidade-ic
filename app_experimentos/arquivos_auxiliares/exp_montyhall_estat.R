exp_montyhall_estat <- function(input, output, session, estatisticas) {
  
  # Gráfico de partidas ganhas
  output$grafico_ganhos <- renderPlot({
    
    dados <- estatisticas()
    
    ganhou_trocando <- sum(
      dados$resultado == "Ganhou!" &
        dados$decisao == "trocar"
    )
    
    ganhou_mantendo <- sum(
      dados$resultado == "Ganhou!" &
        dados$decisao == "manter"
    )
    
    valores <- c(
      ganhou_trocando,
      ganhou_mantendo
    )
    
    limite_y <- max(1, max(valores) + 1)
    
    barplot(
      valores,
      names.arg = c("Trocando", "Mantendo"),
      main = "Partidas ganhas",
      ylab = "Número de partidas",
      ylim = c(0, max(1, valores) + 1),
      col = c("#6F968B", "#ADB3B8"),
      yaxt = "n"
    )
    
    axis(2, at = 0:max(valores))
  })
  
  
  # Gráfico de partidas perdidas
  output$grafico_perdas <- renderPlot({
    
    dados <- estatisticas()
    
    perdeu_trocando <- sum(
      dados$resultado == "Perdeu!" &
        dados$decisao == "trocar"
    )
    
    perdeu_mantendo <- sum(
      dados$resultado == "Perdeu!" &
        dados$decisao == "manter"
    )
    
    valores <- c(
      perdeu_trocando,
      perdeu_mantendo
    )
    
    limite_y <- max(1, max(valores) + 1)
    
    barplot(
      valores,
      names.arg = c("Trocando", "Mantendo"),
      main = "Partidas perdidas",
      ylab = "Número de partidas",
      ylim = c(0, max(1, valores) + 1),
      col = c("#A66A6E", "#ADB3B8"),
      yaxt = "n"
    )
    
    axis(2, at = 0:max(valores))
  })
  
  
  # Interface
  tagList(
    
    h4("Estatísticas das partidas"),
    
    fluidRow(
      
      column(
        width = 6,
        plotOutput(
          session$ns("grafico_ganhos")
        )
      ),
      
      column(
        width = 6,
        plotOutput(
          session$ns("grafico_perdas")
        )
      )
      
    )
  )
}