exp_montyhall_estat <- function(input, output, session, estatisticas) {
  
  # Gráfico de partidas trocando 
  
  output$grafico_trocando <- renderPlot({
    
    dados <- estatisticas()
    
    ganhou<- sum(
      dados$resultado == "Ganhou!" &
        dados$decisao == "trocar"
    )
    
    perdeu <- sum(
      dados$resultado == "Perdeu!" &
        dados$decisao == "trocar"
    )
    
    valores <- c(ganhou, perdeu)
    
    limite_y <- max(1, max(valores) + 1)
    
    barplot(
      valores,
      names.arg = c("Ganhou", "Perdeu"),
      main = "Estratégia: Trocando",
      ylab = "Número de partidas",
      ylim = c(0, max(1, valores) + 1),
      col = c("#6F968B", "#A66A6E"),
      yaxt = "n"
    )
    
    axis(2, at = 0:max(valores))
  })
  
  
  # Gráfico de partidas mantendo
  output$grafico_mantendo <- renderPlot({
    
    dados <- estatisticas()
    
    ganhou <- sum(
      dados$resultado == "Ganhou!" &
        dados$decisao == "manter"
    )
    
    perdeu <- sum(
      dados$resultado == "Perdeu!" &
        dados$decisao == "manter"
    )
    
    valores <- c(ganhou, perdeu)
    
    limite_y <- max(1, max(valores) + 1)
    
    barplot(
      valores,
      names.arg = c("Ganhou", "Perdeu"),
      main = "Estratégia: Mantendo",
      ylab = "Número de partidas",
      ylim = c(0, max(1, valores) + 1),
      col = c("#6F968B", "#A66A6E"),
      yaxt = "n"
    )
    
    axis(2, at = 0:max(valores))
  })
  
  
  # Interface
  tagList(
    
    h4("Eficiência das Estratégias Escolhidas"),
    
    fluidRow(
      
      column(
        width = 6,
        plotOutput(
          session$ns("grafico_trocando")
        )
      ),
      
      column(
        width = 6,
        plotOutput(
          session$ns("grafico_mantendo")
        )
      )
      
    )
  )
}