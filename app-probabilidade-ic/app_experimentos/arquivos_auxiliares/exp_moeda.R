exp_moeda <- function(input, output, session){
  
  arquivo_dados <- "partidas_moeda.csv"
  
  
  carregar_dados <- function() {
    if(file.exists(arquivo_dados)) {
      read.csv(arquivo_dados, stringsAsFactors = FALSE)
    } else {
      data.frame(
        Nome = character(),
        Total_Sorteios = integer(),
        Total_Caras = integer(),
        Total_Coroas = integer(),
        Data_Hora = character(),
        stringsAsFactors = FALSE
      )
    }
  }
  
  #Vigia e atualiza
  banco_dados <- reactiveFileReader(
    intervalMillis = 1000,  
    session = session,
    filePath = arquivo_dados,
    readFunc = carregar_dados
  )
  
  

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
  
  
  
  zerar_partida <- function() {
    primeiro_lancamento <- sample(c("cara", "coroa"), 1)
    resultado_atual(primeiro_lancamento)
    historico_resultados(primeiro_lancamento)
  }
  
  observeEvent(input$encerrar_partida, {
    nome <- input$nome_jogador
    
    if(!is.null(nome) && nome != "") {
      total <- length(historico_resultados())
      caras <- sum(historico_resultados() == "cara")
      coroas <- sum(historico_resultados() == "coroa")
      
      nova_partida <- data.frame(
        Nome = nome,
        Total_Sorteios = total,
        Total_Caras = caras,
        Total_Coroas = coroas,
        stringsAsFactors = FALSE
      )
      
      if(file.exists(arquivo_dados)) {  #salva no csv
        dados_existentes <- read.csv(arquivo_dados, stringsAsFactors = FALSE)
        dados_atualizados <- rbind(dados_existentes, nova_partida)
      } else {
        dados_atualizados <- nova_partida
      }
      
      write.csv(dados_atualizados, arquivo_dados, row.names = FALSE)
     
      zerar_partida()
    }
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
  
  
  #Saídas para a aba histórico de partidas
  output$tabela_historico <- renderTable({
    dados <- banco_dados()
    if(nrow(dados) == 0) {
      return(data.frame(Mensagem = "Nenhuma partida encerrada ainda"))
    }
    dados[, c("Jogador", "Caras", "Coroas", "Total_Sorteios", "Data_Hora")]
  })
  
  output$grafico_convergencia_historico <- renderPlot({
    dados <- banco_dados()
    req(nrow(dados) > 0)
  
    dados$Total_Caras_Acumulado <- cumsum(dados$Caras)
    dados$Total_Sorteios_Acumulado <- cumsum(dados$Total_Sorteios)
    dados$Proporcao_Acumulada <- dados$Total_Caras_Acumulado / dados$Total_Sorteios_Acumulado
    dados$Partida <- 1:nrow(dados)
  
    plot(
      dados$Partida, dados$Proporcao_Acumulada,
      type = "b",
      col = "#2F5D50",
      lwd = 2,
      pch = 19,
      ylim = c(0, 1),
      xlab = "Número de partidas encerradas",
      ylab = "Proporção acumulada de caras",
      main = "Convergência da proporção de caras (todas as partidas)"
    )
    abline(h = 0.5, col = "#67161C", lty = 2, lwd = 2)
    grid()
  })
  
  
  
  
  #taglist com textos e diagramação
  tagList(
    
    h3("Lançamento de uma Moeda Honesta"),
    br(), 
   
    textOutput(
      session$ns("resultado_moeda")
    ),
    br(),
    textInput(session$ns("nome_jogador"), 
              "Seu nome:", 
              placeholder = "Digite seu nome aqui"),
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
    ),
    
    
  )
  

  
  
}