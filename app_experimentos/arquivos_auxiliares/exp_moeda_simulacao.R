exp_moeda_simulacao <- function(input, output, session){
  
  arquivo_dados <- "partidas_moeda.csv"
  
  
  

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
  
  
  observe({
    n <- length(historico_resultados())
    
    if (n < 10) {
      shinyjs::disable("encerrar_partida")
    } else {
      shinyjs::enable("encerrar_partida")
    }
  })
  
  observeEvent(input$encerrar_partida, {
    
    req(length(historico_resultados()) > 0)
    
    if (length(historico_resultados()) < 10) {
      showNotification(
        "Você precisa de pelo menos 10 lançamentos para encerrar a partida.",
        type = "error"
      )
      return(NULL)
    }
    
    total <- length(historico_resultados())
    caras <- sum(historico_resultados() == "cara")
    coroas <- sum(historico_resultados() == "coroa")
      
      if (file.exists(arquivo_dados)) {
        
        dados_existentes <- read.csv(
          arquivo_dados,
          stringsAsFactors = FALSE
        )
        
        if (nrow(dados_existentes) == 0) {
          numero_partida <- 1
        } else {
          numero_partida <- max(dados_existentes$Partida) + 1
        }
        
      } else {
        
        dados_existentes <- data.frame()
        
        numero_partida <- 1
        
      }
      

      data_hora <- format(Sys.time(), "%d/%m/%Y %H:%M:%S" )
      
   
      nova_partida <- data.frame(
        
        Partida = numero_partida,
        Data_Hora = data_hora,
        Total_Sorteios = total,
        Total_Caras = caras,
        Total_Coroas = coroas,
        
        stringsAsFactors = FALSE
        
      )
      
    
      dados_atualizados <- rbind( dados_existentes, nova_partida)
      
   
      write.csv(dados_atualizados, arquivo_dados, row.names = FALSE)
     
      zerar_partida()
    
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
  
  output$resultado_moeda <- renderUI({
    
    req(resultado_atual())
    
    div(
      style = "text-align: center;",
      
      if (resultado_atual() == "cara") {
        
        img(
          src = "moeda_cara.png",
          width = "180px"
        )
        
      } else {
        
        img(
          src = "moeda_coroa.png",
          width = "180px"
        )
        
      }
    )
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
  
  
  output$moeda_atual <- renderUI({
    
    req(resultado_atual())
    
    resultado <- resultado_atual()
    
    classe <- if (resultado == "cara") {
      "moeda-atual moeda-cara"
    } else {
      "moeda-atual moeda-coroa"
    }
    
    div(
      class = "moeda-container",
      
      div(
        class = classe,
        toupper(resultado)
      )
    )
  })
  
  output$historico_visual <- renderUI({
    
    req(length(historico_resultados()) > 0)
    
    div(
      class = "historico-moedas",
      
      paste(
        historico_resultados(),
        collapse = " | "
      )
    )
  })
  
  #taglist com textos e diagramação
  tagList(
    
    tags$style(HTML("

  .moeda-container {
    text-align: center;
    margin: 10px auto 25px;
  }

  .moeda-atual {
    width: 120px;
    height: 120px;
    border-radius: 50%;
    margin: 15px auto;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 18px;
    font-weight: 600;
    letter-spacing: 1px;
    box-shadow: 0 3px 10px rgba(0,0,0,0.15);
  }

  .moeda-cara {
    background-color: #e8f0ed;
    border: 3px solid #2F5D50;
    color: #2F5D50;
  }

  .moeda-coroa {
    background-color: #f5e9ea;
    border: 3px solid #67161C;
    color: #67161C;
  }

.historico-moedas {
  text-align: center;
  margin: 20px auto;
  max-width: 750px;
  line-height: 1.8;
  color: #343a40;
  font-size: 16px;
}
  

  .historico-titulo {
    text-align: center;
    margin-top: 20px;
    margin-bottom: 8px;
    color: #343a40;
    font-weight: 600;
  }

  .botoes-moeda {
    text-align: center;
    margin-top: 15px;
  }

")),
    
    br(),
    h4("Lançamento de uma Moeda Honesta"),
   
    p(
      "Observe o resultado de cada lançamento e acompanhe a evolução ",
      "da proporção de caras ao longo da simulação."
    ),
    
    br(), 
    uiOutput(
      session$ns("resultado_moeda")
    ),
    br(),
    h3(
      style = "text-align: center;",
      "Resultados dos últimos lançamentos"
    ),
    
    uiOutput(
      session$ns("historico_visual")
    ),
    
    br(),
   
    fluidRow(
      
      column(
        width = 6,
        
        div(
          style = "text-align: center;",
          
          tableOutput(
            session$ns("tabela_moeda")
          )
        )
      ),
      
      column(
        width = 6,
        
        div(
          style = "text-align: center; padding-top: 10px;",
          
          actionButton(
            session$ns("novo_lancamento"),
            "Novo lançamento",
            class = "btn-primary"
          ),
        
        br(), br(),
        
        actionButton(
          session$ns("encerrar_partida"),
          "Encerrar Partida",
          class = "btn-danger"
        )
      )
      
    ),
    
    br(), br(),
    plotOutput(session$ns("grafico_convergencia"), height = "350px"),

    
  )
  
)
  
  
}