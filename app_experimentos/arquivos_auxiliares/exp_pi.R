exp_pi <- function(input, output, session){


  amostra <- reactiveVal()


  gerar_nova_amostra <- function() {
    n_total <- 20000
    x <- runif(n_total, -1, 1)
    y <- runif(n_total, -1, 1)
    dentro <- x^2 + y^2 <= 1
    nova_amostra <- data.frame(x = x, y = y, dentro = dentro, indice = 1:n_total)
    amostra(nova_amostra)
  }


  observe({
    if(is.null(amostra())) {
      gerar_nova_amostra()
    }
  })


  observeEvent(input$nova_amostra, {
    gerar_nova_amostra()
    updateSliderInput(session, "n_pi", value = 100)
  })


  dados_visiveis <- reactive({
    req(amostra(), input$n_pi)
    df <- amostra()
    df[1:input$n_pi, ]
  })


  dados_convergencia <- reactive({
    req(amostra())
    df <- amostra()
    pontos <- seq(0, input$n_pi, by = 100)
    estimativas <- sapply(pontos, function(n) {
      dentro_acum <- sum(df$dentro[1:n])
      4 * dentro_acum / n
    })

    data.frame(
      N = pontos,
      Estimativa = estimativas,
      Pi_real = pi
    )
  })



      output$estimativa_pi <- renderText({

        req(dados_visiveis())

        df <- dados_visiveis()

        M <- sum(df$dentro)
        N <- nrow(df)

        pi_estimativa <- 4 * M/N

        paste("Estimativa de π:", round(pi_estimativa,5), "| π real:", round(pi,5)
        )

      })


      output$grafico_pi <- renderPlot({

        req(dados_visiveis())

        df <- dados_visiveis()

        plot(
          df$x, df$y,
          col = ifelse(df$dentro,"#2F5D50","#67161C"),
          pch = 16,
          cex = 0.5,
          asp = 1,
          
          xlim = c(-1, 1),
          ylim = c(-1, 1),
          
          xaxs = "i",
          yaxs = "i",
          
          xlab = "x",
          ylab = "y",
          main = "Simulação"
        )
        
       

        symbols(0,0,  circles = 1, add = TRUE,inches = FALSE)

      })


      output$grafico_convergencia <- renderPlot({
        req(dados_convergencia(), input$n_pi)

        df_conv <- dados_convergencia()
        n_atual <- input$n_pi


        plot(df_conv$N, df_conv$Estimativa,
             type = "l",
             col = "#2F5D50",
             lwd = 2,
             xlab = "Número de pontos (n)",
             ylab = "Estimativa de π",
             main = "Convergência da Estimativa de π",
             ylim = c(2.8, 3.4),
             xlim = c(0, max(df_conv$N)),
             xaxs = "i")
        


        abline(h = pi, col = "#67161C", lwd = 2, lty = 2)

        estimativa_atual <- 4 * sum(dados_visiveis()$dentro) / n_atual
        points(n_atual, estimativa_atual, col = "#FF6600", pch = 19, cex = 2)

        x_pos <- max(df_conv$N) * 0.72
        y_pos <- 3.35



        text(x_pos, y_pos,
             labels = paste0("π ≈ ", round(estimativa_atual, 6)),
             col = "#FF6600", font = 2, cex = 1.2, adj = 0)

        text(x_pos, y_pos - 0.09,
             labels = paste0("n = ", format(n_atual, big.mark = ",")),
             col = "#666666", cex = 0.85, adj = 0)

        legend("bottomright",
               legend = c("Estimativa", "π real", "Posição atual"),
               col = c("#2F5D50", "#67161C", "#FF6600"),
               lty = c(1, 2, NA),
               lwd = c(2, 2, NA),
               pch = c(NA, NA, 19),
               bty = "n")


      })

      tagList(
        br(),
        p(" Observe a convergência no gráfico para o valor real de π à medida que o número de pontos aumenta"),
        
        actionButton(session$ns("nova_amostra"), "Nova Amostra",
                     style = "margin-bottom: 20px; background-color: #4CAF50; color: white;"),
        sliderInput(session$ns( "n_pi"), "Número de pontos gerados:",  min = 0, max = 20000,  value = 100,
                    step = 100, sep = "",animate = TRUE, width = "400px"),
        
        
        
        textOutput(session$ns( "estimativa_pi")),

      
      fluidRow(
        
        column(
          width = 6,
          plotOutput(session$ns("grafico_pi"), height = "300px")
        ),
        
        column(
          width =6,
          tagList(
            plotOutput(session$ns("grafico_convergencia"), height = "300px")
          )
        )
        
      )
      )


}

