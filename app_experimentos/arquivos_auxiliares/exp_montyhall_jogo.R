exp_montyhall_jogo <- function(input, output, session) {
  
  # infos atuais "vazias
    premio <- reactiveVal(NULL)
    escolha_1 <- reactiveVal(NULL)
    portas_abertas <- reactiveVal(c(FALSE, FALSE, FALSE)) #FALSE - porta fechado, vetor das 3 portas
    decisao <- reactiveVal(NULL)
    escolha_final <- reactiveVal(NULL)
    resultado <- reactiveVal(NULL)
    
    
    
  #########   função para partidas ###########
    
    #primeira partida com tudo zerado
    nova_partida <- function(){
      
      premio(sample(1:3, 1))
      escolha_1(NULL)
      portas_abertas(c(FALSE, FALSE, FALSE))
      decisao(NULL)
      escolha_final(NULL)
      resultado(NULL)
      
      shinyjs::removeClass(
        id = session$ns("porta_1"),
        class = "porta-escolhida"
      )
      
      shinyjs::removeClass(
        id = session$ns("porta_2"),
        class = "porta-escolhida"
      )
      
      shinyjs::removeClass(
        id = session$ns("porta_3"),
        class = "porta-escolhida"
      )
    }
    
    nova_partida()
  
    #reativo para nova partida quando o usuario apertar o botao
    observeEvent(input$nova_partida,{
      nova_partida()
    })
    
    
    
  ########### qual porta abre após primeira escolha ############
    
    
    abrir_porta_monty <- function(){
      
      req(!is.null(premio()))
      req(!is.null(escolha_1()))
      
      
        
    portas_possiveis <- setdiff(  #setdiff compara objetos assimetricos, aqui, o sorteio entre 1,2,3 e os
                                  #resultados contidos em escolha_1 e premio 
      1:3,
      c(escolha_1(), premio())
    )
    
    print(portas_possiveis)  #debug
    
    #quando escolha_1 != de premio só existe uma porta possível para ser aberta
    #porem, quando a escolha_1 = premio
    
    porta_abrir <- sample(-portas_possiveis, 1)
    
    
    abertas <- portas_abertas()
    abertas[-porta_abrir] <- TRUE  #uma uma porta dentro das opções possíveis
    portas_abertas(abertas) #grava a nova sequencia de TRUE/FALSE
    
    }
    
    ########   primeira escolha usuario ###########
    
    
    #função para observa e guardar a escolha inicial do usuario
    escolha_porta <- function(porta){
      req(is.null(escolha_1()))
      
      escolha_1(porta)
      
      abrir_porta_monty()
      
      shinyjs::disable(session$ns("porta_1"))
      shinyjs::disable(session$ns("porta_2"))
      shinyjs::disable(session$ns("porta_3"))
    }
    
    #observe para as 3 portas (identificar a escolha inicial do usuario)
    observeEvent(input$porta_1,{
      
      shinyjs::addClass(
        id = session$ns("porta_1"),
        class = "porta-escolhida"
      )
      
      escolha_porta(1)
      
      
    })
    
    observeEvent(input$porta_2,{
      
      shinyjs::addClass(
        id = session$ns("porta_2"),
        class = "porta-escolhida"
      )
      
      escolha_porta(2)
      
      
    })
    
    observeEvent(input$porta_3,{
      shinyjs::addClass(
        id = session$ns("porta_3"),
        class = "porta-escolhida"
      )
      
      escolha_porta(3)
      
     
    })
    
    
    
    
    
    #### finalizar jogo - segunda etapa ##########
    
    finalizar_partida <- function(tipo){
      
      decisao(tipo)
      
      if(tipo == "manter"){
          escolha_final(escolha_1())
      }else{
          porta_restante <- setdiff(
          which(!portas_abertas()),
          escolha_1()
        )
        
        escolha_final(porta_restante)
        
      }
      
      if(escolha_final() == premio()){
        resultado("Ganhou!")
      }else{
        resultado("Perdeu!")
      }
      
    }
    
    observeEvent(input$manter,{
      
      finalizar_partida("manter")
      
    })
    
    observeEvent(input$trocar,{
      
      finalizar_partida("trocar")
      
    })
    
    
    #Outputs
    
    #outputs para mudança das imagens
    #PORTA 1
    output$imagem_porta_1 <- renderUI({
      
      if (portas_abertas()[1]) {
        img(src = "cabra.png",  width = "150px")
        
      } else if (!is.null(resultado()) && premio() == 1) {
        img(src = "premio.png",  width = "150px" )
        
      } else {
        img(src = "porta.png",width = "150px")
        
      }
    })
    
    #PORTA 2
    output$imagem_porta_2 <- renderUI({
      
      if (portas_abertas()[2]) {
        img(src = "cabra.png", width = "150px")
        
      } else if (!is.null(resultado()) && premio() == 2) {
        img(src = "premio.png",  width = "150px" )
        
      } else {
        img(src = "porta.png",width = "150px")
        
      }
    })
    
    #porta 3
    output$imagem_porta_3 <- renderUI({
      
      if (portas_abertas()[3]) {
        img(src = "cabra.png", width = "150px")
        
      } else if (!is.null(resultado()) && premio() == 3) {
        img(src = "premio.png",  width = "150px" )
        
      } else {
        img(src = "porta.png",width = "150px")
        
      }
    })
    
    #saída para botões manter/trocar funcionarem apenas após a escolha_1 ter sido feita
    output$porta_escolhida <- reactive({
      !is.null(escolha_1())
    })
    
    outputOptions(
      output,
      "porta_escolhida",
      suspendWhenHidden = FALSE
    )
    
    
    #saída para teste 
    output$debug <- renderPrint({
      
      list(
        
        premio = premio(),
        
        escolha_1 = escolha_1(),
        
        portas_abertas = portas_abertas(),
        
        decisao = decisao(),
        
        escolha_final = escolha_final(),
        
        resultado = resultado()
        
      )
      
    })
  
    
  # tagList
    
    tagList(
      tags$style(HTML("
          .btn-porta {
           background: transparent;
           border: none;
           padding: 0;
          }

          .btn-porta:hover {
           background: transparent;
          }

          .btn-porta.porta-escolhida {
           opacity: 0.3;
  }
      ")),
   
      h3("Experimento de Monty Hall"),
      br(),
      p("Escolha uma das três portas. 
      Atrás de uma delas está o prêmio."
      ),
      
      br(),
      
      fluidRow( 
      
        column(width = 4,
          actionButton( session$ns("porta_1"),
                        uiOutput(session$ns("imagem_porta_1")),
                       class = "btn-porta"
          )
        ),
        column(width = 4,
          actionButton(session$ns("porta_2"),
                       uiOutput(session$ns("imagem_porta_2")),
                       class = "btn-porta"
          )
        ),
        column(width = 4,
          actionButton( session$ns("porta_3"),
                        uiOutput(session$ns("imagem_porta_3")),
                        class = "btn-porta"
          )
          
        )
        
        
      ),
      br(),
      br(),
      
      conditionalPanel(
        condition = sprintf("output['%s']", session$ns("porta_escolhida")),
        
        div(
          class = "text-center",
        
         actionButton(
          session$ns("manter"),
          "MANTER JOGADA"
          ),
        
          actionButton(
         session$ns("trocar"),
         "TROCAR JOGADA"
          )
        )
      ),
      
      br(),
      br(),
      
      fluidRow(
        column( width = 12, class = "text-end",
          actionButton(session$ns("nova_partida"),
                       "NOVA PARTIDA",
                        class = "btn-primary"
          )
        )
      ),
      
      
      verbatimTextOutput(session$ns("debug")),
      
      
      br(),
      br(),
      br(),
      br(),
      
      h5("Estatísticas"),
      textOutput( session$ns("estatisticas_monty")
      )
    )
    
}