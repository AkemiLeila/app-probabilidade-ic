exp_montyhall_jogo <- function(input, output, session, registrar_partida) {
  
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
      
      shinyjs::enable(session$ns("manter"))
      shinyjs::enable(session$ns("trocar"))
      
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
    
   # print(portas_possiveis)  #debug
    
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
      
      shinyjs::addClass(
        id = session$ns(paste0("porta_", porta)),
        class = "porta-escolhida"
      )
      
      abrir_porta_monty()

      shinyjs::disable(session$ns("porta_1"))
      shinyjs::disable(session$ns("porta_2"))
      shinyjs::disable(session$ns("porta_3"))
    }
    
    #observe para as 3 portas (identificar a escolha inicial do usuario)
    observeEvent(input$porta_1,{
      escolha_porta(1)
    })
    
    observeEvent(input$porta_2,{
      escolha_porta(2)
    })
    
    observeEvent(input$porta_3,{
      escolha_porta(3)
    })
    
    
    
    
    
    #### finalizar jogo - segunda etapa ##########
    
    finalizar_partida <- function(tipo){
      
      req(is.null(resultado()))
      
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
      
      registrar_partida(
        resultado = resultado(),
        decisao = decisao()
      )
      
      shinyjs::disable(session$ns("manter"))
      shinyjs::disable(session$ns("trocar"))
      
     
      
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
      
      escolhida <- !is.null(escolha_1()) && escolha_1() == 1
      
      if (portas_abertas()[1]) {
        img(
          src = "cabra.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
      } else if (!is.null(resultado()) && premio() == 1) {
        img(
          src = "premio.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
      } else {
        img(
          src = "porta.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
      }
    })
    
    #PORTA 2
    output$imagem_porta_2 <- renderUI({
      
      escolhida <- !is.null(escolha_1()) && escolha_1() == 2
      
      if (portas_abertas()[2]) {
        
        img(
          src = "cabra.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
      } else if (!is.null(resultado()) && premio() == 2) {
        
        img(
          src = "premio.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
      } else {
        
        img(
          src = "porta.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
      }
    })
    
    #porta 3
    output$imagem_porta_3 <- renderUI({
      
      escolhida <- !is.null(escolha_1()) && escolha_1() == 3
      
      if (portas_abertas()[3]) {
        
        img(
          src = "cabra.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
      } else if (!is.null(resultado()) && premio() == 3) {
        
        img(
          src = "premio.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
      } else {
        
        img(
          src = "porta.png",
          width = "150px",
          style = if (escolhida) "opacity: 0.4;" else "opacity: 1;"
        )
        
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
    
    #teste para box com infos
    output$info_jogada <- renderUI({
      
      if (is.null(escolha_1()) || !any(portas_abertas())) {
        return(NULL)
      }
      porta_aberta <- which(portas_abertas())
      
      div(
        class = "box-info-jogada",
        
        p(
          strong("Primeira porta selecionada: "),
          escolha_1()
        ),
        
        p(
          strong("Monty Hall abriu a porta: "),
          porta_aberta
        ),
        p("Agora você pode manter sua escolha ou trocar de porta.")
        
      )
    })
    
    output$resultado_jogada <- renderUI({
      
      req(!is.null(resultado()))
      
      if (resultado() == "Ganhou!") {
        
        div(
          class = "box-resultado ganhou",
          
          h3("Parabéns! Você ganhou!"),
          
          p(
            strong("Porta selecionada: "),
            escolha_final()
          ),
          
          p(
            strong("Prêmio na porta: "),
            premio()
          )
        )
        
      } else {
        
        div(
          class = "box-resultado perdeu",
          
          h3("Você perdeu!"),
          
          p(
            strong("Porta selecionada: "),
            escolha_final()
          ),
          
          p(
            strong("Prêmio na porta: "),
            premio()
          )
        )
      }
    })
        
        
    #saída para teste 
    # output$debug <- renderPrint({
    #   
    #   list(
    #     
    #     premio = premio(),
    #     
    #     escolha_1 = escolha_1(),
    #     
    #     portas_abertas = portas_abertas(),
    #     
    #     decisao = decisao(),
    #     
    #     escolha_final = escolha_final(),
    #     
    #     resultado = resultado()
    #     
    #   )
    #   
    # })
  
    
  # tagList
    
    tagList(
      tags$style(HTML("
 
.porta-container {
  text-align: center;
}

.portas-row {
  margin-top: 5px;
  margin-bottom: 5px;
}

.numero-porta {
  font-size: 20px;
  font-weight: bold;
  margin-bottom: 3px;
  text-align: center;
}

.btn-porta {
  background: transparent;
  border: none;
  padding: 0;
  margin: 0 auto;
  display: block;
}

.btn-porta:hover {
  background: transparent;
}

.btn-porta.porta-escolhida img {
  opacity: 0.3;
}

.botao-nova-partida {
  position: fixed;
  bottom: 20px;
  right: 30px;
  z-index: 1000;
}

.box-info-jogada {
  max-width: 400px;
  margin: 5px auto 15px;
  padding: 12px 20px;
  text-align: center;
  border-radius: 12px;
  background: #f5f5f5;
  border: 1px solid #ddd;
  box-shadow: 0 3px 10px rgba(0,0,0,0.10);
}

.box-info-jogada h4 {
  margin-top: 0;
  margin-bottom: 15px;
}

.box-info-jogada p {
  margin: 8px 0;
}


.box-resultado {
  max-width: 300px;
  margin: 30px auto;
  padding: 25px 30px;
  text-align: center;
  border-radius: 15px;
  box-shadow: 0 4px 15px rgba(0,0,0,0.15);
}

.box-resultado h3 {
  margin-top: 0;
  margin-bottom: 20px;
}

.box-resultado p {
  margin: 8px 0;
  font-size: 17px;
}

.box-resultado.ganhou {
  border: 2px solid #198754;
  background-color: #eaf7ee;
}

.box-resultado.perdeu {
  border: 2px solid #dc3545;
  background-color: #fbeaec;
}
   ")),
      br(),
      h5("Escolha uma das portas abaixo:"),
    
      
      fluidRow( 
        class = "portas-row",
        column(width = 4,
               div(
                 class = "porta-container",
                 div( class = "numero-porta", "1"),
                 actionButton( session$ns("porta_1"),
                        uiOutput(session$ns("imagem_porta_1")),
                       class = "btn-porta"
          )
        )
        ),
        
        column(width = 4,
               div(
                 class = "porta-container",
                 div( class = "numero-porta", "2"),
                 actionButton(session$ns("porta_2"),
                       uiOutput(session$ns("imagem_porta_2")),
                       class = "btn-porta"
          )
        )
        ),
        column(width = 4,
               div(
                 class = "porta-container",
                 div( class = "numero-porta", "3"),
                 actionButton( session$ns("porta_3"),
                        uiOutput(session$ns("imagem_porta_3")),
                        class = "btn-porta"
          )
          
        )
        )
        
      ),
     
      
      conditionalPanel(
        condition = sprintf("output['%s']", session$ns("porta_escolhida")),
        
        uiOutput(session$ns("info_jogada")),
        
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
      uiOutput(session$ns("resultado_jogada")),
      
    
      
      div(
        class = "botao-nova-partida",
        actionButton(
          session$ns("nova_partida"),
          "NOVA PARTIDA",
          class = "btn-primary"
        )
      )
      
      
      #verbatimTextOutput(session$ns("debug")),
     
    
      
    )
    
}