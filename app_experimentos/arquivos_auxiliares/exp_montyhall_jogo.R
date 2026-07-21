exp_montyhall_jogo <- function(input, output, session) {
  # 
  # # infos atuais "vazias
  # 
  # premio <- 
  # 
  # escolha_1 <- 
  # 
  # qual_porta_aberta <- 
  # 
  # decisao <- 
  #   
  # escolha_2 <- 
  # 
  # resultado <- 
  # 
  #   
  #   
  # #função para partidas
  #   
  #   nova_partida <- function(){
  #     
  #   # sorteio aleatório do premio 
  #   premio_atual <- sample(1:3, size = 1)
  #   
  #   #resetar infos anteriores
  #   
  #   
  #   #guarda premio da nova partida
  #   premio(premio_atual)
  #   
  #   
  #   }
  # 
  # 
  # # primeira partida
  #   observe({
  #     if(is.null(premio())
  #        ){nova_partida()}
  #   })
  #   
  
  # outputs
  
    
    
    
    
  # tagList
    
    tagList(
   
      h3("Experimento de Monty Hall"),
      br(),
      p("Escolha uma das três portas. 
      Atrás de uma delas está o prêmio."
      ),
      
      br(),
      
      fluidRow( 
      
        column(width = 4,
          actionButton( session$ns("porta_1"),
                       "PORTA 1",
                       class = "btn-secondary"
          )
        ),
        column(width = 4,
          actionButton(session$ns("porta_2"),
                       "PORTA 2",
                      class = "btn-secondary"
          )
        ),
        column(width = 4,
          actionButton( session$ns("porta_3"),
                        "PORTA 3",
                        class = "btn-secondary"
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
      
      br(),
      br(),
      br(),
      br(),
      
      h5("Estatísticas"),
      textOutput( session$ns("estatisticas_monty")
      )
    )
    
}