exp_moeda_historico <- function(input, output, session){
  
  
  arquivo_dados <- "partidas_moeda.csv"
  
  
  carregar_dados <- function(arquivo) {
    
    if(file.exists(arquivo)) {
      read.csv(arquivo, stringsAsFactors = FALSE)
    } else {
      data.frame(
        
        Partida = integer(),
        Data_Hora = character(),
        Total_Sorteios = integer(),
        Total_Caras = integer(),
        Total_Coroas = integer(),
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
  
  
  
  
  
  
  
  
  #Saídas para a aba histórico de partidas
  output$tabela_historico <- DT::renderDT({
    
    dados <- banco_dados()
    
    req(nrow(dados) > 0)
    
    dados$Proporcao_Caras <-  round(dados$Total_Caras / dados$Total_Sorteios, 4   )
    
    dados <- dados[ order(dados$Partida, decreasing = TRUE),]
    
    names(dados) <- c("Partida","Data/Hora","Total de Sorteios",
                      "Caras","Coroas", "Proporção de Caras")
    
    DT::datatable(
      dados,
      rownames = FALSE,
      options = list(
        pageLength = 30,
        autoWidth = FALSE,
        scrollX = TRUE,
        order = list(list(0, "desc")),
        searching = FALSE,
        language = list(
          decimal = ",",
          thousands = ".",
          lengthMenu = "Mostrar _MENU_ entradas",
          zeroRecords = "Nenhum registro encontrado",
          info = "Mostrando _START_ até _END_ de _TOTAL_ entradas",
          infoEmpty = "Nenhuma partida disponível",
          infoFiltered = "(filtrado de _MAX_ entradas totais)"
          
        )
      )
    )
  })
  
  
  tagList(
    
    h3("Histórico das Partidas"),
    
    DT::DTOutput(
      session$ns("tabela_historico")
    )
    
  )

  
}