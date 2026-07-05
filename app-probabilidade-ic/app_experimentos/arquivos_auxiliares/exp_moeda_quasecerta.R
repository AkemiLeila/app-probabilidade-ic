exp_moeda_cqc <- function(input, output, session){
  
  tagList(
    br(),
    
    actionButton(session$ns("nova_simulacao"),
                 "Nova Simulação"),
    
    br(),
    br(),
    plotOutput("grafico_cqc", height = "600px")
  )
  
}