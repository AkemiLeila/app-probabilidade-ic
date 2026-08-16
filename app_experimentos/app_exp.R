


arquivos_aux_exp <- list.files("app_experimentos/arquivos_auxiliares", full.names = TRUE)
sapply(arquivos_aux_exp, source)


exp_ui <- function(id){

  fluidPage(
    
    useShinyjs(),
    
    withMathJax(),
    
    sidebarLayout(
             sidebarPanel( input_exp(id) ),
             
              mainPanel(
                          conditionalPanel(
                          condition = paste0("input['",NS(id, "tipo_exp"), "'] == ''"),
                                         texto_objetivo_casos),
                          conditionalPanel(
                          condition = paste0("input['", NS(id, "tipo_exp"),  "'] == 'Monte Carlo — π'" ),
                          ),
                          conditionalPanel(
                          condition = paste0("input['", NS(id, "tipo_exp"),  "'] == 'Lançamento Moeda'" ),
                          ),
                          conditionalPanel(
                            condition = paste0("input['", NS(id, "tipo_exp"),  "'] == 'Monty Hall'" ),
                          ),
                          
                          uiOutput(NS(id,"painel_exp"))
                        )
    )
  )
}



exp_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    estatisticas <- reactiveVal(
      data.frame(
        resultado = character(),
        decisao = character(),
        stringsAsFactors = FALSE
      )
    )
    
    registrar_partida <- function(resultado, decisao) {
      
      nova_linha <- data.frame(
        resultado = resultado,
        decisao = decisao,
        stringsAsFactors = FALSE
      )
      
      estatisticas(
        rbind(estatisticas(), nova_linha)
      )
    }

  output$painel_exp <- renderUI({
    req(input$tipo_exp != "")
    switch(
      input$tipo_exp,
    
      "Monte Carlo — π" = tabsetPanel(
        tabPanel("Estimativa de π" , exp_pi(input, output, session)),
        tabPanel("Teoria" , exp_pi_teoria(input, output, session))
        ),
      
      "Lançamento Moeda" = tabsetPanel(
        tabPanel( "Jogo",exp_moeda_simulacao(input, output, session)), 
        tabPanel("Histórico de Partidas", exp_moeda_historico(input, output, session)),
        tabPanel("Teoria Jogo", exp_moeda_teoria_cp(input, output, session)),
        tabPanel("Simulação", exp_moeda_cqc(input,output,session)),
        tabPanel("Teoria Simulação", exp_moeda_teoria_cqc(input, output, session))
      ),
      
      "Monty Hall" = tabsetPanel(
        tabPanel("Jogo",exp_montyhall_jogo(input,output,session, registrar_partida)),
        tabPanel("Estatísticas do Jogo", exp_montyhall_estat(input, output, session, estatisticas)),
        tabPanel("Teoria Probabilidade Condicional", exp_montyhall_teoria(input, output, session))
      )
      
      
    )

  })
  
}
)}
