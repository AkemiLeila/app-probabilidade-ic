


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
                          condition = paste0("input['", NS(id, "tipo_exp"),  "'] == 'PI'" ),
                          ),
                          conditionalPanel(
                          condition = paste0("input['", NS(id, "tipo_exp"),  "'] == 'Moeda'" ),
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

  output$painel_exp <- renderUI({
    req(input$tipo_exp != "")
    switch(
      input$tipo_exp,
    
      "PI" = exp_pi(input, output, session),
      
      
      "Moeda" = tabsetPanel(
        tabPanel( "Simulação",exp_moeda_simulacao(input, output, session)), 
        tabPanel("Histórico de Partidas", exp_moeda_historico(input, output, session)),
        tabPanel("Convergencia Quase certa", exp_moeda_cqc(input,output,session))
      ),
      
      "Monty Hall" = tabsetPanel(
        tabPanel("Jogo",exp_montyhall_jogo(input,output,session)),
        tabPanel("Simulação", exp_montyhall_simulacao(input, output, session))
      )
      
      
    )

  })
  
}
)}
