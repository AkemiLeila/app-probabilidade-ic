
library(ggplot2)


arquivos_comp <- list.files("app_comparacoes/arquivos_auxiliares", full.names = TRUE)
sapply(arquivos_comp, source)



comparacoes_ui <- function(id){
  
  fluidPage(
    
    withMathJax(),
    
    
    sidebarLayout(
      sidebarPanel(
        
        input_comparacoes(id)
        
      ),
      
      mainPanel(
        conditionalPanel(
          condition = paste0("input['", NS(id, "tipo_comp"), "'] == ''"),
          texto_objetivo_comp
        ),
        uiOutput(NS(id,"painel_comparacao"))
      )
    )
    
  )
}



comparacoes_server <- function(id){
  
  moduleServer(id, 
               function(input, output, session) {
                 
                 
                 
                 #evita executar algo quando "" vazio - desbug
                 # observeEvent(input$tipo_comp, {
                 #   if (input$tipo_comp != "") {
                 #     print("Comparação selecionada")
                 #   } 
                 # })
                 
                 
                 
                 #escolha da comparação nos auxiliares
                 output$painel_comparacao <- renderUI({
                   
                   req(input$tipo_comp != "")
                   
                   switch(
                     input$tipo_comp,
                     
                     "Exponencial x Gama" =
                       comp_expo_gama(input, output, session),
                     
                     "Binomial x Poisson" =
                       comp_binomial_poisson(input, output, session),
                     
                     "Normal x Cauchy x T-Student" =
                       comp_normal_cauchy_tstudent(input, output, session),
                     
                     "Beta x Uniforme" =
                       comp_beta_uniforme(input, output, session),
                     
                     "Normal x Qui-quadrado" =
                       comp_normal_quiquadrado(input, output, session),
                     
                     "Qui-quadrado x Gama x Exponencial" =
                       comp_qui_gama_exp(input, output, session)
                   )
                 })
               })}
