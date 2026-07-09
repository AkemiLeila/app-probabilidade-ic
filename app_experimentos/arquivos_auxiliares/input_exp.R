input_exp <- function(id){
  
  
  tagList(
      selectInput(NS(id,"tipo_exp"), "Experimentos:", choices = c("", "PI", "Moeda")),


      conditionalPanel(
      condition = paste0("input['", NS(id, "tipo_exp"), "'] == 'PI'"),
      helpText("Simulação de pontos aleatórios em um alvo quadrado para calcular a razão de acertos
            no círculo circunscrito e estimar, a partir dela, o valor de PI")
      ),
      
      conditionalPanel(
      condition = paste0("input['", NS(id, "tipo_exp"), "'] == 'Moeda'"),
      helpText("Simulação de probabilidade para uma moeda honesta")
     
      )
)}