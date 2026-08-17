input_exp <- function(id){
  
  
  tagList(
      selectInput(NS(id,"tipo_exp"), "Experimentos:", choices = c("", "Monte Carlo — π", "Lançamento Moeda", "Monty Hall")),


      conditionalPanel(
      condition = paste0("input['", NS(id, "tipo_exp"), "'] == 'Monte Carlo — π'"),
      helpText("Experimento de Monte Carlo para estimar π")
      ),
      
      conditionalPanel(
      condition = paste0("input['", NS(id, "tipo_exp"), "'] == 'Lançamento Moeda'"),
      helpText("Experimento aleatório: probabilidade para uma moeda honesta")
     
      ),
      conditionalPanel(
        condition = paste0("input['", NS(id, "tipo_exp"), "'] == 'Monty Hall'"),
        helpText("Experimento de probabilidade condicional para o problema de Monty Hall")
      )
)}