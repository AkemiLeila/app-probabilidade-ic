


library(shiny)
library(ggplot2)
library(DT)
library(bslib)
library(paletteer)
library(shinyjs)

source("temas/temas_graficos.R")
source("app_distribuicoes/app_dist.R")
source("app_comparacoes/app_comp.R")
source("app_experimentos/app_exp.R")


ui_App <- function()
  
  page_navbar(
    
    
 
    theme = bs_theme(
      version = 5,
      bootswatch = "sandstone",
      
      primary = "#4B5F80",     
      secondary = "#A4804C",   
      
      bg = "#F4F2E8",          
      fg = "#2B2B2B",
      
      "body-bg" = "#F4F2E8",
      "body-color" = "#2B2B2B",
      "navbar-bg" = "#3F6148",
      "navbar-light-color" = "#FFFFFF",
      "navbar-light-hover-color" = "#DBD3A4",
      
      # cards suaves
      "card-bg" = "#FFFFFF",
      "card-border-color" = "#E2DDCF",
      
      # inputs discretos
      "input-bg" = "#FFFFFF",
      "input-border-color"
      
      = "#D8D2C4",
      
      # links
      "link-color" = "#4B5F80"
    ),
    
  nav_panel(
    title = "Início",
    
    h2("Meu Aplicativo de Probabilidade"),
    
    p(
      "Este aplicativo tem como objetivo funcionar como uma ferramenta didática
  interativa, auxiliando estudantes e profissionais no ensino e na
  aprendizagem de conceitos de Probabilidade."
    ),
    
    p(
      "Aqui, você encontrará uma aba destinada às ",
      strong("Distribuições de Probabilidade"),
      ", na qual será possível explorar diferentes distribuições discretas
  e contínuas, seus parâmetros, funções de probabilidade ou densidade
  e algumas de suas principais propriedades."
    ),
    
    p(
      "A aba ",
      strong("Comparações"),
      " é destinada à visualização e ao estudo das relações entre diferentes
  distribuições de probabilidade, permitindo observar casos particulares,
  aproximações e outras relações importantes."
    ),
    
    p(
      "A última aba, ",
      strong("Experimentos"),
      ", busca dar uma abordagem interativa a conceitos de Probabilidade
  por meio de simulações. Nela, são explorados o ",
      strong("método de Monte Carlo"),
      ", os ensaios aleatórios do",
      strong("lançamento de uma moeda"),
      "e o ",
      strong("problema de Monty Hall"),
      "."
    ),
    
    p(
      "Caso queira entrar em contato, envie um e-mail para: ",
      a(
        "l277185@dac.unicamp.com",
        href = "mailto:seuemail@email.com"
      )
    )
  ),
  
  nav_panel(
    title = "Distribuições de Probabilidade",
    distribuicoes_ui("distribuicoes")
  ),

  nav_panel(
    title = "Comparações",
    comparacoes_ui("comparacoes")
  ),

   nav_panel(
     title = "Experimentos",
     exp_ui("experimentos")
   )
)



server_App <- function(input, output, session) {
  
  distribuicoes_server("distribuicoes")
  comparacoes_server("comparacoes")
  exp_server("experimentos")
}

shinyApp(ui_App, server_App)