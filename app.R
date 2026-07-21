


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
    p("Selecione uma das abas acima para começar.")
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