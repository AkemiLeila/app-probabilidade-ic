info_abas <-function(id) {
  
  
  tabsetPanel(
  id = NS(id,"abas_principais"),
  
  tabPanel("Teoria",
           uiOutput(NS(id,"texto_teorico"))
  ),
  tabPanel("Distribuição Original",
         h3("Histograma da Distribuição Original"),
         plotOutput(NS(id,"histograma_original")),
         
         fluidRow(
           column(
             width = 6,
             div(
               class = "well",
               h4("Estatísticas Amostrais"),
               uiOutput(NS(id,"resumo_amostral")),
               tableOutput(NS(id,"resumo_amostral"))
             )
           ),
           column(
             width = 6,
             div(
               class = "well",
               h4("Estatísticas Teóricas"),
               tableOutput(NS(id,"resumo_teorico"))
             )
           )
         )
         
         
         
),

tabPanel("Suporte Didático",
         h3("Explorações e Cenários Didáticos"),
         uiOutput(NS(id,"conteudo_suporte_didatico"))
),

tabPanel("TCL",
                   uiOutput((NS(id,"titulo_histograma"))),
                   checkboxInput(NS(id, "mostrar_normal"),
                                 "Curva Normal Padrão",
                                 value = TRUE),
                   plotOutput((NS(id,"histograma"))
                     )
         )
)
}
