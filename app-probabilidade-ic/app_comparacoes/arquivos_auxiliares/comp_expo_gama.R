comp_expo_gama <- function(input, output, session){
  
  observeEvent(input$lambda_exp, {
    
    updateNumericInput( session,  "beta_gama",  value = input$lambda_exp)
    
  })
  
  
  observeEvent(input$beta_gama, {
    
    updateNumericInput(session, "lambda_exp", value = input$beta_gama
    )
    
  })
  
  
  #EXPONENCIAL
  
  output$plot_exponencial <- renderPlot({
    
    lambda <- input$lambda_exp
    
    x <- seq(0, 10, length.out = 500)
    
    y <- dexp(
              x,
              rate = lambda
    )
    
    ggplot(data.frame(x, y), aes(x, y)) +
      
      geom_line(color = pal_edward_hopper["green"], linewidth = 1.2) +
      
      labs(title = paste0("Exponencial(",lambda, ")"),
          x = "Valores",
          y = "Densidade") +
      
      tema_base_graficos
  })
  
  #GAMA 
  
  output$plot_gama <- renderPlot({
    
    alpha <- input$alpha_gama
    beta  <- input$beta_gama
    
    x <- seq(0, 10, length.out = 500)
    
    y <- dgamma(
      x,
      shape = alpha,
      rate = beta
    )
    
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color = pal_edward_hopper["dark_red"], linewidth = 1.2) +
      labs(
        title = paste0(
          "Gamma(",
          alpha, ", ",
          beta, ")"
        ),
        x = "Valores",
        y = "Densidade"
      ) +
      tema_base_graficos
  })
  
  
  
  tagList(
    
    h3("Comparação entre Exponencial x Gama"),
    
    fluidRow(
      column(6, plotOutput(session$ns("plot_exponencial"))),
      column(6, plotOutput(session$ns("plot_gama")))
    ),
    
    br(),
    
    withMathJax(),
    
    HTML("
         <p>
           Acima é possível visualizar a comparação entre as distribuições
         Exponencial e Gama.
         </p>
           
           <p>
           A densidade da distribuição Exponencial com parâmetro
         \\(\\lambda\\) é dada por:
           </p>
           
           $$
           f(x)=
           \\lambda e^{-\\lambda x},
         \\quad x \\geq 0
         $$
           
           <p>
           E a densidade da distribuição Gama é dada por:
           </p>
           
           $$
           f(x)=
           \\frac{\\beta^\\alpha}
         {\\Gamma(\\alpha)}
         x^{\\alpha-1}e^{-\\beta x},
         \\quad x \\geq 0
         $$
           
           
           <p>
           Se:
           </p>
           
           $$
           \\alpha = 1
         \\quad \\text{e} \\quad
         \\beta = \\lambda
         $$
           
           <p>
           então a densidade da Gama torna-se:
           </p>
           
           $$
           f(x)=
           \\frac{\\lambda^1}
         {\\Gamma(1)}
         x^{1-1}e^{-\\lambda x}
         $$
           
           <p>
           Como:
           </p>
           
           $$
           \\Gamma(1)=1
         \\quad \\text{e} \\quad
         x^0=1
         $$
           
           <p>
           obtemos:
           </p>
           
           $$
           f(x)=
           \\lambda e^{-\\lambda x}
         $$
           
           <p>
           que é exatamente a densidade da distribuição Exponencial.
         </p>
           
           <p>
           Portanto:
           </p>
           
           $$
           Exp(\\lambda)=Gamma(1,\\lambda)
         $$
           
           <p>
           Isso significa que toda distribuição Exponencial é uma
         distribuição Gama com parâmetro de forma igual a 1.
         </p>
     
    ")
    
  )
  
}