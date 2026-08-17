comp_beta_uniforme <- function(input, output, session){
  
  #---------------- GRÁFICO BETA ----------------#
  
  output$plot_beta <- renderPlot({
    
    x <- seq(0, 1, length.out = 200)
    
    y <- dbeta(
      x,
      input$alpha_beta,
      input$beta_beta
    )
    
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color = pal_edward_hopper["dark_red"], linewidth = 1.2) +
      labs(
        title = paste(
          "Beta(",
          input$alpha_beta, ",",
          input$beta_beta, ")"
        ),
        x = "Valores",
        y = "Densidade"
      ) +
      tema_base_graficos
  })
  
  #---------------- GRÁFICO UNIFORME ----------------#
  
  output$plot_uniforme <- renderPlot({
    
    x <- seq(
      input$min_unif,
      input$max_unif,
      length.out = 200
    )
    
    y <- dunif(
      x,
      input$min_unif,
      input$max_unif
    )
    
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color = pal_edward_hopper["green"], linewidth = 1.2) +
      labs(
        title = paste(
          "Uniforme(",
          input$min_unif, ",",
          input$max_unif, ")"
        ),
        x = "Valores",
        y = "Densidade"
      ) +
      tema_base_graficos
  })
  
  #---------------- UI ----------------#
  
  tagList(
    
    h3("Comparação entre Beta e Uniforme"),
    
    fluidRow(
      column(6, plotOutput(session$ns("plot_beta"))),
      column(6, plotOutput(session$ns("plot_uniforme")))
    ),
    
    br(),
    
    withMathJax(),
    
    HTML("
<p>Isso acontece porque a densidade da distribuição Beta é:</p>

$$
f(x) =
\\frac{x^{\\alpha - 1}(1 - x)^{\\beta - 1}}
{B(\\alpha, \\beta)},
\\quad 0 \\le x \\le 1
$$

<p>Enquanto que a densidade da distribuição Uniforme é:</p>

$$
f(x) =
\\frac{1}{b-a},
\\quad a \\le x \\le b
$$

<p>
Se \\(\\alpha = 1\\) e \\(\\beta = 1\\), então:
</p>

$$
f(x) = 1
$$

<p>
O mesmo acontece na Uniforme se \\(a = 0\\) e \\(b = 1\\):
</p>

$$
f(x) = \\frac{1}{1-0} = 1
$$

<p>
Sendo assim, ambas possuem densidade constante no intervalo [0,1],
o que mostra que:
</p>

$$
Beta(1,1) = Uniforme(0,1)
$$
")
  )
  
}