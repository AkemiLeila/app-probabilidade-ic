comp_normal_cauchy_tstudent <- function(input, output, session){
  
  
  
  output$plot_normal_cauchy_tstudent <- renderPlot({
    
    x <- seq(-10, 10, length.out = 1000)
    
    y_normal <- dnorm( x, mean = input$mi_norm, sd = sqrt(input$sigma2_norm))
    
    y_cauchy <- dcauchy(x, location = input$theta_cauchy,  scale = input$lambda_cauchy)
    
    y_t <- dt(x, df = input$v_t)
    
    dados <- data.frame(
      
      x = rep(x, 3),
      
      y = c(
        y_normal,
        y_t,
        y_cauchy
      ),
      
      distribuicao = factor(
        
        c(
          rep("Normal", length(x)),
          rep("T-Student", length(x)),
          rep("Cauchy", length(x))
        ),
        
        levels = c(
          "Normal",
          "T-Student",
          "Cauchy"
        )
      )
    )
      
    
    ggplot(
      dados,
      aes(
        x = x,
        y = y,
        color = distribuicao,
        linetype = distribuicao
      )
    ) +
      
      geom_line(
        linewidth = 1.5,
        alpha = 0.95
      ) +
      
      
      labs(
        title = "Comparação entre Normal, Cauchy e T-Student",
        x = "Valores",
        y = "Densidade",
        color = "Distribuição"
      ) +
      
      scale_color_manual(
        
        values = c(
          
          "Normal" = "#A12638",
          
          "T-Student" = "#D39B3D",
          
          "Cauchy" = "#4A6D8C"
          
        )
      ) +
      
      
      scale_linetype_manual(
        
        values = c(
          
          "Normal" = "solid",
          
          "T-Student" = "dotted",
          
          "Cauchy" = "dashed"
          
        )
      ) +
      
      tema_base_graficos
  })
  
  
  
  
  tagList(
    
    h3("Comparação entre Normal, Cauchy e T-Student"),
    
    plotOutput(session$ns("plot_normal_cauchy_tstudent")),
    
    
    br(),
    
    withMathJax(),
    
    HTML("
    Ao compararmos as distribuições Normal, T-Student e Cauchy, podemos observar algumas relações interessantes entre elas.
    
    
<p>
Sendo a densidade da distribuição Normal 
</p>
$$
f(x)=
\\frac{1}
{\\sqrt{2\\pi\\sigma^2}}
e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}},
\\quad x\\in\\mathbb{R},
\\quad \\sigma>0
$$

<p>
a densidade da distribuição T-Student com \\(\\nu\\) graus de liberdade 
</p>

$$
f(x)=
\\frac{
\\Gamma\\left(
\\frac{\\nu+1}{2}
\\right)
}
{
\\sqrt{\\nu\\pi}
\\Gamma\\left(
\\frac{\\nu}{2}
\\right)
}
\\left(
1+
\\frac{x^2}{\\nu}
\\right)^{
-\\frac{\\nu+1}{2}
},
\\quad x\\in\\mathbb{R},
\\quad \\nu>0
$$

<p>
e a densidade da distribuição Cauchy, com parâmetro de localização \\(\\theta\\) e
parâmetro de escala \\(\\lambda>0\\)
</p>

$$
f(x)=
\\frac{1}
{
\\pi\\lambda
\\left[
1+
\\left(
\\frac{x-\\theta}{\\lambda}
\\right)^2
\\right]
},
\\quad x\\in\\mathbb{R},
\\quad \\lambda>0
$$

<p>
pode-se observar que, conforme os graus de liberdade aumentam
</p>

$$
\\nu \\to \\infty
$$

<p>
a distribuição T-Student converge para a distribuição Normal padrão:
</p>

$$
T_\\nu \\xrightarrow{d} N(0,1).
$$


<p>
Enquanto isso, distribuição Cauchy padrão é um caso particular da distribuição T-Student quando
</p>

$$
\\nu = 1
$$

<p>
Nesse caso:
</p>

$$
\\boxed{T_1=Cauchy(0,1)}
$$

<p>
Ou seja, a Cauchy padrão é equivalente à distribuição T-Student com
1 grau de liberdade.
</p>

      ")
    
  )
  
}