comp_qui_gama_exp <- function(input, output, session){
  
  output$texto_parametros <- renderText({

    k <- input$v_chi_2
    paste0( "α = ", k/2," | β = 1/2", " | λ = 1/2") })
  
  
  
  
  output$plot_qui_gama_exp <- renderPlot({

    k <- input$v_chi_2
    alpha <- k / 2
    beta  <- 1 / 2
    lambda <- 1 / 2
    
    n <- 5000
    x_qui_simula <- rchisq(n, df = k )
    
  
    x <- seq( 0,  max(x_qui_simula),  length.out = 1000 )
    dens_gama <- dgamma(x,shape = alpha,  rate = beta  )
    dens_exp <- dexp( x,  rate = lambda)
    
    
  
    ggplot() +
       
       #histograma  qui-quadrado
        geom_histogram(
                        data = data.frame(x_qui_simula),
                        aes( x = x_qui_simula,
                             y = after_stat(density),
                            fill = "Distribuição Qui-quadrado"),
                        bins = 50,
                        alpha = 0.8) +
      #curva gama
      geom_line(
        data = data.frame(x, dens_gama),
        aes(x = x,
            y = dens_gama,
            color = "Densidade Gama",
            linetype = "Densidade Gama"),
        linewidth = 1.2) +
    
      
      #curva exponencial
      
      geom_line(
        data = data.frame(x, dens_exp),
        aes(x = x,
            y = dens_exp,
            color = "Exponencial(λ = 1/2)",
            linetype = "Exponencial(λ = 1/2)"),
        linewidth = 1.2) +
      
      #alteração manual das cores e tipos linhas
      scale_color_manual(
        values = c("Densidade Gama" = "#67161C",
                   "Exponencial(λ = 1/2)" = "#7C99B8"),
        name = "Curvas") +
      
      scale_fill_manual(
        values = c("Distribuição Qui-quadrado" = "#DCE8DF"),
        name = "Histograma") +
      
      scale_linetype_manual(
        values = c("Densidade Gama" = "solid",
                    "Exponencial(λ = 1/2)" = "dashed"),
        name = "Curvas") +
      
      #limites de exiibição
      coord_cartesian(
                     xlim = c(0, max(10, k * 2))) +
      
      labs(
           title = "Qui-Quadrado, Gama e Exponencial",
           subtitle = paste0("χ²(", k, ")  ~  Gamma(",alpha, ", ",beta, ")"),
           x = "Valores",
           y = "Densidade",
           color = "Distribuição") +
      
      tema_base_graficos
    
  })
  
  
 
  tagList(
    
    h3("Comparação entre Qui-Quadrado, Gama e Exponencial"),
    
    plotOutput(
      session$ns("plot_qui_gama_exp")
    ),
    
    br(),
    
    withMathJax(),
    
    withMathJax(
      
      HTML("

<p>
Isso porque tanto a distribuição Exponencial como a distribuição Qui-Quadrado
são casos particulares da distribuição Gama.
</p>

<p>
A densidade da distribuição Gama é:
</p>

$$
f(x)=
\\frac{\\beta^\\alpha}
{\\Gamma(\\alpha)}
x^{\\alpha-1}e^{-\\beta x},
\\quad x \\geq 0
$$

<p>
Uma distribuição Qui-Quadrado com \\(k\\) graus de liberdade possui densidade:
</p>

$$
f(x)=
\\frac{1}
{2^{k/2}\\Gamma(k/2)}
x^{k/2-1}e^{-x/2},
\\quad x \\geq 0
$$

<p>
Comparando com a distribuição Gama, temos:
</p>

$$
\\alpha = \\frac{k}{2}
\\quad \\text{e} \\quad
\\beta = \\frac{1}{2}
$$

<p>
Logo:
</p>

$$
\\chi^2(k)=Gamma\\left(
\\frac{k}{2},
\\frac{1}{2}
\\right)
$$

<p>
Da mesma forma, a distribuição Exponencial com parâmetro
\\(\\lambda\\) possui densidade:
</p>

$$
f(x)=
\\lambda e^{-\\lambda x},
\\quad x \\geq 0
$$

<p>
Comparando com a distribuição Gama:
</p>

$$
\\alpha = 1
\\quad \\text{e} \\quad
\\beta = \\lambda
$$

<p>
Portanto:
</p>

$$
Exp(\\lambda)=Gamma(1,\\lambda)
$$

")
      
    )
  )
  
}