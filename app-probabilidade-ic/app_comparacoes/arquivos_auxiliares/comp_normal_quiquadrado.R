comp_normal_quiquadrado <- function(input, output, session){
  
  
  
  n <- 10000
  media <- 0
  variancia <- 1
  desvio <- sqrt(variancia)
  v <- input$v_chi
  
  
  z <- rnorm( n,mean = media, sd = desvio)  # SIMULAÇÃO NORMAL
  
  
  z2 <- z^2   # A mesma simulação ao quadrado
  
  #Curva normal
  x_normal <- seq(-4,4,length.out = 500)
  densidade_normal <- dnorm(x_normal, mean = 0, sd = 1)
  
  
  
  # CURVA QUI-QUADRADO
  
  x_chi <- seq( 0,  max(z2), length.out = 500 )
  
  densidade_chi <- dchisq( x_chi, df = v )
  
  
  
  
  # GRÁFICO 1 - normal com curva de densidade normal
  
  output$plot_normal_chi <- renderPlot({
    
    ggplot(data.frame(z), aes(z)) +
      
      geom_histogram( 
                     aes(y = after_stat(density)),
                     bins = 40,
                     fill = "#DCE8DF",
                     alpha = 0.8) +
      
      geom_line(  
                 data = data.frame(x = x_normal, y = densidade_normal),
                 aes(x = x, y = y,
                 color = "Densidade Normal"),
                 linewidth = 1.2) +
      
      scale_color_manual(
        values = c("Densidade Normal" = "#67161C"),
        name = "Legenda") +
      
      coord_cartesian( 
                      xlim = c(-4, 4),
                      ylim = c(0, 0.5)) +
      
      labs(title = "Z ~ Normal(0,1)",
           x = "Valores",
           y = "Densidade") +
      
      tema_base_graficos
    
  })
  
  
  
  # GRÁFICO 2
  
  output$plot_normal2_chi <- renderPlot({
    
    ggplot(data.frame(z2), aes(z2)) +
      
      geom_histogram(
                     aes(y = after_stat(density)),
                     bins = 40,
                     fill = "#DCE8DF",
                     alpha = 0.8) +
      
      geom_line(  
                 data = data.frame(x = x_chi, y = densidade_chi),
                 aes(x = x, y = y,
                 color = "Densidade Qui-Quadrado"),
                 linewidth = 1.2) +
      
      scale_color_manual(
                          values = c("Densidade Qui-Quadrado" = "#2E4F73"),
                          name = "Legenda") +
      
      coord_cartesian(
                       xlim = c(0, 10),
                       ylim = c(0, 0.6)) +
      
      labs(
           title = expression(Z^2 ~"~ Qui-Quadrado com 1 grau de liberdade"),
           x = "Valores",
           y = "Densidade") +
      
      tema_base_graficos
    
  })
  
  
  # UI
  
  tagList(
    
    h3("Comparação entre Normal e Qui-Quadrado"),
    
    fluidRow(
      
      column( 6, plotOutput(session$ns("plot_normal_chi"))),
      column( 6,plotOutput(session$ns("plot_normal2_chi")))
      
    ),
    
    
    br(),
    
    withMathJax(
      HTML("


<p>
Quando uma variável aleatória possui distribuição Normal padrão,
o quadrado dessa variável segue uma distribuição Qui-Quadrado
com 1 grau de liberdade:
</p>

$$
X^2 \\sim \\chi^2(1)
$$

<p>
Isso acontece porque a distribuição Qui-Quadrado é definida como
a soma dos quadrados de variáveis Normais padrão independentes.
</p>

<p>
De forma geral:
</p>

$$
\\chi^2(k)=
Z_1^2 + Z_2^2 + \\cdots + Z_k^2
$$

<p>
onde:
</p>

$$
Z_1, Z_2, \\ldots, Z_k \\sim N(0,1)
$$

<p>
e todas as variáveis são independentes.
</p>


<p>
Observe que a distribuição Normal possui valores positivos e negativos,
sendo simétrica em torno de zero.
</p>

<p>
Ao elevar os valores ao quadrado:
</p>

<ul>
<li>todos os valores tornam-se não negativos;</li>
<li>a distribuição perde sua simetria.</li>
</ul>


")
    
  )
)
  
}