comp_binomial_poisson <- function(input, output, session){
  
  
  
  output$plot_binomial_poisson <- renderPlot({
    
    lambda <- input$lambda_pois
    
    n <- input$n_bin
    
    p <- lambda/n
    
    x_max <- qpois(0.999, lambda)
    
    x <- 0:x_max
    
    dados <-  data.frame(
      x = x,
      binomial = dbinom(
                        x,
                        size = n,
                        prob = p
    ),
    
      poisson = dpois(
                      x,
                      lambda = lambda
      )
    
    
  )
    
    
    
    
    ggplot(dados, aes(x, y)) +
      geom_col(aes(y = binomial,
                   fill = "Binomial",
                   color = cor_borda_hist),
               alpha = 0.5,
               width = 0.8) +
      
      geom_point(aes(y = poisson,
                     color = "Poisson"),
                 
                 size = 1.5) +

      scale_fill_manual(
                        values = c("Binomial" = cor_histograma_comp)) +
      
      scale_color_manual(
                        values = c("Poisson" = "#A63A50")) +
      
      labs( title = "Aproximação Binomial x Poisson",
            subtitle = paste0("n = ", n,", λ = ", round(lambda,2),", p = λ/n = ", round(p,4)),
            x = "Valores",
            y = "Probabilidade",
            fill = "Distribuição",
            color = "Distribuição") +
      
      tema_base_graficos
  })
  
 

  
  tagList(
    
    h3("Comparação entre Binomial x Poisson"),
    
    plotOutput(session$ns("plot_binomial_poisson")),
      
    
    br(),
    
    withMathJax(),
    
    HTML("
         
         
<p>
A distribuição de Poisson pode ser entendida como um caso limite da
distribuição Binomial.
</p>

<p>
A distribuição Binomial modela o número de sucessos em
\\(n\\) ensaios de Bernoulli independentes, cada um com probabilidade de sucesso \\(p\\).
</p>

$$
P(X=x)=
\\binom{n}{x}
p^x(1-p)^{n-x},
\\quad x=0,1,2,\\dots,n
$$

<p>
Já a distribuição de Poisson diz sobre o número de ocorrências de um
evento em um intervalo fixo, utilizando o parâmetro \\(\\lambda\\),
que representa a taxa média de ocorrências:
</p>

$$
P(X=x)=
\\frac{e^{-\\lambda}\\lambda^x}{x!},
\\quad x=0,1,2,\\dots
$$

<p>
A relação entre elas aparece quando:
</p>

<ul>
<li>o número de ensaios \\(n\\) é grande;</li>
<li>a probabilidade \\(p\\) é pequena;</li>
<li>o produto \\(np\\) permanece aproximadamente constante.</li>
</ul>

<p>
Nessas condições:
</p>

$$
\\lambda = np
$$

<p>
e a Binomial passa a ser bem aproximada pela Poisson:
</p>

$$
Binomial(n,p)
\\approx
Poisson(\\lambda=np)
$$

<p>
Isso ocorre porque, quando existem muitos ensaios e a probabilidade
de sucesso é muito pequena, os eventos tornam-se raros.
</p>

<p>
Observe nos gráficos que, para valores adequados de \\(n\\) e \\(p\\),
as funções de probabilidade da Binomial e da Poisson tornam-se muito semelhantes.
</p>

         ")
    
  )
  
}