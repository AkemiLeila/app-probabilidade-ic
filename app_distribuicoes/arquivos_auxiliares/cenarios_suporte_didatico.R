cenarios_suporte_didatico_server <-  function(input, session) {

   
    
######## BETA
#1.TCL
  observeEvent(input$exemplo_beta, {
    updateSelectInput(session, "dist", selected = "Beta")
     updateNumericInput( session,"alpha_beta", value = 2)
     updateNumericInput( session,"beta_beta", value = 5)
     updateNumericInput( session,"n_beta", value = 1000)
    
    updateTabsetPanel(session,"abas_principais", selected = "TCL")
  })
  # 2. Peso próximo de 1 (α > β)
  observeEvent(input$beta_peso_1, {
     updateNumericInput(session,"alpha_beta", value = 8)
     updateNumericInput(session,"beta_beta", value = 2)
     updateNumericInput(session,"n_beta", value = 1000)
    # Voltamos para a aba de densidade/visualização para ver a forma da curva
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })

  # 3. Peso próximo de 0 (β > α)
  observeEvent(input$beta_peso_0, {
     updateNumericInput(session,"alpha_beta", value = 2)
     updateNumericInput( session,"beta_beta", value = 8)
     updateNumericInput(session,"n_beta", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })

  # 4. Concentrada (α e β grandes)
  observeEvent(input$beta_concentrada, {
     updateNumericInput(session,"alpha_beta", value = 50)
     updateNumericInput( session,"beta_beta", value = 50)
     updateNumericInput( session,"n_beta", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })


####### BINOMIAL
#1.TCL
  observeEvent(input$exemplo_bin, {
    updateSelectInput(session,"dist", selected = "Binomial")
     updateNumericInput( session,"n_bin", value = 20)
     updateNumericInput( session,"p_bin", value = 0.3)
     updateNumericInput(session,"n_bsam", value = 1000)
     updateNumericInput( session,"S_bin", value = 5000)

    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  # 2. Binomial Simétrica
  observeEvent(input$bin_simetrica, {
     updateNumericInput( session,"n_bin", value = 20)
     updateNumericInput( session,"p_bin", value = 0.5)
     updateNumericInput( session,"n_bsam", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })

  # 3. Binomial Assimétrica (Sucessos Raros)
  observeEvent(input$bin_assimetrica_pos, {
     updateNumericInput( session,"n_bin", value = 50)
     updateNumericInput( session,"p_bin", value = 0.05)
     updateNumericInput( session,"n_bsam", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })

####### BINOMIAL NEGATIVA
  #1.TCL
  observeEvent(input$exemplo_nbin, {
    updateSelectInput( session,"dist", selected = "Binomial Negativa")
     updateNumericInput( session,"size_nbin", value = 10)
     updateNumericInput( session,"prob_nbin", value = 0.5)
     updateNumericInput( session,"n_nbin", value = 1000)
     updateNumericInput( session,"S_nbin", value = 5000)

    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  # 2. Aumento de r (Deslocamento)
  observeEvent(input$nbin_r_grande, {
     updateNumericInput( session,"r_nbin", value = 30)
     updateNumericInput( session,"p_nbin", value = 0.5)
     updateNumericInput( session,"n_nbin", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })


####### CAUCHY
  #1.TCL
  observeEvent(input$cauchy_media_inexistente, {
     updateNumericInput( session,"n_cauchy", value = 1000)
     updateNumericInput( session,"S_cauchy", value = 100)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  #.2 caudas pesadas
  observeEvent(input$cauchy_vs_normal, {
     updateNumericInput( session,"theta_cauchy", value = 0) # Posição
     updateNumericInput( session,"lambda_cauchy", value = 1) # Escala
    updateTabsetPanel( session,"abas_principais", selected = "Densidade")

    showModal(modalDialog(
      title = "Caudas Pesadas (Heavy Tails)",
      withMathJax(
        p("Embora o a curva da Cauchy pareça com a Normal, suas extremidades demoram muito mais para chegar ao zero."),
        p("Isso explica por que, em fenômenos que seguem a Cauchy, eventos catastróficos ou extremos acontecem com uma frequência surpreendente.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })


###### EXPONENCIAL
#1. TCL
  observeEvent(input$exemplo_exp, {
    updateSelectInput( session,"dist", selected = "Exponencial")
     updateNumericInput( session,"lambda_exp", value = 0.5)
     updateNumericInput( session,"n_exp", value = 1000)
     updateNumericInput( session,"S_exp", value = 5000)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

 #2. Lei dos grandes numeros
  observeEvent(input$exemplo1_exp, {
    updateSelectInput( session,"dist", selected = "Exponencial")
     updateNumericInput( session,"lambda_exp", value = 0.5)
     updateNumericInput( session,"n_exp", value = 100000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })


  #3.lambda garnde
  observeEvent(input$exp_impacto_lambda, {
     updateNumericInput( session,"lambda_exp", value = 8)
     updateNumericInput( session,"n_exp", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })

##### GAMA
  #1.TCL
  observeEvent(input$exemplo_gama, {
    updateSelectInput( session,"dist", selected = "Gama")
    updateNumericInput(session,"shape_gama", value = 2)
     updateNumericInput( session,"rate_gama", value = 1)
     updateNumericInput( session,"n_gama", value = 1000)
     updateNumericInput( session,"S_gama", value = 5000)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  # 2. Transição de Forma
  observeEvent(input$gama_formato_sino, {
     updateNumericInput( session,"alpha_gama", value = 5)
     updateNumericInput( session,"beta_gama", value = 1)
     updateNumericInput( session,"n_gama", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
  })

  #3.Soma de exponenciais
  observeEvent(input$gama_soma_exp, {
     updateNumericInput( session,"alpha_gama", value = 1)
     updateNumericInput( session,"beta_gama", value = 2)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "De Exponencial para Gama",
      withMathJax(
        p("Se você espera por apenas 1 evento $$\\alpha = 1$$ a Gama é idêntica à Exponencial."),
        p("Ao aumentar $$\\alpha$$ você está visualizando o tempo total para que vários eventos ocorram em sequência.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })

  #4. Relação com a qui-quadrado
  observeEvent(input$gama_qui_quadrado, {
     updateNumericInput( session,"alpha_gama", value = 2)
     updateNumericInput( session,"beta_gama", value = 0.5)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Conexão Qui-Quadrado",
      withMathJax(
        p("A Distribuição Qui-Quadrado com $$\\nu$$ graus de liberdade é um caso particular da Gama:"),
        p("$$\\alpha = \\nu/2 \\quad \\text{e} \\quad \\beta = 1/2$$"),
        p("Neste cenário, configuramos $$\\nu = 4$$.")
      ),
      easyClose = TRUE, footer = modalButton("Ok")
    ))
  })

###### GEOMÉTRICA
  #1.TCL
  observeEvent(input$exemplo_geo, {
    updateSelectInput( session,"dist", selected = "Geométrica")
     updateNumericInput( session,"p_geo", value = 0.3)
     updateNumericInput( session,"n_geo", value = 1000)
     updateNumericInput( session,"S_geo", value = 5000)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  # 2. Decaimento Geométrico
  observeEvent(input$geo_decaimento, {
     updateNumericInput( session,"p_geo", value = 0.5)
     updateNumericInput( session,"n_geo", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Por que o pico é no zero/um?",
      withMathJax(
        p("A Distribuição Geométrica modela o número de falhas antes do 1º sucesso."),
        p("O cenário mais provável é sempre o sucesso ocorrer o quanto antes. Por isso, a primeira barra é sempre a mais alta do gráfico.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })


  # 3. Sucessos Difíceis (p pequeno)
  observeEvent(input$geo_impacto_p, {
     updateNumericInput( session,"p_geo", value = 0.05)
     updateNumericInput( session,"n_geo", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Eventos Raros e Espera Longa",
      withMathJax(
        p("Com $$p = 0.05$$ a média de tentativas até o sucesso é $$1/p = 20$$."),
        p("Observe como o gráfico se comporta à direita. A cauda longa mostra que é muito comum precisar de mais tentativas quando o sucesso é difícil.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })


  ###### HIPERGEOMÉTRICA

  # 1. TCL: Aproximação pela Normal
  observeEvent(input$exemplo_hip, {
    updateSelectInput( session,"dist", selected = "Hipergeométrica")
     updateNumericInput( session,"N_hip", value = 100)
     updateNumericInput( session,"K_hip", value = 40)
     updateNumericInput( session,"nn_hip", value = 20)
     updateNumericInput( session,"n_hip", value = 100)
     updateNumericInput( session,"S_hip", value = 5000)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  # 2. Dependência (População Pequena)
  observeEvent(input$hiper_dependencia, {
     updateNumericInput( session,"N_hip", value = 20)
     updateNumericInput( session,"K_hip", value = 10)
     updateNumericInput( session,"nn_hip", value = 10)
     updateNumericInput( session,"n_hip", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Efeito da Retirada sem Reposição",
      withMathJax(
        p("Em uma população pequena ($$N=20$$), retirar um item altera drasticamente a proporção restante."),
        p("Ao sortear $$k=10$$ itens, a chance do próximo ser sucesso muda a cada retirada.
         Note como o histograma reflete essa dependência em comparação a uma Binomial."),
        p("Aqui, o tamanho da amostra para o gráfico é $$n_{hip}=2000$$ sorteios repetidos.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })

  # 3. Aproximação Binomial (N grande)
  observeEvent(input$hiper_approx_bin, {
     updateNumericInput( session,"N_hip", value = 1000)
     updateNumericInput( session,"K_hip", value = 200)
     updateNumericInput( session,"nn_hip", value = 50)
     updateNumericInput( session,"n_hip", value = 2000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Convergência para a Binomial",
      withMathJax(
        p("Com $$N=1000$$ e sorteando apenas $$k=50$$ itens, a retirada de uma bola quase não afeta a chance da próxima."),
        p("Neste cenário, a Hipergeométrica torna-se indistinguível de uma Binomial com $$n=50$$ e $$p = K/N = 0,2$$.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })


##### NORMAL
  #3. tcl
  observeEvent(input$exemplo_norm, {
    updateSelectInput( session,"dist", selected = "Normal")
     updateNumericInput( session,"mi_norm", value = 0)
     updateNumericInput( session,"sigma2_norm", value = 1)
     updateNumericInput( session,"n_norm", value = 1000)
     updateNumericInput( session,"S_norm", value = 5000)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
})

  # 2. Regra Empírica
  observeEvent(input$norm_regra_empirica, {
     updateNumericInput( session,"media_norm", value = 0)
     updateNumericInput( session,"sd_norm", value = 1)
     updateNumericInput( session,"n_norm", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "A Regra de Ouro da Normal",
      withMathJax(
        p("Em qualquer distribuição Normal:"),
        tags$ul(
          tags$li("~68% dos dados estão entre $$\\mu \\pm 1\\sigma$$"),
          tags$li("~95% dos dados estão entre $$\\mu \\pm 2\\sigma$$"),
          tags$li("~99.7% dos dados estão entre $$\\mu \\pm 3\\sigma$$")
        ),
        p("Isso torna a Normal extremamente previsível para controle de qualidade e riscos.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })

  # 2. Impacto do Sigma (Variabilidade)
  observeEvent(input$norm_impacto_sigma, {
     updateNumericInput( session,"media_norm", value = 0)
     updateNumericInput( session,"sigma2_norm", value = 9)
     updateNumericInput( session,"n_norm", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Dispersão e Densidade",
      p("Ao aumentar o desvio padrão, a curva fica mais larga e baixa. Isso acontece porque a área total deve continuar sendo igual a 1 (100%)."),
      p("Um desvio maior significa que os dados estão mais longe da média, o que aumenta a incerteza."),
      easyClose = TRUE, footer = modalButton("Ok")
    ))
  })


  ###### POISSON
  #1.TCL
  observeEvent(input$exemplo_pois, {
    updateSelectInput( session,"dist", selected = "Poisson")
     updateNumericInput( session,"lambda_pois", value = 5)
     updateNumericInput( session,"n_pois", value = 1000)
     updateNumericInput( session,"S_pois", value = 5000)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  # 2. Lambda Pequeno (Assimetria à direita)
  observeEvent(input$poisson_raro, {
     updateNumericInput( session,"lambda_pois", value = 0.5)
     updateNumericInput( session,"n_pois", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Ocorrências Raras",
      withMathJax(
        p("Com $$\\lambda = 0.5$$, esperamos apenas meio evento por intervalo."),
        p("O gráfico mostra que o resultado mais provável é 0 sucessos. A cauda se alonga para a direita, permitindo eventos raros (1, 2 ou 3).")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })

  # 3. Média = Variância (λ = 4)
  observeEvent(input$poisson_media_var, {
     updateNumericInput( session,"lambda_pois", value = 10)
     updateNumericInput( session,"n_pois", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Propriedade Fundamental",
      withMathJax(
        p("Na Poisson: $$E[X] = Var(X) = \\lambda$$."),
        p("Ao aumentar $$\\lambda$$ para 10, a média se desloca para a direita e o gráfico fica mais largo, pois a variância também aumentou para 4.")
      ),
      easyClose = TRUE, footer = modalButton("Ok")
    ))
  })


##### QUI-QUADRADO
 #1.TCL
  observeEvent(input$exemplo_chi, {
    updateSelectInput( session,"dist", selected = "Qui-quadrado")
     updateNumericInput( session,"df_chi", value = 5)
     updateNumericInput( session,"n_chi", value = 1000)
     updateNumericInput( session,"S_chi", value = 5000)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  # 2. A Normal ao Quadrado (gl = 1)
  observeEvent(input$q_quad_origem, {
     updateNumericInput( session,"v_chi", value = 1)
     updateNumericInput( session,"n_chi", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "De onde vem a Qui-Quadrado?",
      withMathJax(
        p("Se $$Z$$ segue uma distribuição $$N(0, 1)$$, então $$Z^2$$ segue uma $$\\chi^2$$ com 1 grau de liberdade."),
        p("Como qualquer número ao quadrado é positivo, a distribuição tem origem no zero e se estende ao infinito positivo."),
        p("Note como o gráfico é extremamente assimétrico com $$ν = 1$$.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })

  # 3. Soma de Normais (Aumento de gl)
  observeEvent(input$q_quad_soma, {
     updateNumericInput( session,"v_chi", value = 10)
     updateNumericInput( session,"n_chi", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Acumulando Variabilidade",
      withMathJax(
        p("Uma $$\\chi^2(ν)$$ é a soma de $$ν$$ variáveis Normais Padrão ao quadrado e independentes."),
        p("Por isso, quanto maior o $$ν$$ (graus de liberdade), mais a distribuição se desloca para a direita e começa a se parecer com uma Normal novamente (pelo TCL).")
      ),
      easyClose = TRUE, footer = modalButton("Ok")
    ))
  })

  # 4. Caso ν = 4 (Equivalente a Gama com alpha = 2 e beta = 0.5)
  observeEvent(input$q_quad_gama, {
     updateNumericInput( session,"v_chi", value = 4)
     updateNumericInput( session,"n_chi", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "A Qui-quadrado como uma Gama",
      withMathJax(
        p("Toda Qui-quadrado é uma Distribuição Gama ."),
        p("Se $$X \\sim \\chi^2(\\nu)$$, então ela é equivalente a uma:"),
        p("$$X \\sim \\text{Gama}\\left(\\alpha = \\frac{\\nu}{2}, \\beta = \\frac{1}{2}\\right)$$"),
        p("Neste exemplo com $$\\nu = 4$$, o gráfico é idêntico a uma Gama com $$\\alpha = 2$$ e $$\\beta = 0.5$$."),
        p("Isso explica por que, conforme $$\\nu$$ aumenta, a assimetria diminui — é o mesmo comportamento de acumular sucessos da Gama.")
      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })

  ####T-STUDENT
  #1. TCL
  observeEvent(input$exemplo_t, {
    updateSelectInput( session,"dist", selected = "T-Student")
     updateNumericInput( session,"df_t", value = 5)
     updateNumericInput( session,"n_t", value = 1000)
     updateNumericInput( session,"S_t", value = 5000)

    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

  # 2. T-Student com Caudas Pesadas (v = 2)
  observeEvent(input$t_caudas_pesadas, {
     updateNumericInput( session,"v_t", value = 2)
     updateNumericInput( session,"n_t", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Incerteza em Amostras Pequenas",
      withMathJax(
        p("Com apenas $$\\nu = 2$$, a distribuição é mais baixa no centro e mais alta nas pontas."),
        p("Isso reflete a maior incerteza: como a amostra é pequena, valores longe da média são mais prováveis do que na Normal.")

      ),
      easyClose = TRUE, footer = modalButton("Entendi")
    ))
  })
  #3. incerteza moderada
  observeEvent(input$t_v10, {
     updateNumericInput( session,"v_t", value = 10)
     updateNumericInput( session,"n_t", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Incerteza Moderada",
      withMathJax(
        p("Com $$\\nu = 10$$, a curva T já começa a se elevar no centro, mas as caudas ainda são mais altas que as de uma Normal."),
        p("Isso significa que, para o mesmo nível de confiança, os intervalos de confiança serão um pouco mais largos do que se conhecêssemos o desvio padrão populacional.")
      ),
      easyClose = TRUE,
      footer = modalButton("Entendi")
    ))
  })

  # 2. Caso v = 30 (Convergência)
  observeEvent(input$t_v30, {
     updateNumericInput( session,"v_t", value = 30)
     updateNumericInput( session,"n_t", value = 1000)
    updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

    showModal(modalDialog(
      title = "Convergência para a Normal",
      withMathJax(
        p("Na prática estatística, $$\\nu = 30$$ é frequentemente usado como o limite onde a distribuição T 'converge' para a Normal."),
        p("A diferença entre as áreas das caudas torna-se mínima. A partir daqui, usar a tabela Z (Normal) ou a tabela T gera resultados muito semelhantes.")
      ),
      easyClose = TRUE,
      footer = modalButton("Fascinante!")
    ))
  })

##### UNIFORME
  #1. TCL
  observeEvent(input$exemplo_unif, {
    updateSelectInput( session,"dist", selected = "Uniforme")
     updateNumericInput( session,"min_unif", value = 0)
     updateNumericInput( session,"max_unif", value = 1)
     updateNumericInput( session,"n_unif", value = 1000)
     updateNumericInput( session,"S_unif", value = 5000)
    updateTabsetPanel( session,"abas_principais", selected = "TCL")
  })

    # 2. Geometria (Intervalo Estreito vs Largo)
    observeEvent(input$uni_caixa, {
       updateNumericInput( session,"min_unif", value = 0)
       updateNumericInput( session,"max_unif", value = 2)
       updateNumericInput( session,"n_unif", value = 1000)
      updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")

      showModal(modalDialog(
        title = "Área e Altura",
        withMathJax(
          p("Se o intervalo [a, b] tem largura 2, a altura deve ser 1/2 = 0.5."),
          p("A área total do retângulo é sempre $$Base \\times Altura = (b-a) \\times \\frac{1}{b-a} = 1$$")

        ),
        easyClose = TRUE,
        footer = modalButton("Entendi")
      ))
    })

    # 3. Simetria e Deslocamento
    observeEvent(input$uni_simetria, {
       updateNumericInput( session,"min_unif", value = 10)
       updateNumericInput( session,"max_unif", value = 20)
       updateNumericInput( session,"n_unif", value = 1000)
      updateTabsetPanel( session,"abas_principais", selected = "Distribuição Original")
    })
}
