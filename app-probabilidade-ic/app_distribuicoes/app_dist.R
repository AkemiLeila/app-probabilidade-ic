library(shiny)
library(ggplot2)
library(DT)


arquivos_dist <- list.files("app_distribuicoes/arquivos_auxiliares", full.names = TRUE)
sapply(arquivos_dist, source)


distribuicoes_ui <- function(id){
  
  tagList(
    fluidPage(
      
      withMathJax(),
      
      
      sidebarLayout(
        sidebarPanel(
          
          selectInput((NS(id,"tipo_dist")), "Tipo de distribuição:",
                      choices = c("", "Todas", "Discretas", "Contínuas")),
          selectInput((NS(id,"dist")), "Escolha a distribuição:",
                      choices = c("", "Beta", "Binomial", "Binomial Negativa",  "Cauchy", "Exponencial", "Gama", "Geométrica", 
                                  "Hipergeométrica","Normal", "Poisson", "Qui-quadrado", "T-Student", "Uniforme")), 
          entradas_distribuicoes(id), 
          
          uiOutput(NS(id,"botao_contador")), 
          
          conditionalPanel(
            condition = paste0("input['", (NS(id,"dist")), "'] != ''"),
            uiOutput(NS(id,"botao_download_ui"))
            
            
          )
        ),
        mainPanel(
          
          conditionalPanel(
            condition = paste0("input['", NS(id, "dist"), "'] == ''"),
            texto_intro
          ),
          
          conditionalPanel(
            condition = paste0("input['", NS(id,"dist"), "'] != ''"),
            info_abas(id) 
          )
        )
        
        
      )
    )
  )
  
  
}


distribuicoes_server <-  function(id){
  
  moduleServer(id, 
               function(input, output, session) {
                 
                 
                 
                 ######### BOTÕES, ENTRADAS E ESCOLHAS LATERAIS
                 
                 
                 
                 
                 # contador
                 contador <- reactiveVal(0)
                 
                 observeEvent(input$nova_amostra, {
                   contador(contador() + 1)
                 })
                 
                 output$botao_contador <- renderUI({
                   
                   # impede aparecer sem distribuição
                   req(input$dist)
                   if (input$dist == "") return(NULL)
                   
                   actionButton(
                     inputId = session$ns("nova_amostra"),
                     label = HTML(paste0(
                       "Gerar nova amostra ",
                       "<span style='margin-left:10px; color:blue; font-weight:bold;'>",
                       contador(),
                       "</span>"
                     ))
                   )
                 })
                 
                 #escolha das distribuições no painel lateral
                 observeEvent(input$tipo_dist, {
                   if (input$tipo_dist == "Discretas") {
                     updateSelectInput(session, 
                                       session$ns("dist"), 
                                       choices = c("", "Binomial", "Binomial Negativa", "Geométrica", "Hipergeométrica", "Poisson"))
                   } else if (input$tipo_dist == "Contínuas"){ 
                     updateSelectInput(session, 
                                       session$ns("dist"), 
                                       choices = c("", "Beta", "Cauchy", "Exponencial", "Gama", "Normal", "Qui-quadrado", "T-Student", 
                                                   "Uniforme"))
                   } else {
                     updateSelectInput(session, 
                                       session$ns("dist"), 
                                       choices = c("", "Beta", "Binomial", "Binomial Negativa", "Cauchy",  "Exponencial", "Gama", 
                                                   "Geométrica", "Hipergeométrica", "Normal", "Poisson", "Qui-quadrado", "T-Student", 
                                                   "Uniforme"))
                   }
                 })
                 
                 
                 #entrada dos parametros
                 observeEvent(
                   list(
                     input$dist, 
                     input$alpha_beta, input$beta_beta, input$n_beta, input$S_beta,
                     input$n_bin, input$p_bin, input$n_bsam, input$S_bin,
                     input$r_nbin, input$p_nbin, input$n_nbin, input$S_nbin,
                     input$theta_cauchy,input$lambda_cauchy, input$n_cauchy, input$S_cauchy, 
                     input$lambda_exp, input$n_exp, input$S_exp,
                     input$alpha_gama, input$beta_gama, input$n_gama, input$S_gama,
                     input$p_geo, input$n_geo, input$S_geo,
                     input$K_hip, input$N_hip, input$n_hip, input$S_hip,
                     input$lambda_pois, input$n_pois, input$S_pois,
                     input$n_norm,input$mi_norm, input$sigma2_norm, 
                     input$v_chi, input$n_chi, input$S_chi,
                     input$v_t, input$n_t, input$S_t,
                     input$min_unif, input$max_unif, input$n_unif, input$S_unif),
                   {contador(0)}
                 )
                 
                 
                 
                 #função para botão identificar qual input n para botao de dowload
                 get_n_atual <- function(input) {
                   dist <- input$dist
                   
                   if (dist == "Beta")                return(input$n_beta)
                   if (dist == "Binomial")            return(input$n_bsam)
                   if (dist == "Binomial Negativa")   return(input$n_nbin)
                   if (dist == "Cauchy")              return(input$n_cauchy)
                   if (dist == "Exponencial")         return(input$n_exp)
                   if (dist == "Gama")                return(input$n_gama)
                   if (dist == "Geométrica")          return(input$n_geo)
                   if (dist == "Hipergeométrica")     return(input$n_hip)
                   if (dist == "Normal")              return(input$n_norm)
                   if (dist == "Poisson")             return(input$n_pois)
                   if (dist == "Qui-quadrado")        return(input$n_chi)
                   if (dist == "T-Student")           return(input$n_t)
                   if (dist == "Uniforme")            return(input$n_unif)
                   
                   return(NULL)
                 }
                 
                 
                 #botão para atualizar o n= ___ da amostra no botao de gerar download
                 output$botao_download_ui <- renderUI({
                   req(input$dist)
                   n_atual <- get_n_atual(input)
                   downloadButton(
                     session$ns("download_amostras"),
                     label = paste0("Baixar amostra n = ", n_atual %||% "")
                   )
                 })
                 
                 
                 
                 #gera amostra original a partir dos inputs (arquivo auxiliar)
                 amostra_base <- reactive({
                   req(input$dist)
                   gera_amostra_original(input$dist, input)
                 })
                 
                 
                 
                 
                 ########## BLOCO DO BOTÃO DE DOWNLOAD DA AMOSTRA OBSERVADA 
                 
                 #armazena uma amostra específica
                 amostra_original <- reactiveVal(NULL)
                 
                 #gera primeira amostra qdo parametros mudam
                 observeEvent(amostra_base(), {
                   amostra_original(amostra_base())
                 }, ignoreNULL = TRUE)
                 
                 #ao clicar em "nova amostra" dispara esse observe, ele gera uma nova amostra e guarda em amostra_original
                 observeEvent(input$nova_amostra, {
                   amostra_original( gera_amostra_original(input$dist, input) )
                 })
                 
                 #cria data frame com amostra_original - atualizado sempre que se cria uma nova amostra
                 dados_para_download <- reactive({
                   req(amostra_original())
                   data.frame(amostra = amostra_original())
                 })
                 
                 
                 # cria a saida para o download
                 output$download_amostras <- downloadHandler(
                   filename = function() {
                     dist <- input$dist
                     if (is.null(dist) || dist == "") dist <- "dados"
                     paste0("amostras_", dist, ".csv")
                   },
                   content = function(file) {
                     write.csv(dados_para_download(), file, row.names = FALSE)
                   }
                 )
                 
                 
                 
                 ############  PRIMEIRA ABA -  TEXTOS/RESUMOS DAS DISTRIBUIÇÕES   
                 
                 output$texto_teorico <- renderUI({
                   textos_distribuicoes(input$dist) 
                 })
                 
                 
                 
                 ############ SEGUNDA ABA  - HISTOGRAMA DE UM AMOSTRA E ESTATISTICA DESCRITIVA (AMOSTRAL E TEORICA) 
                 
                 #saída do histograma, os inputs são validados antes - chama funções auxiliares
                 output$histograma_original <- renderPlot({
                   req(input$dist)
                   validar_parametros_distribuicao(input)
                   histograma_amostra_original(input, amostra_original())
                 })
                 
                 
                 #estatisticas teoricas que estao no arquivo auxiliar
                 output$resumo_teorico <- renderUI({
                   req(input$dist)
                   validar_parametros_distribuicao(input)
                   estatisticas_teoricas(input$dist, input)
                 })
                 
                 
                 #estatisticas amostrais 
                 output$resumo_amostral <- renderUI({
                   req(amostra_original())
                   x <- amostra_original()
                   media <- mean(x)
                   mediana <- median(x)
                   variancia <- var(x)
                   tab <- table(x)
                   moda_empirica <- as.numeric(names(tab)[which.max(tab)])
                   
                   # cria tabela em HTML manualmente
                   HTML(paste0(
                     "<table class='table table-bordered'>",
                     "<tr><th>Estatística</th><th>Valor</th></tr>",
                     
                     "<tr><td><span title='", paste("Valor esperado da distribuição ", input$dist), 
                     "' style='color:#2E4F73;'> Média</span></td><td>", round(media,4), "</td></tr>",
                     
                     "<tr><td><span title='", paste("Variância teórica da distribuição ", input$dist), 
                     "' style='color:#6c757d;'> Variância</span></td><td>", round(variancia,4), "</td></tr>",
                     
                     "<tr><td><span title='Existe apenas se α > 1 e β > 1.' style='color:#2F5D50;'>Moda</span></td><td>", round(moda_empirica,4), "</td></tr>",
                     
                     "<tr><td><span title='Divide a distribuição em duas partes iguais.' style='color:#67161C;'> Mediana</span></td><td>", round(mediana,4), "</td></tr>",
                     
                     "</table>"
                   ))
                 })
                 
                 
                 
                 ########### QUARTA ABA - CAUCHY E A TCL ###
                 
                 
                 #Muda título da aba caso seja a Cauchy
                 titulo_tcl <- function(tipo) {
                   if (tipo == "Cauchy") {
                     list(  hist = "Distribuição das médias amostrais - Cauchy",
                            tabela = "Médias amostrais geradas")
                   } else {
                     list( hist = "Histograma das Médias Padronizadas (Z)",
                           tabela = "Valores Z das amostras")
                   }
                 }
                 
                 #reativo para alteracao do titulo de acordo com a distribuicao escolhida
                 output$titulo_histograma <- renderUI({
                   req(input$dist)
                   h3(titulo_tcl(input$dist)$hist)})
                 
                 output$titulo_tabela <- renderUI({
                   req(input$dist)
                   h3(titulo_tcl(input$dist)$tabela)})
                 
                 
                 # z-score  nas medias amostrais (arquivo auxiliar)
                 amostra_tcl <- reactive({
                   req(input$dist, amostra_original())
                   calculo_z_score(input, amostra_original())  
                 })
                 
                 
                 # histograma exceção exibição diferenciada da cauchy  e TCL das demais distribuições
                 output$histograma <- renderPlot({
                   req(input$dist)
                   validar_parametros_distribuicao(input)
                   
                   #se a entrada for Cauchy
                   if (input$dist == "Cauchy") {
                     req(input$n_cauchy, input$S_cauchy, input$theta_cauchy, input$lambda_cauchy)
                     n_val <- input$n_cauchy
                     S_val <- input$S_cauchy
                     theta <- input$theta_cauchy
                     lambda <- input$lambda_cauchy
                     medias <- replicate(S_val, mean(rcauchy(n_val, location = theta, scale = lambda)))
                     
                     ggplot(data.frame(Media = medias), aes(x = 1, y = Media)) +
                       geom_jitter(width = 0.2, color = cor_alerta , alpha = 0.5, size = 1.5) +
                       geom_boxplot(width = 0.1, outlier.shape = NA, color = "black", alpha = 0.2) +
                       coord_flip() +
                       labs(title = "A TCL não se aplica à Cauchy,pois ela não tem média nem variância definidas.\nOs outliers mostram que a média é instável e não converge quando n grande.",
                            x = "", y = "Média amostral") +
                       tema_base_graficos
                     
                     
                   } else {    #para quaisquer outras entradas
                     req(amostra_tcl())
                     z_scores <- amostra_tcl()$z
                     hist_tcl <- ggplot(data.frame(z = z_scores), aes(x = z)) +
                       geom_histogram(aes(y = after_stat(density)), bins = 30, fill = cor_histograma_tcl, alpha = 0.4, color = "black") +
                       geom_density(color = cor_densidade_teorica_tcl, size = 0.7) +
                       labs(title = paste("TCL - Distribuição", amostra_tcl()$dist),
                            subtitle = "Curva (linha): densidade empírica das médias padronizadas | Curva Pontilhada: Distribuição teórica da Normal Padrão ~N(0,1)",
                            x = "Z- Score", y = "Densidade") +
                       tema_base_graficos
                     
                     #se a caixa selecionavel estiver clicada ele mostra a linha densidade normal padrao
                     if (isTRUE(input$mostrar_normal)) {
                       hist_tcl <- hist_tcl + stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                                                            color = cor_alerta, size = 1, linetype = "dotted")}
                     
                     hist_tcl #imprime histograma
                     
                   }
                 }) 
                 
                 # observe({
                 #   cat("\n--- INPUTS EXISTENTES ---\n")
                 #   print(names(input))
                 # })
                 
                 ###############  TERCEIRA ABA - SUPORTE DIDÁTICO
                 
                 
                 
                 #cards
                 output$conteudo_suporte_didatico <- renderUI({
                   req(input$dist)
                   cards_suporte_didatico(input$dist, session$ns)
                 })
                 
                 #cenarios
                 cenarios_suporte_didatico_server(input,session)
                 
                 
                 
               }
  )
}
