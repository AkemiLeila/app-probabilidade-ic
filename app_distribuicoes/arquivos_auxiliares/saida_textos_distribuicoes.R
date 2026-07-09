textos_distribuicoes <-function(dist) {
    
    if (dist == "Beta"){
      
      withMathJax(
        HTML('
    <b><span style="font-size:16px;">DISTRIBUIÇÃO BETA</span></b><br><br>
    
    <b>Tipo:</b> contínua<br><br>
    
    <b>Suporte:</b> 0 < x < 1<br><br>
    
    <b>Uso:</b> A distribuição Beta é usada para modelar proporções e probabilidades, ou seja, variáveis aleatórias que assumem valores entre 0 e 1. Ela é amplamente utilizada em inferência bayesiana, especialmente para modelar a incerteza sobre a probabilidade de sucesso em experimentos do tipo Bernoulli ou Binomial.<br><br>
    
    <b>Parâmetros:</b><br>
    A distribuição Beta possui dois parâmetros de forma:<br><br>
    α (alpha) > 0<br>
    β (beta) > 0<br><br>
    Esses parâmetros controlam:<br>
    <ul>
      <li>a posição da distribuição</li>
      <li>o grau de concentração (confiança)</li>
      </ul>
    <ul>
      
    
    <b>Propriedades:</b><br>
    $$ E[X] = \\frac{\\alpha}{\\alpha + \\beta} $$<br>
    $$ \\text{Var}[X] = \\frac{\\alpha \\beta}{(\\alpha + \\beta)^2 (\\alpha + \\beta + 1)} $$<br><br>
    
    <b>Função densidade de probabilidade:</b><br>
    $$ f(x) = \\frac{1}{B(\\alpha, \\beta)} x^{\\alpha-1}(1-x)^{\\beta-1}, \\quad 0 < x < 1 $$<br>
    onde B(α,β) é a função beta, responsável pela normalização da densidade.<br><br>

    <b>Função de distribuição acumulada:</b><br>
    $$ F(x) = I_x(\\alpha, \\beta) $$<br>
    onde I<sub>x</sub>(α,β) é a função beta incompleta regularizada.<br><br>
    
    <b>Observações:</b><br>
    A distribuição Beta possui uma relação direta e profunda com a distribuição Gama:<br><br>
    <ul>
    <li>
    <b>Relação com a distribuição Gama:</b><br>
    Se \\( X \\sim \\text{Gama}(\\alpha, 1) \\) e \\( Y \\sim \\text{Gama}(\\beta, 1) \\) são variáveis aleatórias independentes, então a variável
    $$ \\frac{X}{X + Y} $$
    segue uma distribuição Beta com parâmetros \\( \\alpha \\) e \\( \\beta \\).
    </li><br>
    <li>
    Essa construção mostra que a distribuição Beta pode ser interpretada como a proporção relativa entre duas quantidades positivas modeladas por distribuições Gama.
    </li><br>
    <li>
    Essa relação é fundamental em inferência bayesiana, pois explica por que a distribuição Beta é conjugada da Binomial e da Bernoulli.
    </li>
    </ul>
  ')
 )
      
    } else if (dist == "Binomial Negativa") {
      
      withMathJax(
        HTML('
    <b><span style="font-size:16px;">DISTRIBUIÇÃO BINOMIAL NEGATIVA</span></b><br><br>

    <b>Tipo:</b> discreta<br><br>

    <b>Suporte:</b> \\( k = 0, 1, 2, \\dots \\)<br><br>

    <b>Uso:</b> A distribuição Binomial Negativa é usada para modelar o número de falhas observadas até que um número fixo de sucessos ocorra em uma sequência de ensaios de Bernoulli independentes, todos com a mesma probabilidade de sucesso. Ela é muito utilizada em estudos de confiabilidade, controle de qualidade e processos estocásticos.<br><br>

    <b>Parâmetros:</b><br>
    A distribuição Binomial Negativa possui dois parâmetros:<br><br>
    \\( r \\in \\mathbb{N} \\) — número fixo de sucessos a serem alcançados<br>
    \\( 0 < p < 1 \\) — probabilidade de sucesso em cada ensaio<br><br>
    Esses parâmetros controlam:<br>
    <ul>
    <li>quantos sucessos são necessários para encerrar o experimento (\\(r\\))</li>
    <li>a frequência relativa de falhas antes desses sucessos (\\(p\\))</li>
    </ul>

    <b>Propriedades:</b><br>
    $$ E[X] = \\frac{r(1 - p)}{p} $$<br>
    $$ \\text{Var}[X] = \\frac{r(1 - p)}{p^2} $$<br><br>

    <b>Função de probabilidade:</b><br>
    $$ P(X = k) = \\binom{k + r - 1}{r - 1} p^r (1 - p)^k, \\quad k = 0, 1, 2, \\dots $$<br>
    onde o coeficiente binomial $$ \\binom{k + r - 1}{r - 1} $$ representa o número de sequências possíveis com \\(k\\) falhas e \\(r\\) sucessos, sendo o último resultado um sucesso.<br><br>

    <b>Função de distribuição acumulada:</b><br>
    $$ F(k) = P(X \\le k) = \\sum_{i=0}^{k} \\binom{i + r - 1}{r - 1} p^r (1 - p)^i $$<br><br>

    <b>Observações:</b><br>
    A distribuição Binomial Negativa está diretamente relacionada às distribuições Bernoulli e Binomial:<br><br>
    <ul>
    <li>
    <b>Relação com a Bernoulli:</b><br>
    A distribuição Binomial Negativa é construída a partir de uma sequência de ensaios de Bernoulli independentes. Cada ensaio possui apenas dois resultados possíveis (sucesso ou fracasso), com probabilidade constante de sucesso \\(p\\). A variável aleatória conta quantas falhas ocorrem antes que o \\(r\\)-ésimo sucesso seja observado.
    </li><br>

    <li>
    <b>Relação com a Binomial:</b><br>
    A diferença central entre a distribuição Binomial e a Binomial Negativa está no que é mantido fixo. Na Binomial, o número de ensaios é fixo e o número de sucessos é aleatório. Já na Binomial Negativa, o número de sucessos é fixo e o número de ensaios (ou falhas) até alcançá-los é aleatório. Ambas assumem ensaios de Bernoulli independentes com a mesma probabilidade de sucesso.
     </li>
      </ul>
  ')
      )
      
    } else if (dist == "Binomial") {
      
      withMathJax(
        HTML('
      <b><span style="font-size:16px;">DISTRIBUIÇÃO BINOMIAL</span></b><br><br>
        
      <b>Tipo:</b> discreta<br><br>
      
      <b>Suporte:</b>\\( k = 0, 1, 2, \\dots\\), n<br><br>

      <b>Uso:</b> A distribuição Binomial é usada para modelar o número de sucessos obtidos em um número fixo de ensaios de Bernoulli independentes, todos com a mesma probabilidade de sucesso. Exemplos típicos incluem o número de caras em lançamentos de moeda ou o número de itens defeituosos em um lote inspecionado.<br><br>

      <b>Parâmetros:</b><br>
      A distribuição Binomial possui dois parâmetros:<br><br
      \\( n \\in \\mathbb{N} \\) — número fixo de ensaios<br>
      \\( 0 < p < 1 \\) — probabilidade de sucesso em cada ensaio<br><br>
      Esses parâmetros controlam:<br>
      <ul>
      <li>o número máximo de sucessos possíveis (\\(n\\))</li>
      <li>a chance de sucesso em cada experimento (\\(p\\))</li>
      </ul>

      <b>Propriedades:</b><br>
      $$ E[X] = n \\cdot p $$<br>
      $$ \\text{Var}[X] = n \\cdot p \\cdot (1 - p) $$<br><br>

      <b>Função de probabilidade:</b><br>
      $$ P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}, \\quad k = 0, 1, 2, \\dots, n $$<br><br>

      <b>Função de distribuição acumulada:</b><br>
      $$ F(k) = P(X \\le k) = \\sum_{i=0}^{k} \\binom{n}{i} p^i (1-p)^{n-i}, \\quad k = 0, 1, 2, \\dots, n $$<br><br>

      <b>Observações:</b><br>
      A distribuição Binomial está diretamente relacionada às distribuições Bernoulli e Binomial Negativa:<br><br>
      <ul>
      <li>
      <b>Relação com a Bernoulli:</b><br>
      Um ensaio de Bernoulli é um experimento com apenas dois resultados possíveis (sucesso ou fracasso). A distribuição Binomial representa a soma de \\(n\\) variáveis aleatórias de Bernoulli independentes com a mesma probabilidade de sucesso. Em particular, quando \\(n = 1\\), a distribuição Binomial se reduz à distribuição Bernoulli.
      </li><br>

      <li>
      <b>Relação com a Binomial Negativa:</b><br>
      Enquanto a distribuição Binomial modela o número de sucessos em um número fixo de ensaios, a distribuição Binomial Negativa modela o número de ensaios necessários até que um número fixo de sucessos ocorra. Ambas assumem ensaios de Bernoulli independentes com probabilidade constante de sucesso, mas diferem na variável aleatória de interesse.
      </li>
      </ul>
    ')
      )
      
     } else if (dist == "Cauchy") {
       
       withMathJax(
         HTML('
          <b><span style="font-size:16px;">DISTRIBUIÇÃO CAUCHY</span></b><br><br>
          
          <b>Tipo:</b> contínua<br><br>
          
          <b>Suporte:</b>\\( -\\infty < x < \\infty\\)<br><br>
          
          <b>Uso:</b> A distribuição Cauchy é usada para modelar fenômenos com caudas pesadas, nos quais valores extremos ocorrem com frequência relativamente alta. Ela aparece, por exemplo, na modelagem de erros de medição extremos, ruídos em sinais físicos e em problemas de ressonância. Diferentemente da Normal, a Cauchy não possui média nem variância bem definidas.<br><br>
          
          <b>Parâmetros:</b><br> A distribuição Cauchy possui dois parâmetros:<br><br>

          \\( \\theta \\in \\mathbb{R} \\) — parâmetro de posição (centro da distribuição)<br>
          \\( \\lambda > 0 \\) — parâmetro de escala (largura do pico)<br><br>

          Esses parâmetros controlam:<br>
          <ul>
          <li>a localização do pico da densidade (\\(\\theta\\))</li>
          <li>a dispersão dos valores em torno do centro (\\(\\lambda\\))</li>
          </ul>
         
          <b>Propriedades:</b><br>
          A distribuição Cauchy não possui esperança matemática nem variância definidas:<br><br>
          $$ E[X] \\text{ não existe} $$  
          $$ \\text{Var}[X] \\text{ não existe} $$<br><br>
          
          <b>Função densidade de probabilidade:</b><br>
          $$ f(x) = \\frac{1}{\\pi \\lambda \\left[1 + \\left(\\frac{x - \\theta}{\\lambda}\\right)^2\\right]}, \\quad x \\in \\mathbb{R} $$<br><br>
          
          <b>Função de distribuição acumulada:</b><br>
          $$ F(x) = \\frac{1}{\\pi} \\arctan\\left(\\frac{x - \\theta}{\\lambda}\\right) + \\frac{1}{2} $$
          ')
       )
       
     } else if (dist == "Exponencial") {
       
       withMathJax(
         HTML('
          <b><span style="font-size:16px;">DISTRIBUIÇÃO EXPONENCIAL</span></b><br><br>
          
          <b>Tipo:</b> contínua<br><br>
          
          <b>Suporte:</b> \\(x \\ge 0\\)<br><br>

          <b>Uso:</b> A distribuição Exponencial é usada para modelar o tempo de espera até a ocorrência de um evento aleatório, assumindo que os eventos ocorrem de forma independente e a uma taxa constante. Ela é amplamente utilizada para modelar o tempo entre eventos sucessivos em um processo de Poisson, como o tempo até a próxima falha de um equipamento ou a chegada de um cliente.<br><br>

          <b>Parâmetros:</b><br>
          A distribuição Exponencial possui um único parâmetro:<br><br>
          \\( \\lambda > 0 \\) — parâmetro de taxa<br><br>
          Esse parâmetro controla:<br>
          <ul>
          <li>a frequência média de ocorrência dos eventos</li>
          <li>a rapidez com que a probabilidade decai ao longo do tempo</li>
          </ul>

          <b>Propriedades:</b><br>
          $$ E[X] = \\frac{1}{\\lambda} $$<br>
          $$ \\text{Var}[X] = \\frac{1}{\\lambda^2} $$<br><br>

          <b>Função densidade de probabilidade:</b><br>
          $$ f(x) = \\lambda e^{-\\lambda x}, \\quad x \\ge 0 $$<br><br>

          <b>Função de distribuição acumulada:</b><br>
          $$ F(x) = P(X \\le x) = 1 - e^{-\\lambda x}, \\quad x \\ge 0 $$<br><br>

          <b>Observações:</b><br>
          A distribuição Exponencial é um caso particular da distribuição Gama. Quando a distribuição Gama possui parâmetro de forma igual a 1, ela se reduz à distribuição Exponencial:<br><br>
          $$ X \\sim \\text{Gama}(\\alpha = 1, \\lambda) \\; \\Rightarrow \\; X \\sim \\text{Exponencial}(\\lambda) $$
')
       )
       
    } else if (dist == "Gama") {
      
      withMathJax(
        HTML('
      <b><span style="font-size:16px;">DISTRIBUIÇÃO GAMA</span></b><br><br>

      <b>Tipo:</b> contínua<br><br>
      
      <b>Suporte:</b> \\( x \\in (0, \\infty) \\)<br><br>

      <b>Uso:</b> A distribuição Gama é usada para modelar tempos de espera acumulados, durações e, em geral, variáveis contínuas positivas e assimétricas à direita. Ela descreve o tempo total até que um evento ocorra várias vezes, sendo amplamente utilizada em confiabilidade, teoria das filas, processos de Poisson e inferência bayesiana. A distribuição Gama generaliza a distribuição Exponencial, que surge como um caso particular.<br><br>

      <b>Parâmetros:</b><br>
      A distribuição Gama possui dois parâmetros:<br><br>
      \\( \\alpha > 0 \\) — parâmetro de forma (shape)<br>    
      \\( \\beta > 0 \\) — parâmetro de taxa (rate)<br><br>
      Esses parâmetros controlam:<br>
      <ul>
      <li>\\(\\alpha\\): a forma da distribuição e o grau de assimetria</li>
      <li>\\(\\beta\\): a taxa de ocorrência dos eventos (concentração da densidade)</li>
      </ul>

      Também é comum definir o parâmetro de escala:<br><br>
      \\( \\text{scale} = \\frac{1}{\\beta} \\)<br><br>

      <b>Interpretação:</b><br>
      <ul>
      <li><b>Taxa (rate):</b> descreve quão rapidamente os eventos acontecem, por exemplo: “eventos ocorrem a uma taxa média de 2 por unidade de tempo”.</li>
      <li><b>Escala (scale):</b> descreve o tempo médio ou duração típica, por exemplo: “cada etapa dura em média 0,5 unidades de tempo”.</li>
      </ul>

      Neste aplicativo, está sendo utilizada a parametrização em termos da taxa (\\(\\beta\\)).<br><br>

      <b>Propriedades:</b><br>
      $$ E[X] = \\frac{\\alpha}{\\beta} $$<br>
      $$ \\text{Var}[X] = \\frac{\\alpha}{\\beta^2} $$<br><br>


      <b>Função densidade de probabilidade:</b><br>
      $$ f(x) = \\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)} x^{\\alpha - 1} e^{-\\beta x}, \\quad x > 0 $$<br>
      onde \\( \\Gamma(\\alpha) \\) é a função Gama, responsável por normalizar a densidade (garantir que a área total seja 1).<br><br>

      <b>Função de distribuição acumulada:</b><br>
      $$ F(x) = \\frac{1}{\\Gamma(\\alpha)} \\, \\gamma(\\alpha, \\beta x) $$<br>
      onde \\( \\gamma(\\alpha, \\cdot) \\) é a função Gama incompleta inferior.<br><br>

      <b>Observações:</b><br>
      <ul>
      <li>Para \\( \\alpha \\) fixo, a variância cresce mais rapidamente que a média, 
       o que torna a distribuição Gama adequada para modelar <i>overdispersion</i> 
      em dados positivos.
      </li>
      <li>
      \\( \\alpha \\) inteiro positivo → Distribuição Erlang
      </li>

      <li>
      Soma de variáveis Exponenciais independentes → Distribuição Gama
      </li>

      <li>
     Tempo de espera acumulado em um processo de Poisson → Distribuição Gama
     </li>
     </ul>

      
      A distribuição Gama possui relações importantes com outras distribuições:<br><br>
      
      <ul>
      <li>
      <b>Relação com a Exponencial:</b><br>
      Quando \\( \\alpha = 1 \\), a distribuição Gama se reduz à distribuição Exponencial. Nesse caso, ela modela o tempo até a ocorrência de um único evento. De forma geral, a Gama pode ser interpretada como o tempo até que \\(\\alpha\\) eventos ocorram em um processo com tempos entre eventos exponenciais.
      </li><br>

      <li>
      <b>Relação com a Beta:</b><br>
      A distribuição Beta pode ser obtida a partir de variáveis Gama independentes. Se \\(X \\sim \\text{Gama}(\\alpha, 1)\\) e \\(Y \\sim \\text{Gama}(\\beta, 1)\\), então a variável \\( \\frac{X}{X + Y} \\) segue uma distribuição Beta com parâmetros \\(\\alpha\\) e \\(\\beta\\). Essa relação é fundamental em inferência bayesiana e explica a forte conexão entre as duas distribuições.
      </li><br>
      
      <li>
      <b>Relação com a Qui-quadrado:</b><br>
      A distribuição Qui-quadrado é um caso particular da distribuição Gama. 
      Especificamente, se \\( X \\sim \\chi^2(\\nu) \\), então 
      \\( X \\sim \\text{Gama}(\\alpha = \\nu/2, \\beta = 1/2) \\). 
      Essa relação explica a presença da função Gama na densidade da Qui-quadrado 
      e reforça sua interpretação como a soma de quantidades positivas acumuladas, 
      muito comum em problemas de inferência estatística.
      </li>
      </ul>

      '))
      
    } else if (dist == "Geométrica") {
      
      withMathJax(
        HTML('
      <b><span style="font-size:16px;">DISTRIBUIÇÃO GEOMÉTRICA</span></b><br><br>

      <b>Tipo:</b> discreta<br><br>
      
      <b>Suporte:</b> \\( k = 0, 1, 2, \\dots \\)<br><br>

      <b>Uso:</b> A distribuição Geométrica é usada para modelar o número de falhas observadas até a ocorrência do primeiro sucesso em uma sequência de ensaios de Bernoulli independentes, todos com a mesma probabilidade de sucesso. Nesta parametrização, a variável aleatória conta falhas. Caso o interesse fosse o número de tentativas até o primeiro sucesso, o suporte e a função de probabilidade seriam definidos de forma diferente.<br><br>

      <b>Parâmetros:</b><br>
      A distribuição Geométrica possui um único parâmetro:<br><br>
      \\( 0 < p < 1 \\) — probabilidade de sucesso em cada ensaio<br><br>
      Esse parâmetro controla:<br>
      <ul>
      <li>a chance de sucesso em cada tentativa</li>
      <li>a velocidade com que a probabilidade decai ao longo de \\(k\\)</li>
      </ul>

      <b>Propriedades:</b><br>
      $$ E[X] = \\frac{1 - p}{p} $$<br>
      $$ \\text{Var}[X] = \\frac{1 - p}{p^2} $$<br><br>

      <b>Função de probabilidade:</b><br>
      $$ P(X = k) = (1 - p)^k \\, p, \\quad k = 0, 1, 2, \\dots $$<br><br>

      <b>Função de distribuição acumulada:</b><br>
      $$ F(k) = P(X \\le k) = 1 - (1 - p)^{k + 1} $$<br><br>

      <b>Observações:</b><br>
      A distribuição Geométrica possui propriedades e interpretações importantes:<br><br>

      <ul>
    
      <li>
      <b>Interpretação do parâmetro \\(p\\):</b><br>
      À medida que \\(p\\) diminui, o valor esperado de \\(X\\) aumenta e a cauda da distribuição se torna mais longa.
      Em particular:
      <ul>
      <li>\\( p = 0{,}8 \\) → \\( E[X] = 0{,}25 \\): distribuição muito concentrada</li>
      <li>\\( p = 0{,}5 \\) → \\( E[X] = 1 \\): comportamento equilibrado</li>
      <li>\\( p = 0{,}2 \\) → \\( E[X] = 4 \\): cauda longa</li>
      <li>\\( p = 0{,}1 \\) → \\( E[X] = 9 \\): distribuição bem espalhada</li>
      </ul>
      Diminuir gradualmente \\(p\\) faz a cauda crescer — essa é a essência da distribuição Geométrica.
      </li>
      </ul>
    ')
      )
      
    } else if (dist == "Hipergeométrica") {
      
      withMathJax(
        HTML('
      <b><span style="font-size:16px;">DISTRIBUIÇÃO HIPERGEOMÉTRICA</span></b><br><br>

      <b>Tipo:</b> discreta<br><br>
      
      <b>Suporte:</b> \\( k = \\max(0, n - (N - K)), \\dots, \\min(n, K) \\)<br><br>

      <b>Uso:</b> A distribuição Hipergeométrica é usada para modelar o número de sucessos obtidos em uma amostra retirada <b>sem reposição</b> de uma população finita. Ela é apropriada quando cada elemento só pode ser selecionado uma vez, o que torna as observações dependentes entre si.<br><br>

      <b>Parâmetros:</b><br>
      A distribuição Hipergeométrica possui três parâmetros:<br><br>
      \\( N \\in \\mathbb{N} \\) — tamanho da população<br> 
      \\( K \\in \\mathbb{N} \\) — número de sucessos na população<br>
      \\( N - K \\in \\mathbb{N} \\) — tamanho da fracassos na população<br><br>
      \\( n \\in \\mathbb{N} \\) — tamanho da amostra<br><br>
      
      Esses parâmetros controlam:<br>
      <ul>
      <li>o total de elementos disponíveis (\\(N\\))</li>
      <li>a quantidade de elementos classificados como sucesso (\\(K\\))</li>
      <li>o número de retiradas sem reposição (\\(n\\))</li>
      </ul>

      <b>Propriedades:</b><br>
      $$ E[X] = n \\cdot \\frac{K}{N} $$<br>
      $$ \\text{Var}[X] = n \\cdot \\frac{K}{N} \\left(1 - \\frac{K}{N} \\right) \\frac{N - n}{N - 1} $$<br><br>

      <b>Função de probabilidade:</b><br>
      $$ P(X = k) = \\frac{\\binom{K}{k} \\binom{N - K}{n - k}}{\\binom{N}{n}} $$<br><br>

      <b>Função de distribuição acumulada:</b><br>
      $$ F(k) = P(X \\le k) = \\sum_{i=0}^{k} \\frac{\\binom{K}{i} \\binom{N - K}{n - i}}{\\binom{N}{n}} $$<br><br>

      <b>Observações:</b><br>
      A distribuição Hipergeométrica possui relações importantes com outras distribuições discretas:<br><br>
      <ul>
      <li>
      <b>Relação com a Binomial:</b><br>
      Quando o tamanho da população \\(N\\) é muito grande em comparação com o tamanho da amostra \\(n\\), a distribuição Hipergeométrica pode ser bem aproximada por uma distribuição Binomial com parâmetros \\(n\\) e \\(p = K/N\\). Essa aproximação é válida porque a dependência entre as retiradas se torna desprezível.
      </li><br>
      <li>
      <b>Dependência entre observações:</b><br>
      Diferentemente da Binomial, os ensaios na Hipergeométrica não são independentes, pois cada retirada altera a composição da população restante.
      </li>
      </ul>
    ')
      )
      
    } else if (dist == "Normal") {
      
      withMathJax(
        HTML('
      <b><span style="font-size:16px;">DISTRIBUIÇÃO NORMAL</span></b><br><br>

      <b>Tipo:</b> contínua<br><br>
      
      <b>Suporte:</b> \\( x \\in (-\\infty, \\infty) \\)<br><br>

      <b>Uso:</b> A distribuição Normal é usada para modelar fenômenos naturais e processos que tendem a se concentrar em torno de um valor médio, com dispersão simétrica. Ela é amplamente utilizada em inferência estatística, controle de qualidade, testes de hipóteses, construção de intervalos de confiança e modelagem de erros aleatórios.<br><br>

      <b>Parâmetros:</b><br>
      A distribuição Normal possui dois parâmetros:<br><br>
      \\( \\mu \\in \\mathbb{R} \\) — média (parâmetro de localização)<br>
      \\( \\sigma > 0 \\) — desvio padrão (parâmetro de escala)<br><br>
      Esses parâmetros controlam:<br>
      <ul>
      <li>a posição do centro da distribuição (\\(\\mu\\))</li>
      <li>a dispersão dos valores em torno da média (\\(\\sigma\\))</li>
      </ul>

      <b>Propriedades:</b><br>
      $$ E[X] = \\mu $$<br>
      $$ \\text{Var}[X] = \\sigma^2 $$<br><br>

      <b>Função densidade de probabilidade:</b><br>
      $$ f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} \\, e^{-\\frac{(x - \\mu)^2}{2 \\sigma^2}}, \\quad x \\in \\mathbb{R} $$<br><br>

      <b>Função de distribuição acumulada:</b><br>
      $$ F(x) = P(X \\le x) = \\int_{-\\infty}^{x} \\frac{1}{\\sigma \\sqrt{2\\pi}} \\, e^{-\\frac{(t - \\mu)^2}{2 \\sigma^2}} \\, dt $$<br><br>

      <b>Observações:</b><br>
      A distribuição Normal possui propriedades fundamentais que explicam sua ampla utilização:<br><br>

      <ul>
      <li>
      <b>Simetria:</b><br>
      A densidade é simétrica em torno da média \\(\\mu\\). Média, mediana e moda coincidem.
      </li><br>


      <li>
      <b>Normal padrão:</b><br>
      Qualquer variável Normal pode ser padronizada pela transformação
      $$ Z = \\frac{X - \\mu}{\\sigma}, $$
      resultando em uma variável com distribuição Normal padrão, de média 0 e variância 1.
      </li><br>

      <li>
      <b>Teorema Central do Limite:</b><br>
      A Normal surge como distribuição limite da soma (ou média) de um grande número de variáveis aleatórias independentes sob condições gerais, o que explica seu papel central na estatística.
      </li>
      </ul>
  ')
      )
      
    } else if (dist == "Poisson") {
      
      withMathJax(
        HTML('
      <b><span style="font-size:16px;">DISTRIBUIÇÃO POISSON</span></b><br><br>

      <b>Tipo:</b> discreta<br><br>
      
      <b>Suporte:</b> \\( k = 0, 1, 2, \\dots \\)<br><br>

      <b>Tipo:</b> discreta<br><br>

      <b>Uso:</b> A distribuição Poisson modela o número de eventos que ocorrem em um intervalo fixo de tempo, espaço ou área, assumindo que os eventos:
      <ul>
      <li>ocorrem de forma independente</li>
      <li>ocorrem a uma taxa média constante</li>
      <li>não ocorrem simultaneamente</li>
      </ul>

      Exemplos incluem número de chamadas recebidas por minuto, falhas em um sistema, acidentes em um trecho de estrada ou chegadas a uma fila.<br><br>

      <b>Parâmetro:</b><br>
      \\( \\lambda > 0 \\) — taxa média de ocorrência no intervalo considerado<br><br>

      <b>Propriedades:</b><br>
      $$ E[X] = \\lambda $$<br>
      $$ \\text{Var}[X] = \\lambda $$<br><br>
      
      <b>Função de probabilidade:</b><br>
      $$ P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}, \\quad k = 0,1,2,\\dots $$<br><br>

      <b>Função de distribuição acumulada:</b><br>
      $$ F(k) = P(X \\le k) = \\sum_{i=0}^{k} \\frac{\\lambda^i e^{-\\lambda}}{i!} $$<br><br>

      <b>Observações:</b><br>
      <ul>
      <li>
      <b>Média igual à variância:</b><br>
      Na Poisson, a média e a variância coincidem. Isso é uma característica marcante da distribuição e significa que a incerteza do número de eventos cresce exatamente na mesma proporção que a taxa média de ocorrência, esta é uma característica de processos aleatórios sem memória e sem interação entre eventos.
      </li><br>

      <li>
      <b>Forma da distribuição:</b><br>
      Valores pequenos de \\(\\lambda\\) produzem distribuições assimétricas à direita. À medida que \\(\\lambda\\) aumenta, a distribuição se torna mais simétrica.
      </li><br>

      <li>
      <b>Aproximação Normal:</b><br>
      Para valores grandes de \\(\\lambda\\), a distribuição Poisson pode ser bem aproximada por uma Normal com média \\(\\lambda\\) e variância \\(\\lambda\\).
      </li><br>

      <li>
      <b>Relação com a Binomial:</b><br>
      A Poisson pode ser obtida como limite da Binomial quando o número de tentativas é grande e a probabilidade de sucesso é pequena, mantendo \\( np = \\lambda \\).
      </li>
      </ul>
  ')
      )
      
    } else if (dist == "Qui-quadrado") {
      
      withMathJax(
        HTML('
      <b><span style="font-size:16px;">DISTRIBUIÇÃO QUI-QUADRADO</span></b><br><br>
  
      <b>Tipo:</b> contínua<br><br>
  
      <b>Suporte:</b> \\(x > 0 \\)<br><br>
    
      <b>Uso:</b> 
      Modela a soma dos quadrados de variáveis normais padrão independentes. 
      É amplamente utilizada em testes de hipóteses, 
  
     <b>Parâmetro:</b><br>
      ν (graus de liberdade), com ν &gt; 0<br><br>
  
     <b>Interpretação dos graus de liberdade:</b><br>
     Os graus de liberdade indicam quantas variáveis normais padrão independentes
     estão sendo somadas ao quadrado.<br><br>
  
    <b>Propriedades:</b><br>
    $$ E[X] = \\nu $$<br>
    $$ \\text{Var}[X] = 2\\nu $$<br><br>
  
    <b>Função densidade de probabilidade:</b><br>
    $$ f(x) = \\frac{1}{2^{\\nu/2} \\Gamma(\\nu/2)} x^{\\nu/2-1} e^{-x/2}, \\quad x > 0 $$<br>
    onde Γ(·) é a função Gama, responsável por normalizar a densidade.<br><br>
  
    <b>Função de distribuição acumulada:</b><br>
    $$ F(x) = P(X \\le x) = \\int_{0}^{x} f(t) \\, dt $$<br><br>
  
    <b>Observações:</b><br>
    <ul>
    <li>A distribuição qui-quadrado é sempre assimétrica à direita.</li>
    <li>À medida que ν aumenta, a assimetria diminui e a distribuição se aproxima de uma normal.</li>
    <li>Para valores pequenos de ν, a distribuição é fortemente concentrada próxima de zero.</li>
    </ul>
  
    <b>Relação com a distribuição Normal:</b><br>
    Se Z<sub>1</sub>, Z<sub>2</sub>, …, Z<sub>ν</sub> são variáveis independentes com distribuição Normal padrão,
    então:<br><br>
    $$ X = Z_1^2 + Z_2^2 + \\cdots + Z_\\nu^2 \\sim \\chi^2(\\nu) $$<br><br>
    Ou seja, a distribuição qui-quadrado surge naturalmente como a soma dos quadrados
    de variáveis normais padrão independentes.<br><br>
    
   <b>Relação com a distribuição Gama:</b><br>
   A distribuição qui-quadrado é um caso particular da distribuição Gama. 
   Mais especificamente, se \\( X \\sim \\chi^2(\\nu) \\), então 
   \\( X \\sim \\text{Gama}\\left( \\alpha = \\frac{\\nu}{2},\\; \\beta = \\frac{1}{2} \\right) \\). 
   Essa relação explica a presença da função Gama na densidade da distribuição qui-quadrado 
   e reforça sua interpretação como a soma de quantidades positivas acumuladas.
   À medida que \\( \\nu \\uparrow \\), a forma da distribuição se torna menos assimétrica.
   <br><br>

  ')
      )

    } else if (dist == "T-Student") {
      
      withMathJax(
        HTML('
      <b><span style="font-size:16px;">DISTRIBUIÇÃO T-STUDENT</span></b><br><br>

      <b>Tipo:</b> contínua<br><br>
      
      <b>Suporte:</b> \\( x \\in \\mathbb{R} \\)<br><br>

      <b>Uso:</b> 
      A distribuição t de Student surge em inferência estatística quando a variância populacional é desconhecida e o tamanho da amostra é pequeno. 
      Ela é amplamente utilizada em testes de hipóteses e intervalos de confiança para a média, bem como na comparação de médias entre grupos.<br><br>

      <b>Parâmetro:</b><br>
      \\( \\nu > 0 \\) — graus de liberdade<br><br>
      O parâmetro \\(\\nu\\) controla a incerteza associada à estimativa da variância: quanto menor \\(\\nu\\), maior a variabilidade extra e mais pesadas são as caudas da distribuição.<br><br>
      <b>Interpretação visual:</b><br>
      <ul>
      <li>\\( \\nu = 2 \\) → caudas largas</li>
      <li>\\( \\nu = 10 \\) → ainda pesada</li>
      <li>\\( \\nu = 30 \\) → praticamente Normal</li>
      </ul>
      <br><br>

      <b>Propriedades:</b><br>
      $$ E[X] = 0, \\quad \\nu > 1 $$<br>
      $$ \\text{Var}[X] = \\frac{\\nu}{\\nu - 2}, \\quad \\nu > 2 $$<br><br>

      <b>Função densidade de probabilidade:</b><br>
      $$ f(x) = 
      \\frac{\\Gamma\\left(\\frac{\\nu + 1}{2}\\right)}
      {\\sqrt{\\nu\\pi}\\,\\Gamma\\left(\\frac{\\nu}{2}\\right)}
      \\left(1 + \\frac{x^2}{\\nu}\\right)^{-\\frac{\\nu + 1}{2}} $$<br><br>

      <b>Função de distribuição acumulada:</b><br>
      $$ F(x) = P(T \\le x) = \\int_{-\\infty}^{x} f(t) \\, dt $$<br><br>

      <b>Observações:</b><br>
      <ul>
      <li>Distribuição simétrica em torno de zero</li>
      <li>Possui caudas mais pesadas que a Normal</li>
      <li>Valores extremos são mais prováveis quando \\(\\nu\\) é pequeno</li>
      <li>À medida que \\( \\nu \\to \\infty \\), a distribuição converge para a Normal padrão \\( N(0,1) \\)</li>
      </ul>
      <br><br>
      <b>Relação com a Normal e a Qui-quadrado:</b><br>
      A distribuição t de Student pode ser construída como a razão entre uma variável Normal padrão e a raiz de uma Qui-quadrado normalizada:
      $$ T = \\frac{Z}{\\sqrt{U / \\nu}}, \\quad Z \\sim N(0,1), \\; U \\sim \\chi^2(\\nu) $$
      Essa relação explica as caudas mais pesadas da distribuição t: o denominador envolve uma estimativa aleatória da variância, introduzindo incerteza adicional. 
      À medida que \\(\\nu\\) aumenta, essa incerteza diminui e a distribuição se aproxima da Normal.
')
      )
      
    } else if (dist == "Uniforme") {
      
      withMathJax(
        HTML('
    <b><span style="font-size:16px;">DISTRIBUIÇÃO UNIFORME</span></b><br><br>

    <b>Tipo:</b> contínua<br><br>
    
    <b>Suporte:</b> \\( x \\in [a, b] \\)<br><br>

    <b>Uso:</b> 
    A distribuição Uniforme modela situações em que todos os valores dentro de um intervalo finito têm a mesma probabilidade de ocorrência, não havendo preferência por nenhum valor específico. 
    É apropriada quando há completa ausência de informação sobre onde a variável tende a se concentrar.<br><br>

    <b>Parâmetros:</b><br>
    \\( a \\) — limite inferior (mínimo)<br>
    \\( b \\) — limite superior (máximo), com \\( a < b \\)<br><br>

    <b>Propriedades:</b><br>
    $$ E[X] = \\frac{a + b}{2} $$<br>
    $$ \\text{Var}[X] = \\frac{(b - a)^2}{12} $$<br><br>

     <b>Função densidade de probabilidade:</b><br>
    $$ f(x) = \\begin{cases} \\dfrac{1}{b - a}, & a \\le x \\le b \\\\
    0, & \\text{caso contrário} \\end{cases} $$<br>

    <b>Função de distribuição acumulada:</b><br>
    $$  F(x) = \\begin{cases}
    0, & x < a \\\\ \\dfrac{x - a}{b - a}, & a \\le x \\le b \\\\ 1, & x > b \\end{cases} $$<br><br>
   
    <b>Observações:</b><br>
    <ul>
    <li>A variância da distribuição Uniforme cresce quadraticamente com o comprimento do intervalo \\( b - a \\). Se o intervalo dobra de tamanho, a variância quadruplica, refletindo maior dispersão dos valores possíveis.
    <li>Distribuição simétrica em torno do ponto médio do intervalo</li>
    <li>Média = mediana = ponto médio \\( \\frac{a+b}{2} \\)</li>
    <li>Suporte finito</li>
     </ul>
    <br>
')
      )
      
    }
  
  }
  
