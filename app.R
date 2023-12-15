library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(yfR)
library(ggplot2)
library(bslib)
library(rugarch)
library(xts)
library(dygraphs)
library(fpp3)
library(tidyverse)
library(rsconnect)



my_theme = create_theme(
  adminlte_color(
    light_blue = "#4285f4",
    yellow = "#fbbc05",
    blue = "#285092"
  )
)

nome_acao <- "GOOG"   # C?digo no Yahoo Finance
data_ini  <- "2010-01-01" # Data de inicio
data_fim  <- Sys.Date() # Data de fim
precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
retornos <- precos$ret_adjusted_prices[-1]
tsplot <- ts.plot(retornos)

#Dados no formato tsibble
precos_tsibble <- precos %>% as_tsibble(index = ref_date) %>%
  as_tsibble(index = ref_date) %>% fill_gaps()

#Dados sem datas faltantes
n <- nrow(precos)
precos_tsibble <- precos %>% 
  mutate(ref_date = seq(as.Date("2010/01/01"), by = "day", length.out = n)) %>%
  as_tsibble(index = ref_date) %>%
  as_tsibble(index = ref_date)

#Dados para os gr?ficos interativos
don <- xts(x = precos$price_adjusted, order.by = precos$ref_date)
don2 <- xts(x = precos$ret_adjusted_prices, order.by = precos$ref_date)

#Regressão
modelo <- precos_tsibble |> model(TSLM(ret_adjusted_prices ~ trend() + season() + price_low + volume))
report(modelo)
plot_regress <- augment(modelo) %>%
  ggplot(aes(x = ref_date)) +
  geom_line(aes(y = ret_adjusted_prices, colour = "Retornos")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Retornos = "black", Fitted = "red")) +
  ylab("Retornos") + 
  xlab("Data") +
  ggtitle("Retornos. Período 2010-2023") + 
  guides(colour = guide_legend(title = "séries"))

#Fun??es para escolha de par?metros
funcao_snaive <- function(sazonalidade, dados_tsibble){
  fit_SNaive <- dados_tsibble |> model(SNAIVE(ret_adjusted_prices ~ lag(2*sazonalidade)))
  fit_SNaive %>% gg_tsresiduals()
}

ar_garch <- function(p, q, n, m, retorno){
  centrada <- na.omit(retorno) - mean(retorno, na.rm = T)
  spec <- ugarchspec(mean.model = list(armaOrder = c(p, q), include.mean = FALSE),
                     variance.model = list(model = 'sGARCH', garchOrder = c(n, m)),
                     distribution = 'norm')
  
  fit_01 <- ugarchfit(spec, centrada, solver = 'hybrid')
  e_hat = fit_01@fit$residuals/fit_01@fit$sigma
  par(mfrow=c(2,2))
  plot(acf(fit_01@fit$residuals^2, plot = FALSE), main = "Resíduos^2")
  acf(e_hat)
  acf(e_hat^2)
  qqnorm(e_hat)
}

funcao_arima  <- function(p, q, i, retorno){
  `Retonos diferenciados` <- diff(na.omit(retorno), differences = i)
  fit <- arima(na.omit(retorno), order = c(p, i, q))
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  ts.plot(`Retonos diferenciados`)
  plot(acf(na.omit(retorno), plot = FALSE), main = "Series Retornos")
  acf(`Retonos diferenciados`)
}

#Tabela
pvalor <- c(0.9967, 0.9991, 0.9988)
pvalor <- as.double(pvalor)
Modelo <- c("Garch(0,1)", "Arma(2,1)-Garch(1,1)", "Arma(2,2)-Garch(1,2)")
tabela <- cbind(Modelo, pvalor)


ui <- fluidPage(theme = my_theme, dashboardPage(
  dashboardHeader(title = "Análise Temporal de ações do Google"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Capa", tabName = "capa"),
      menuItem("Introdução", tabName = "Introdução"),
      menuItem("Contexto", tabName = "contexto"),
      menuItem("Modelo", tabName = "modelo"),
      menuItem("Análises", tabName = "Análises"),
      menuItem("Diagnóstico", tabName = "Diagnóstico"),
      menuItem("Validação", tabName = "Validação"),
      menuItem("Conclusão", tabName = "Conclusão")
    ),
    sidebar <- tags$head(tags$style(HTML(".sidebar {background-color: #4285f4;color: black;}")))
  ),
  dashboardBody(use_theme(my_theme),
                tags$head(
                  tags$style(
                    HTML(
                      '
                       #sidebar {
            background-color: #34a853;
            color: white;
            font-family: "lexend";
        }
        /* Custom CSS styles */
        .skin-blue .main-header .logo{
          color: white;
          font-weight: bold;
          font-size: 24px;
        }
        
        /* Custom CSS styles */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          color: white;
        }
        
        /* Custom CSS styles */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
          color: white;
        }
        
        /* Custom CSS styles */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
          color: white;
        }
        
        /* body */
        .content-wrapper, .right-side {
        background-color: white;
        color: black;
        }        
        '
                    )
                  )
                ),
                tabItems(
                  # Cover tab
                  tabItem(tabName = "capa",
                          includeHTML("Capa.html")
                  ),
                  
                  # Introdução tab
                  tabItem(tabName = "Introdução",
                          h1("Introdução"),
                          h4("O presente dashboard tem por objetivo apresentar
                             uma analise detalhada do comportamento das acoes da
                             Alphabet, conglomerado dono da Google, representada
                             pela sigla GOOG."),
                          h4("Primeiramente sera apresentada uma explicacao sobre
                             o mercado de acoes e o comportamento da acao da 
                             empresa Alphabet nos ultimos 13 anos. Posteriormente
                             serao testados modelos de series temporais que se 
                             adequem aos dados, atraves de um diagnóstico e da
                             tecnica de Cross Validation para escolher o melhor
                             dentre os modelos que se sobressairem. Por fim serao
                             apresentadas as conclusoes tiradas dessa análise."),
                          h4("Pelas análises pode-se concluir que o modelo que 
                             melhor se adequou aos dados das acoes da Alphabet foi
                             um ARMA(2,1)-GARCH(1,1), o qual foi colocado em produção
                             no Github para realizar previsões dos retornos de GOOG
                             diariamente."),
                          # Add your Introdução content here
                  ),
                  
                  # Contexto tab
                  tabItem(tabName = "contexto",
                          h1("Contexto"),
                          h4("O mercado de ações é responsável pelas negociações
                             que são feitas sobre ações, opções, fundos imobiliários,
                             entre outros. As transações monetárias ocorrem
                             principalmente através das bolsas de valores (no
                             Brasil temos a B3) e nos mercados de balcão (onde
                             são negociados títulos que não estão em bolsas).  
                             Hoje em dia, na maioria das economias, as operações 
                             do mercado acionário ocorrem por intermédio das bolsas 
                             de valores, sendo que para investir em ações, é necessário 
                             buscar por uma corretora de valores."),
                          h4("A NASDAQ, bolsa em que é negociada as ações da Alphabet, 
                             é um dos mercados de ações dos Estados Unidos, sendo o 
                             segundo maior do país, apenas atrás da Bolsa de Valores 
                             de Nova Iorque. Essa bolsa possui mais de 2800 ações 
                             e foi fundada em 1971."),
                          h4("Ações são a menor parcela do capital das companhias 
                             que possuem capital aberto, sendo um título patrimonial 
                             que concede aos seus detentores (acionistas) os direitos 
                             e deveres de um sócio. Os acionistas podem receber parte 
                             do lucro das empresas através de dividendos, juros sobre
                             o capital próprio e bonificações (quando companhias
                             distribuem novas ações). Seus valores variam conforme
                             oferta e demanda dentro da bolsa de valores, podendo
                             ser negociadas dentro do horário definido pela bolsa
                             (a NASDAQ funciona entre 14h30 e 21h do horário de Brasília).
                             Essa variação pode ser influenciada por diversos fatores
                             externos, como decisões governamentais, guerras, informações
                             da empresa, percepção pública, entre outros, logo torna-se
                             muito difícil conseguir prever o comportamento dessas ações,
                             sendo necessários modelos complexos de séries temporais
                             para tentar modelar parte do comportamento das mesmas."),
                          # Output: Histogram Plot
                  ),
                  
                  # Modelo tab
                  tabItem(tabName = "modelo",
                          h1("Modelo"),
                          tabsetPanel(
                            tabPanel(
                              title = "Serie",
                              dygraphOutput("plot")
                            ),
                            tabPanel(
                              title = "Retornos",
                              dygraphOutput("ret")
                            ),
                            tabPanel(
                              title = "ACF",
                              plotOutput("acf")
                            ),
                            tabPanel(
                              title = "PACF",
                              plotOutput("pacf")
                            )),
                  ),
                  
                  
                  # Analysis tab
                  tabItem(tabName = "Análises",
                          h1("Análises"),
                          tabsetPanel(
                            tabPanel("AR(I)MA",
                                     sidebarLayout(
                                       sidebarPanel(id = "sidebar",
                                                    numericInput("AR", min = 0, max = 10, value = 1, label="AR"),
                                                    numericInput("I", min = 0, max = 10, value = 1, label="I"),
                                                    numericInput("MA", min = 0, max = 10, value = 1, label="MA"),
                                                    actionButton("rodar_ARIMA", label="Escolha o modelo")
                                       ),
                                       mainPanel(
                                         plotOutput("result_ARIMA")
                                       )
                                     )),
                            tabPanel("ARMA-GARCH", 
                                     sidebarLayout(
                                       sidebarPanel(id = "sidebar",
                                                    numericInput("AR", min = 0, max = 10, value = 1, label="AR"),
                                                    numericInput("MA", min = 0, max = 10, value = 1, label="MA"),
                                                    numericInput("GARCH_P", min = 0, max = 10, value = 1, label="GARCH - P"),
                                                    numericInput("GARCH_Q", min = 0, max = 10, value = 1, label="GARCH - Q"),
                                                    actionButton("rodar_GARCH", label="Escolha o modelo")
                                       ),
                                       mainPanel(
                                         plotOutput("result")
                                       )
                                     )),
                            tabPanel("DRIFT", plotOutput("result_DRIFT")),
                            tabPanel("NAIVE", plotOutput("result_NAIVE")),
                            tabPanel("SNAIVE",
                                     sidebarLayout(
                                       sidebarPanel(id = "sidebar",
                                                    numericInput("sazonalidade", min = 0, max = 100, value = 1, label="PerÃ­odo Sazonal"),
                                                    actionButton("rodar_SNAIVE", label="Escolha o modelo")
                                       ),
                                       mainPanel(
                                         plotOutput("result_SNAIVE")
                                       )
                                     )),
                            tabPanel("Média", plotOutput("result_MEAN")),
                            tabPanel("Regressão", textOutput("regress"), plotOutput("result_regress")),
                            tabPanel("Suavização Exponencial", textOutput("SE"), plotOutput("result_SE")),
                            tabPanel("HWA", textOutput("HWA"), plotOutput("result_HWA"))
                          )),
                  
                  # Validation tab
                  tabItem(tabName = "Diagnóstico",
                          h1("Diagnóstico"),
                          h4("Como visto anteriormente na seção 'Análises', modelos mais simples (Média, NAIVE, DRIFT, entre outros)
                             , não são adequados para dados voláteis, como as ações de uma empresa. Dessa forma, foram filtrados apenas
                             os modelos que consegue capturar, ao menos parcialmente, o comportamento dos dados. Sendo assim, apenas os
                             modelos ARMA-GARCH foram selecionados para que fosse realizado um dianóstico mais aprofundado, a fim de
                             encontrar o modelo, dentro dessa classe de modelos, que melhor se ajustasse aos dados, como será apresentado
                             abaixo."),
                          h4("O gráfico de ACF dos retornos ao quadrado mostraram que é necessário ajustar um modelo GARCH.
                          Após isto, foram testados e feitos os diagnósticos de 13 diferentes tipos de Modelo GARCH,
                          sendo avaliado a significância dos parâmetros e o valor de AIC (AKAIKE), que quanto menor melhor.
                          Após ser feito os diagnósticos se chegou que os 3 melhores modelos são: 
                          Garch(0,1), Arma(2,1)-Garch(1,1) e Arma(2,2)-Garch(1,2), 
                          a seguir será feito Cross-Validation para validar estes 3 amodelos."),
                          mainPanel(
                            tabsetPanel(tabPanel("Garch(0,1)", plotOutput("diag0")),
                                        tabPanel("Arma(2,1)-Garch(1,1)", plotOutput("diag")),
                                        tabPanel("Arma(2,2)-Garch(1,2)", plotOutput("diag2"))))
                          # Add your validation content here
                  ),
                  
                  # Results tab
                  tabItem(tabName = "Validação",
                          h1("Validação"),
                          mainPanel(textOutput("val"), tableOutput("tabela"))
                          # Add your results content here
                  ),
                  
                  # Conclusion tab
                  tabItem(tabName = "Conclusão",
                          h1("Conclusão"),
                          h4("Com o objetivo de modelar o comportamento dos retornos
                             das ações GOOG (Alphabet), foram feitas análises de sua
                             série temporal dos últimos 13 anos. Pode-se notar que
                             por se tratar de ações, seu comportamento é altamente
                             volátil e não previsível, logo, como esperado, modelos
                             mais 'ingênuos', como os modelos de Média, NAIVE, SNAIVE,
                             entre outros, não tiveram um bom desempenho."),
                          h4("Dessa forma, foram testados modelos mais complexos (AR(I)MA
                             e ARMA-GARCH), que provavelmente iriam trazer resultados
                             melhores. Para isso, foram feitas análises exploratórias
                             dos dados e, para os modelos que se mostrassem melhores,
                             foi feita uma validação cruzada."),
                          h4("Sendo assim, através das análises citadas acima, pode-se
                             chegar a Conclusão de que o modelo que melhor se adequou aos
                             dados foi o modelo ARMA(2,1)-GARCH(1,1)."),
                          h4("Por fim, este modelo foi implementado na plataforma Github
                             para que trouxesse previsões diárias de qual seria o retorno
                             das ações GOOG no Período, podendo ser acessado através do link:
                             https://github.com/Joao-Formigari/Previsao_Series_GOOG"),
                          
                          
                          
                          # Add your conclusion content here
                  )
                )
  )
)
)

server <- function(input, output) {
  thematic::thematic_shiny()
  output$plot <- renderDygraph({
    dygraph(don) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#ea4335") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
  })
  removeModal()
  output$ret <- output$ret <- renderDygraph({
    dygraph(don2) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#ea4335") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
  })
  removeModal()
  
  output$acf <- renderPlot({plot(acf(na.omit(precos$ret_adjusted_prices), plot = FALSE), main = "Retornos")})
  output$pacf <- renderPlot({plot(pacf(na.omit(precos$ret_adjusted_prices), plot = FALSE), main = "Retornos")})
  
  observeEvent(input$rodar_GARCH, {output$result = renderPlot({({return(ar_garch(isolate(input$AR),
                                                                                 isolate(input$MA),
                                                                                 isolate(input$GARCH_P),
                                                                                 isolate(input$GARCH_Q),
                                                                                 precos$ret_adjusted_prices[-1]))})})})
  observeEvent(input$rodar_ARIMA, {
    output$result_ARIMA = renderPlot({({return(funcao_arima(isolate(input$AR),
                                                            isolate(input$I),
                                                            isolate(input$MA),
                                                            precos$ret_adjusted_prices[-1]))})})})
  output$result_NAIVE <- renderPlot({fit_Naive <- precos_tsibble |> model(NAIVE(ret_adjusted_prices))
  fit_Naive %>% gg_tsresiduals()
  })
  observeEvent(input$rodar_SNAIVE, {output$result_SNAIVE = renderPlot({({return(funcao_snaive(isolate(input$sazonalidade),
                                                                                              precos_tsibble))})})})
  output$result_DRIFT <- renderPlot({fit_drift <- precos_tsibble |> model(RW(ret_adjusted_prices ~ drift()))
  fit_drift %>% gg_tsresiduals()})
  output$result_MEAN <- renderPlot({fit_Mean <- precos_tsibble |> model(MEAN(ret_adjusted_prices))
  fit_Mean%>% gg_tsresiduals()})
  output$result_regress <- renderPlot({gg_tsresiduals(modelo)})
  output$regress <- renderText({paste("Abaixo apresentados os Resultados obtidos",
                                      "apartir da realização do modelo de Regressão com as",
                                      " variáveis preditoras: tendência, sazonalidade, menor preço, volume.")})
  output$result_SE <- renderPlot({fit_se <- na.omit(precos_tsibble) |> model(ETS(ret_adjusted_prices ~ error("A")))
  fit_se %>% gg_tsresiduals()})
  output$SE <- renderText({paste("Abaixo apresentados os Resultados obtidos",
                                 "apartir da realização do modelo de Suavização Exponencial com erro aditivo.")})
  output$result_HWA <- renderPlot({(
    na.omit(precos_tsibble) |> model(HWA = ETS(ret_adjusted_prices ~ error("A") + trend("Ad") + season("A")))
    %>% gg_tsresiduals()
  )})
  output$HWA <- renderText({paste("Abaixo apresentados os Resultados obtidos",
                                  "apartir da realização do modelo de Holt Winters Amortecido com erro e sazonalidade aditivos.")})
  #output$diagtext <- renderText({paste("O gráfico de ACF dos retornos ao quadrado mostraram que é necessário ajustar um modelo GARCH.
  #                        Após isto, foram testados e feitos os diagnósticos de 13 diferentes tipos de Modelo GARCH,
  #                        sendo avaliado a significância dos parâmetros e o valor de AIC (AKAIKE), que quanto menor melhor.
  #                        Após ser feito os diagnósticos se chegou que os 3 melhores modelos são: 
  #                        Garch(0,1), Arma(2,1)-Garch(1,1) e Arma(2,2)-Garch(1,2), 
  #                        a seguir será feito Cross-Validation para validar estes 3 amodelos.")})
  output$diag0 <- renderPlot({ar_garch(0, 0, 0, 1, retornos)})
  output$diag <- renderPlot({ar_garch(2, 1, 1, 1, retornos)})
  output$diag2 <- renderPlot({ar_garch(2, 2, 1, 1, retornos)})
  output$val <- renderText({paste("Foi realizado Cross-Validation usando rolling windownos 3 melhores modelos
                             segundo os diagnósticos, Foi aplicado o teste de calibração, cuja hipótese nula é: 
                             o VaR/ES estão corretamente especificado, e a hipótese alternativa é:
                             o VaR/ES não estão corretamente especificado. 
                             O modelo Garch(0,1) obteve um p-valor de 0,9967, o Arma(2,1)-Garch(1,1) um p-valor 
                             de 0,9991 e o Arma(2,2)-Garch(1,2) um p-valor de 0,9988,
                             portanto os 3 modelos se mostraram bem especificados, 
                             porém o Arma(2,1)-Garch(1,1) se mostrou o melhor modelo por ter o maior p-valor.")})
  output$tabela <- renderTable(tabela)
}
shinyApp(ui, server)
