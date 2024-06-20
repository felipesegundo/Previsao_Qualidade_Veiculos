# Carrega as bibliotecas necessárias
library(shiny)
library(e1071)
library(dplyr)

# Carrega dados
carros = read.csv("car.data", sep = ",")

# Traduzindo os valores da variável alvo
carros$buying <- recode(carros$buying, "vhigh" = "muito_alto", "high" = "alto", "med" = "médio", "low" = "baixo")
carros$maint <- recode(carros$maint, "vhigh" = "muito_alto", "high" = "alto", "med" = "médio", "low" = "baixo")
carros$doors <- recode(carros$doors, "2" = "2", "3" = "3", "4" = "4", "5more" = "5ou_mais")
carros$persons <- recode(carros$persons, "2" = "2", "4" = "4", "more" = "mais")
carros$lug_boot <- recode(carros$lug_boot, "small" = "pequeno", "med" = "médio", "big" = "grande")
carros$safety <- recode(carros$safety, "low" = "baixa", "med" = "média", "high" = "alta")
carros$class <- recode(carros$class, "unacc" = "inaceitável", "acc" = "aceitável", "good" = "bom", "vgood" = "muito_bom")

# Cria o modelo Naive Bayes
modelo <- naiveBayes(class ~ ., carros)

# Separa os dados únicos para preencher controles
buying = unique(carros$buying)
maint = unique(carros$maint)
doors = unique(carros$doors)
persons = unique(carros$persons)
lug_boot = unique(carros$lug_boot)
safety = unique(carros$safety)

# Define a interface do usuário (UI)
ui <- fluidPage(
  # Adiciona estilos CSS personalizados
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f2f2f2;  # Define a cor de fundo da página
        font-family: Arial, sans-serif;  # Define a fonte da página
      }
      .title {
        color: #cc0000;  # Define a cor do título
        text-align: center;  # Centraliza o texto do título
        padding: 10px;  # Adiciona espaçamento ao redor do título
      }
      .panel {
        border: 2px solid #cccccc;  # Define a borda dos painéis
        border-radius: 10px;  # Arredonda os cantos dos painéis
        padding: 15px;  # Adiciona espaçamento interno aos painéis
        margin: 10px;  # Adiciona espaçamento externo aos painéis
        background-color: #ffffff;  # Define a cor de fundo dos painéis
      }
      .btn-primary {
        background-color: #cc0000;  # Define a cor de fundo do botão
        border-color: #cc0000;  # Define a cor da borda do botão
      }
      .btn-primary:hover {
        background-color: #990000;  # Define a cor de fundo do botão ao passar o mouse
        border-color: #990000;  # Define a cor da borda do botão ao passar o mouse
      }
      .result {
        color: #000000;  # Define a cor do texto do resultado
        font-size: 24px;  # Define o tamanho da fonte do texto do resultado
        text-align: center;  # Centraliza o texto do resultado
        margin-top: 20px;  # Adiciona espaçamento acima do texto do resultado
      }
    "))
  ),
  
  # Define o painel de título
  titlePanel("Previsão de Qualidade de Veículos", windowTitle = "Qualidade de Veículos"),
  
  # Define a primeira linha de seletores de entrada
  fluidRow(
    column(4,
           div(class = "panel",
               selectInput("buying", "Preço:", choices = buying)  # Seletor para preço
           )
    ),
    column(4,
           div(class = "panel",
               selectInput("maint", "Manutenção:", choices = maint)  # Seletor para manutenção
           )
    ),
    column(4,
           div(class = "panel",
               selectInput("doors", "Portas:", choices = doors)  # Seletor para portas
           )
    )
  ),
  # Define a segunda linha de seletores de entrada
  fluidRow(
    column(4,
           div(class = "panel",
               selectInput("persons", "Capacidade de Passageiros:", choices = persons)  # Seletor para capacidade de passageiros
           )
    ),
    column(4,
           div(class = "panel",
               selectInput("lug_boot", "Porta-malas:", choices = lug_boot)  # Seletor para porta-malas
           )
    ),
    column(4,
           div(class = "panel",
               selectInput("safety", "Segurança:", choices = safety)  # Seletor para segurança
           )
    )
  ),
  # Define a linha para o botão de ação e o texto de resultado
  fluidRow(
    column(12,
           div(class = "panel",
               actionButton("Processar", "Processar", class = "btn-primary"),  # Botão para processar a previsão
               h1(textOutput("Resultado"), class = "result")  # Texto para mostrar o resultado da previsão
           )
    )
  )
)

# Define o servidor
server <- function(input, output) {
  observeEvent(input$Processar, {
    # Cria um novo dataframe com os valores selecionados pelo usuário
    novocarro = data.frame("buying" = input$buying, "maint" = input$maint, "doors" = input$doors, "persons" = input$persons, "lug_boot" = input$lug_boot, "safety" = input$safety)
    # Faz a previsão usando o modelo
    predicao = predict(modelo, novocarro)
    # Exibe o resultado da previsão
    output$Resultado = renderText({ as.character(predicao) })
  })
}

# Executa o aplicativo Shiny
shinyApp(ui = ui, server = server)
