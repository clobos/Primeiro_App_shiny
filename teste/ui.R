library(shiny)
fluidPage(
  headerPanel("Uso do Shiny para o ensino da Estatística"),
  sidebarPanel(
    selectInput("distribuicao", 
                "Escolher uma distribuição", 
                choices = c("Normal", "Exponencial")),
    
    selectInput(inputId = "Cor", 
                label=span("Escolha cor histograma", 
                style = "color:darkred"), 
                choices = colors(),
                selected = "yellow"),
    
    sliderInput("tamanho", 
                "Tamanho amostral...",
                min=1000,max=5000, value=500,step=50),
    
    conditionalPanel("input.distribuicao == 'Normal' ",
    textInput("Media", "Escolher media", 10),
    textInput("Desvio", "Escolher DP", 3)),
    
    conditionalPanel("input.distribuicao == 'Exponencial' ",
    textInput("lambda", "Escolher Taxa", 2))
    
  ),
  mainPanel( plotOutput("grafico") )
)

