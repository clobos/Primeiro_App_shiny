library(shiny)
ui<- fluidPage(
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

server<- function(input,output){ 
output$grafico<- renderPlot({
  va <- switch(input$distribuicao,
           Normal = rnorm(input$tamanho,as.numeric(input$Media), as.numeric(input$Desvio)),
           Exponencial = rexp(input$tamanho,as.numeric(input$lambda)))
  
  hist(va, col = input$Cor, 
       main=paste("Histograma da distribuição", input$distribuicao),
       xlab="x", ylab="Freq. absoluta")
})
}
shinyApp(ui=ui, server=server)

