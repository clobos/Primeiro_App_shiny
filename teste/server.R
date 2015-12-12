function(input,output){ 
output$grafico<- renderPlot({
  va <- switch(input$distribuicao,
           Normal = rnorm(input$tamanho,as.numeric(input$Media), as.numeric(input$Desvio)),
           Exponencial = rexp(input$tamanho,as.numeric(input$lambda)))
  
  hist(va, col = input$Cor, 
       main=paste("Histograma da distribuição", input$distribuicao),
       xlab="x", ylab="Freq. absoluta")
})
}
