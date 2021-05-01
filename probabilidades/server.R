library(ggplot2)

configuracao<- theme(
  axis.title.x = element_text(color="black", 
                              size=14,
                              face="bold"), 
  axis.title.y = element_text(color="black", 
                              size=14,
                              face="bold"), 
  strip.text.x = element_text(size=14, 
                              color = "black", 
                              face = "bold"),
  strip.text.y = element_text(size=14, 
                              color = "black",
                              face = "bold"),
  axis.text.x = element_text(size = 14, 
                             color = "black", 
                             face = "bold" ),
  axis.text.y = element_text(size=14, 
                             color = "black", 
                             face = "bold") )

function(input,output){ 
output$grafico<- renderPlot({
  va <- switch(input$distribuicao,
           Normal = rnorm(input$tamanho,as.numeric(input$Media), 
                          as.numeric(input$Desvio)),
           Exponencial = rexp(input$tamanho,
                              as.numeric(input$lambda)))
  
  dados<- data.frame(x=va)
  
  v1<- ggplot(dados, aes(x))+
    geom_histogram(aes(y =..density..),
                   fill=input$Cor, 
                   stat="bin",
                   col="black")+
    configuracao
    
  if (input$distribuicao== "Normal"){
   v1+stat_function(fun=dnorm,
                  args = list(mean=mean(dados$x), sd=sd(dados$x)),
                  col="red",
                  size=3)
  } else
    v1+stat_function(fun=dexp,
                  args = list(rate=1/mean(dados$x)),
                  col="red",
                  size=3)
})
}
