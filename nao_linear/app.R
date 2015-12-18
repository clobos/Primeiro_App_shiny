library(shiny)
ui<- fluidPage(
  titlePanel(
    span("Exemplo de um modelo não linear (SSasymp)", style = "color:red")    ),
  helpText("Descrição do seu aplicativo"),
  sidebarPanel(
sliderInput("Asym","Asym",min=1,max=100, step=1,value = 50,
          animate=animationOptions(interval=1000, loop=TRUE)),
sliderInput("R0","R0",min=-10,max=-1,  step=0.1,value = -5,
          animate=animationOptions(interval=1000, loop=TRUE)),
sliderInput("lrc","lrc",min=-4,max=-1,      step=0.1,value = -2)
  ),
mainPanel( 
  plotOutput("grafico"), 
  h2("Resumo de Idade (Anos)"),
  verbatimTextOutput("resumoIdade"),
  h2("Altura (pés)"),
  verbatimTextOutput("resumoAltura"))
)

server<- function(input,output){ 
  modeloSSasymp<- function(x,Asym,R0,lrc){
  y<- Asym+(R0-Asym)*exp(-exp(lrc)*x)
    return(y)
  }
  set.seed(123)
  x<- runif(150,3,95)
  mu<- modeloSSasymp(x,Asym=94,R0=-8.25,lrc=-3.22)
  y<- rnorm(length(x), mean=mu, sd=2)
  
output$resumoIdade<-  renderPrint({ summary(x) })
output$resumoAltura<- renderPrint({ summary(y) })
  
output$grafico<- renderPlot({
  plot(x,y, lwd=2,family="Bookman",cex.main=2,ylim=c(-10,100),
       xlim = c(0,max(x)),pch=19,
       xlab="Idade (anos)",ylab="Altura (pés)",
       main="y~Asym+(R0-Asym)*exp(-exp(lrc)*x)",col="red")
  curve(modeloSSasymp(x,Asym=input$Asym,R0=input$R0,lrc=input$lrc),
        0,max(x), col="blue", lwd=3, add = TRUE)
  abline(h=0,v=0,col="red", lwd=1, lty=2)
  abline(h=c(input$Asym,input$R0),
         v=-input$lrc,col="black", lwd=2, lty=3)
})  
  }
shinyApp(ui=ui, server=server)
