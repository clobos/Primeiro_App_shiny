library(shiny)
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

ui<- fluidPage(
  titlePanel(
    span("Exemplo de um modelo não linear:
         y~Asym+(R0-Asym)*exp(-exp(lrc)*x)", 
         style = "color:red")    ),
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
  
dados<- data.frame(x,y)

  ggplot(dados, aes(x,y))+
    geom_point(col="black",size=6,pch=20)+
    configuracao+
    labs(x="Idade (Anos)", y="Altura (pés)")+
    stat_function(fun=modeloSSasymp,
      args = list(Asym=input$Asym,R0=input$R0,lrc=input$lrc),
      col="red",size=3)+
    geom_hline(yintercept = c(input$Asym,input$R0),
               col="gray", lty=2)
    })  
  }
shinyApp(ui=ui, server=server)
