library(shiny)
library(forecast)

ui<- fluidPage(
    titlePanel(
        span("ARMA(p,q) y_t=c+phi*y_{t-1}+theta*erro_{t-1}+erro_{t}", 
             style = "color:red")    ),
    helpText("Descrição do seu aplicativo"),
    sidebarPanel(
        sliderInput("p","phi",min=-1,max=1, step=0.1,value = -0.5,
                    animate=animationOptions(interval=1000, loop=TRUE)),
        sliderInput("q","theta",min=-1,max=1, step=0.1,value = 0.5,
                    animate=animationOptions(interval=1000, loop=TRUE))
        
    ),
    mainPanel( 
        plotOutput("grafico"),
        verbatimTextOutput("AjusteModelo"),
        )
)

server<- function(input,output){ 
    
    output$grafico<- renderPlot({
        set.seed(123)
        data.ts = arima.sim(n = 500, 
                            list(order=c(1,0,1),
                                 ar = input$p, 
                                 ma = input$q))
        ggtsdisplay(data.ts)
        
    })
    output$AjusteModelo<-  renderPrint({ 
        set.seed(123)
        data.ts = arima.sim(n = 100, 
                            list(order=c(1,0,1),
                                 ar = input$p, 
                                 ma = input$q))
        ajusteARIMA<- auto.arima(data.ts,
                                 stationary = TRUE,
                                 seasonal = FALSE)#Arima(data.ts, order = c(1,0,1)) 
        summary(ajusteARIMA)
        })
}
shinyApp(ui=ui, server=server)
