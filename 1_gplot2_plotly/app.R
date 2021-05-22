################################################
# Exemplo: Motivação
################################################
library(ggplot2)
library(plotly)
library(shiny)

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
    axis.text.x = element_text(size=14, 
                               color = "black", 
                               face = "bold" ),
    axis.text.y = element_text(size=14, 
                               color = "black", 
                               face = "bold") )

ui <- fluidPage(

    titlePanel("Motivação: ggplot2 e plotly"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("intervalos",
                        "Num. de intervalos",
                        min = 10,
                        max = 50,
                        value = 25)
        ),

        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlotly({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), 
                    length.out = input$intervalos + 1)

        v1<- ggplot(faithful, aes(waiting))+
            geom_histogram(breaks=bins,
                           col="red",
                           fill="black")+
            theme_classic()+configuracao
        ggplotly(v1)
        })
}

shinyApp(ui = ui, server = server)
