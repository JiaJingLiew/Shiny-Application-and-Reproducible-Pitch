#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(plotly)
library(ggplot2)
data(airquality)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("New York Airquality"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Wind",label="Strength Of Wind:",min = 0,max = 125,value = 80),
            sliderInput("Ozone",label="Strength Of Ozone:",min = 1,max = 25,value = 10),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  step1<-reactive(a<-data.frame(airquality))

    output$distPlot <- renderPlot({
        a<-step1()
        am1<-lm(Ozone~Wind,a)
        Wind<-a$Wind
        Temp<-a$Temp
        Ozone<-a$Ozone
        plot(Wind,Ozone, main="Wind-Ozone", col="lightblue",xlab="Wind(mph)", xlim=c(0,25))
        abline(h=input$Wind,col="dimgrey",lwd=2)
        abline(v=input$Ozone,col="burlywood",lwd=2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
