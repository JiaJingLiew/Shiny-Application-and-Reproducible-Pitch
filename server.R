#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
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

})
