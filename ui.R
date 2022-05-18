#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

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
))
