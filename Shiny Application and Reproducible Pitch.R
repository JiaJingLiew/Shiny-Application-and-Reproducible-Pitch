Sys.setenv(LANGUAGE="en")
library(shiny)
library(psych)
library(ggplot2)
library(dplyr)
library(datasets)
library(plotly)
data("airquality")
a<-data.frame(airquality)
class(a)
describe(a)

am1<-lm(Ozone~Wind,a)
summary(am1)
anova(am1)

am2<-lm(Ozone~Wind+Temp,a)
summary(am2)
anova(am2)

p1<-plot_ly(a, x=a$Day, y=a$Ozone, type="bar") %>% layout(xaxis=list(title='Day'), yaxis=list(title='Ozone'))
p1

p2<-plot_ly(a, x=a$Day, y=a$Wind, type="scatter") %>% layout(xaxis=list(title='Day'), yaxis=list(title='Wind'))
p2

p3<-plot_ly(a, x=a$Temp, y=a$Ozone, type="bar") %>% layout(xaxis=list(title='Temp'), yaxis=list(title='Ozone'))
p3

p4<-plot_ly(a, x=a$Temp, y=a$Wind, type="bar") %>% layout(xaxis=list(title='Temp'), yaxis=list(title='Wind'))
p4

p5<-plot_ly(a, x=a$Day, y=a$Temp, type="bar") %>% layout(xaxis=list(title='Day'), yaxis=list(title='Temp'))
p5

p<-subplot(p1, p2, p3, p4)

annotations = list(
  list(x=0.15,y=1.0,text="Day-Ozone",xref="paper",yref="paper",xanchor="center",yanchor="bottom",showarrow=FALSE),
  list(x=0.4,y=1.0,text="Day-Wind",xref="paper",yref="paper",xanchor="center",yanchor="bottom",showarrow=FALSE),
  list(x=0.65,y=1.0,text="Temp-Ozone",xref="paper",yref="paper",xanchor="center",yanchor="bottom",showarrow=FALSE),
  list(x=0.9,y=1.0,text="Temp-Wind",xref="paper",yref="paper",xanchor="center",yanchor="bottom",showarrow=FALSE))

p<-p %>%layout(annotations = annotations) 

p