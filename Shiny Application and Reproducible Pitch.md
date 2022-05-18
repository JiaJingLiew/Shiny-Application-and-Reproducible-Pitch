---
title: "Shiny Application and Reproducible Pitch"
author: "Jia Jing Liew"
date: "2022-05-18"
output: html_document
---



0.  Abstract\
    In this presentation, i will discuss about New York Air Quality, through linear regression model. The measurements was done from May to September 1973, which had measured temperature(degrees F), solar radiation(lang), etc.

1.  Introduction\
    First, loading data and making sure the data class or other information


```r
Sys.setenv(LANGUAGE="en")
library(shiny)
library(psych)
```

```
## Warning: package 'psych' was built under R version 4.1.3
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.1.3
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following objects are masked from 'package:psych':
## 
##     %+%, alpha
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.1.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(datasets)
library(plotly)
```

```
## Warning: package 'plotly' was built under R version 4.1.3
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
data("airquality")
a<-data.frame(airquality)
class(a)
```

```
## [1] "data.frame"
```

```r
describe(a)
```

```
##         vars   n   mean    sd median trimmed   mad  min   max range  skew kurtosis   se
## Ozone      1 116  42.13 32.99   31.5   37.80 25.95  1.0 168.0   167  1.21     1.11 3.06
## Solar.R    2 146 185.93 90.06  205.0  190.34 98.59  7.0 334.0   327 -0.42    -1.00 7.45
## Wind       3 153   9.96  3.52    9.7    9.87  3.41  1.7  20.7    19  0.34     0.03 0.28
## Temp       4 153  77.88  9.47   79.0   78.28  8.90 56.0  97.0    41 -0.37    -0.46 0.77
## Month      5 153   6.99  1.42    7.0    6.99  1.48  5.0   9.0     4  0.00    -1.32 0.11
## Day        6 153  15.80  8.86   16.0   15.80 11.86  1.0  31.0    30  0.00    -1.22 0.72
```

```r
dim(a)
```

```
## [1] 153   6
```

```r
length(unique(is.na(a)))
```

```
## [1] 24
```

2.  Making linear regression model\
    From the before, we know that the data airquality has 6 variables. We can make some assumptions, likely "Ozone" and "Wind" has some relations(actually them haven't relation).


```r
am1<-lm(Ozone~Wind,a)
summary(am1)
```

```
## 
## Call:
## lm(formula = Ozone ~ Wind, data = a)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.572 -18.854  -4.868  15.234  90.000 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  96.8729     7.2387   13.38  < 2e-16 ***
## Wind         -5.5509     0.6904   -8.04 9.27e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.47 on 114 degrees of freedom
##   (37 observations deleted due to missingness)
## Multiple R-squared:  0.3619,	Adjusted R-squared:  0.3563 
## F-statistic: 64.64 on 1 and 114 DF,  p-value: 9.272e-13
```

```r
anova(am1)
```

```
## Analysis of Variance Table
## 
## Response: Ozone
##            Df Sum Sq Mean Sq F value    Pr(>F)    
## Wind        1  45284   45284  64.644 9.272e-13 ***
## Residuals 114  79859     701                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
am2<-lm(Ozone~Wind+Temp,a)
summary(am2)
```

```
## 
## Call:
## lm(formula = Ozone ~ Wind + Temp, data = a)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -41.251 -13.695  -2.856  11.390 100.367 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -71.0332    23.5780  -3.013   0.0032 ** 
## Wind         -3.0555     0.6633  -4.607 1.08e-05 ***
## Temp          1.8402     0.2500   7.362 3.15e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.85 on 113 degrees of freedom
##   (37 observations deleted due to missingness)
## Multiple R-squared:  0.5687,	Adjusted R-squared:  0.5611 
## F-statistic:  74.5 on 2 and 113 DF,  p-value: < 2.2e-16
```

```r
anova(am2)
```

```
## Analysis of Variance Table
## 
## Response: Ozone
##            Df Sum Sq Mean Sq F value    Pr(>F)    
## Wind        1  45284   45284  94.808 < 2.2e-16 ***
## Temp        1  25886   25886  54.196 3.149e-11 ***
## Residuals 113  53973     478                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

3.  Ploting the graphs in bar plot Now, we know "Ozone" and "Wind" has clearly from their F values, and we are plotting the graphs


```r
p1<-plot_ly(a, x=a$Day, y=a$Ozone, type="bar", col="green") %>% layout(xaxis=list(title='Day'),yaxis=list(title='Ozone'))
p1
```

```
## Warning: Ignoring 37 observations
```

```
## Warning: 'bar' objects don't have these attributes: 'col'
## Valid attributes include:
## '_deprecated', 'alignmentgroup', 'base', 'basesrc', 'cliponaxis', 'constraintext', 'customdata', 'customdatasrc', 'dx', 'dy', 'error_x', 'error_y', 'hoverinfo', 'hoverinfosrc', 'hoverlabel', 'hovertemplate', 'hovertemplatesrc', 'hovertext', 'hovertextsrc', 'ids', 'idssrc', 'insidetextanchor', 'insidetextfont', 'legendgroup', 'legendgrouptitle', 'legendrank', 'marker', 'meta', 'metasrc', 'name', 'offset', 'offsetgroup', 'offsetsrc', 'opacity', 'orientation', 'outsidetextfont', 'selected', 'selectedpoints', 'showlegend', 'stream', 'text', 'textangle', 'textfont', 'textposition', 'textpositionsrc', 'textsrc', 'texttemplate', 'texttemplatesrc', 'transforms', 'type', 'uid', 'uirevision', 'unselected', 'visible', 'width', 'widthsrc', 'x', 'x0', 'xaxis', 'xcalendar', 'xhoverformat', 'xperiod', 'xperiod0', 'xperiodalignment', 'xsrc', 'y', 'y0', 'yaxis', 'ycalendar', 'yhoverformat', 'yperiod', 'yperiod0', 'yperiodalignment', 'ysrc', 'key', 'set', 'frame', 'transforms', '_isNestedKey', '_isSimpleKey', '_isGraticule', '_bbox'
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
p2<-plot_ly(a, x=a$Day, y=a$Wind, type="bar") %>% layout(xaxis=list(title='Day'), yaxis=list(title='Wind'))
p2
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
p3<-plot_ly(a, x=a$Temp, y=a$Ozone, type="bar") %>% layout(xaxis=list(title='Temp'), yaxis=list(title='Ozone'))
p3
```

```
## Warning: Ignoring 37 observations
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
p4<-plot_ly(a, x=a$Temp, y=a$Wind, type="bar") %>% layout(xaxis=list(title='Temp'), yaxis=list(title='Wind'))
p4
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
p5<-plot_ly(a, x=a$Day, y=a$Temp, type="bar") %>% layout(xaxis=list(title='Day'), yaxis=list(title='Temp'))
p5
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
p<-subplot(p1, p2, p3, p4)
```

```
## Warning: Ignoring 37 observations

## Warning: 'bar' objects don't have these attributes: 'col'
## Valid attributes include:
## '_deprecated', 'alignmentgroup', 'base', 'basesrc', 'cliponaxis', 'constraintext', 'customdata', 'customdatasrc', 'dx', 'dy', 'error_x', 'error_y', 'hoverinfo', 'hoverinfosrc', 'hoverlabel', 'hovertemplate', 'hovertemplatesrc', 'hovertext', 'hovertextsrc', 'ids', 'idssrc', 'insidetextanchor', 'insidetextfont', 'legendgroup', 'legendgrouptitle', 'legendrank', 'marker', 'meta', 'metasrc', 'name', 'offset', 'offsetgroup', 'offsetsrc', 'opacity', 'orientation', 'outsidetextfont', 'selected', 'selectedpoints', 'showlegend', 'stream', 'text', 'textangle', 'textfont', 'textposition', 'textpositionsrc', 'textsrc', 'texttemplate', 'texttemplatesrc', 'transforms', 'type', 'uid', 'uirevision', 'unselected', 'visible', 'width', 'widthsrc', 'x', 'x0', 'xaxis', 'xcalendar', 'xhoverformat', 'xperiod', 'xperiod0', 'xperiodalignment', 'xsrc', 'y', 'y0', 'yaxis', 'ycalendar', 'yhoverformat', 'yperiod', 'yperiod0', 'yperiodalignment', 'ysrc', 'key', 'set', 'frame', 'transforms', '_isNestedKey', '_isSimpleKey', '_isGraticule', '_bbox'
```

```
## Warning: Ignoring 37 observations
```

```r
annotations = list(
  list(x=0.15,y=1.0,text="Day-Ozone",xref="paper",yref="paper",xanchor="center",yanchor="bottom",showarrow=FALSE),
  list(x=0.4,y=1.0,text="Day-Wind",xref="paper",yref="paper",xanchor="center",yanchor="bottom",showarrow=FALSE),
  list(x=0.65,y=1.0,text="Temp-Ozone",xref="paper",yref="paper",xanchor="center",yanchor="bottom",showarrow=FALSE),
  list(x=0.9,y=1.0,text="Temp-Wind",xref="paper",yref="paper",xanchor="center",yanchor="bottom",showarrow=FALSE))

p<-p %>%layout(annotations = annotations) 

p
```

```
## Warning: 'bar' objects don't have these attributes: 'col'
## Valid attributes include:
## '_deprecated', 'alignmentgroup', 'base', 'basesrc', 'cliponaxis', 'constraintext', 'customdata', 'customdatasrc', 'dx', 'dy', 'error_x', 'error_y', 'hoverinfo', 'hoverinfosrc', 'hoverlabel', 'hovertemplate', 'hovertemplatesrc', 'hovertext', 'hovertextsrc', 'ids', 'idssrc', 'insidetextanchor', 'insidetextfont', 'legendgroup', 'legendgrouptitle', 'legendrank', 'marker', 'meta', 'metasrc', 'name', 'offset', 'offsetgroup', 'offsetsrc', 'opacity', 'orientation', 'outsidetextfont', 'selected', 'selectedpoints', 'showlegend', 'stream', 'text', 'textangle', 'textfont', 'textposition', 'textpositionsrc', 'textsrc', 'texttemplate', 'texttemplatesrc', 'transforms', 'type', 'uid', 'uirevision', 'unselected', 'visible', 'width', 'widthsrc', 'x', 'x0', 'xaxis', 'xcalendar', 'xhoverformat', 'xperiod', 'xperiod0', 'xperiodalignment', 'xsrc', 'y', 'y0', 'yaxis', 'ycalendar', 'yhoverformat', 'yperiod', 'yperiod0', 'yperiodalignment', 'ysrc', 'key', 'set', 'frame', 'transforms', '_isNestedKey', '_isSimpleKey', '_isGraticule', '_bbox'
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

4. Some histrograms

```r
hist(a$Wind, main="Daily Wind", col="lightyellow", xlim=c(0,25))
```

![plot of chunk unnamed-chunk-4](assets/fig/unnamed-chunk-4-1.png)

```r
hist(a$Temp, main="Daily Wind", col="lightgreen", xlim=c(50,100))
```

![plot of chunk unnamed-chunk-4](assets/fig/unnamed-chunk-4-2.png)























