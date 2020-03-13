---
title: "Covid R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
```{r init}
Co <- read.csv('time_series_19-covid 20200312.csv')
#names(Co)
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
library(reshape2)
#if(!require(DDply)){
#  install.packages("DDply")
#  require(DDply)
#}
```

Now prepare somefunctions to determine the lags
```{r} 
min=2
lagcount <- function(column=c(0,0,1,2,3),minval=min){
  return (sum(column < minval))
  }
delaggedcol <- function(kol,minval=min){
 return(kol [ kol >= minval ] )
}
lags<- function(wpdf) {
  as.data.frame(t(apply(wpdf[2:ncol(wpdf)],2,lagcount,...)))
}
names(Co[grep("France",names(Co))])
countries<- c("Thailand","Japan","Belgium","Netherlands","Germany","China_Hunan")
synclags<- function(wpdf=Co, minval=min,cols=countries, 
      rowindexname="day",variables="country"){
  wpdf = wpdf[cols]
  daywpdf<-as.data.frame(1:nrow(wpdf))
  colnames(daywpdf) <-rowindexname
  for (i in 1:ncol(wpdf)){
    daywpdf[[names(wpdf)[i]]] <-  
        c(delaggedcol(wpdf[i],minval),rep(NA,lagcount(wpdf[i],minval)))
  }
  return(melt(daywpdf,id=rowindexname, variable=variables))
}  

```
Now calculate the lagged version and plot
```{r}
names(Co[grep("Jiangsu",names(Co))])
mdayCo <- synclags(Co,10)
countries<- c("Japan","China_Jiangsu","China_Hunan","Belgium","Netherlands","Egypt","Spain","Germany","France_France","Italy", "Iran")
mdayCo <- synclags(Co,50,countries)
ggplot(mdayCo,aes(x=day,y=value,colour=country,group=country)) + geom_line()+ylab("confirmed")

lags(Co[c("Date",countries)])
head(dayCo[countries])


#geom_point
 ylab('Nr infections') +
  #Change x-axis label
xlab('Date')
+
  #Change legend format
labs(color= 'Above or\nBelow Mean') +
  #Add a title
  ggtitle("2012 NATO GDP per Capita Valuations")
}



```

