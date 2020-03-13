---
title: "Covid R Notebook"
output: 
  html_notebook: 
    toc: yes
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
```{r init}
require(git2r)

#names(Co)
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(directlabels)){
  install.packages("directlabels")
  require(directlabels)
}

library(reshape2)
#if(!require(DDply)){
#  install.packages("DDply")
#  require(DDply)
#}
```

Now prepare somefunctions to determine the lags
```{r} 
fetch('./COVID-19','upstream')
Co <- read.csv('time_series_19-covid 20200313.csv')
#Co0 <-'COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Confirmed.csv')
#co0$row.names<-
#apply(co0[c("Country.Region","Province.State")],2,function(x,y){paste(x,y,sep="_")})
#co2<-transpose(co1[3:ncol(co0)])

minv=30
lagcount <- function(kol=c(0,0,1,2,3),minval=minv){
  return (sum(kol < minval))
  }
delaggedcol <- function(kol,minval=minv){
 return(  c(kol[ kol >= minval ],rep(NA,lagcount(kol,minval))  ))
}

lags<- function(wpdf) {
  as.data.frame(t(apply(wpdf,2,lagcount)))
}
nonmis.len<- function(wpdf){
 nrow(wpdf) - min(lags(wpdf))
}
countries<- c("Vietnam","Thailand","Indonesia","Japan")
synclags<- function(wpdf=Co, minval=minv,cols=countries, 
      rowindexname="day",variables="colname"){
  wpdf = wpdf[,cols,drop = FALSE]
  lagscounts<- lags(wpdf)
  len<- nrow(wpdf) - min(lagscounts)
  print("removing columns:")
  print(names((wpdf[,lagscounts==nrow(wpdf)])))
  wpdf<- wpdf[1:len,lags(wpdf)<nrow(wpdf),drop = FALSE]
  daywpdf<-as.data.frame(1:nrow(wpdf))
  colnames(daywpdf) <-rowindexname
  for (i in 1:ncol(wpdf)){
    daywpdf[[names(wpdf)[i]]] <-  
        delaggedcol(wpdf[i],minval)
  }
  return( melt(daywpdf,id=rowindexname, variable=variables) )
}  

```
Now calculate the lagged version and plot
```{r}

findcolnames <- function(tentcolnames=c("LL"),df=Co) {
    colnames <- tentcolnames[1]
    for (coun in tentcolnames[1:length(tentcolnames)]){
      #print (paste( coun, names(df[grep(coun,names(df))]),sep =" -> "))
      colnames<-c(colnames, names(df[grep(coun,names(df))]))
    }
    colnames[2:length(colnames)]
  }


graphthem<- function(tentcountries,minv1=minv,wpdf=Co){
  colnames <- findcolnames(tentcountries,wpdf)
  #xes<- nrow(wpdf)-lags(wpdf)
  #xes<- xes[xes>0]
  #print(t(colnames))
  ldaydf <- synclags(wpdf,minv1,colnames)
  lin<- ggplot(ldaydf,aes(x=day,y=value,colour=colname,group=colname)) + geom_line()+ylab("confirmed")
  lin
  lin+ scale_y_continuous(trans='log2')+
  #geom_text(data = ldaydf, aes(label = colname, colour = colname, x =Inf, y =max(value) ), hjust = -10) 
  geom_dl(aes(label = colname) , method = list(dl.trans(x = x + 0.2),"last.points", cex = 0.8))
}
EUEast<- c("Italy","Iran","Korea","Germany","FranceF","Spain","Norway","Jiangsu","Hunan","Belgium","Netherlands","Egypt", "Romania","Singapore","Japan","Austria")


graphthem(countries)
graphthem(EUEast,50)
graphthem(c("..CA"),20)
graphthem(c("..NY"),10)
graphthem(c("US"),10)

save.plot(logtoday())

```
We need to test the functions before running the whole thing 
```{r testing}
########################
###testcases
names(Co[grep("South",names(Co))])

nonmis.len( Co[findcolnames(c("US"))])
min(lags(a))
nonmis.len(Co[findcolnames(c("..CA"))])
nonmis.len(Co[findcolnames(c("..NY"))])
nonmis.len(Co[findcolnames(EUEast,Co)])
mdayCo <- synclags(Co,10)

```