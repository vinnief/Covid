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
# note ggrepel also does similar labels. 

library(reshape2)
#if(!require(DDply)){
#  install.packages("DDply")
#  require(DDply)
#}
```
Note: the data of John hopkins, git@github.com:CSSEGISandData/COVID-19.git
(here downloaded to the relative path 'COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Confirmed.csv')
is in csv format, and i thought it needed transposing before it can be used, because i want to get rid of the values below a minimum. 
Then the 4 first columns need to be converted into a possible column label. I do that in Excel, by deleting the latitude and longitude, and by concatenating province and country levels into one. THen save it in this folder as Co <- read.csv('time_series_19-covid Confirmed yyyymmdd.csv'). 
Now prepare somefunctions to determine the lags
```{r} 
#fetch(repo = ".", name = NULL, credentials = NULL, verbose = TRUE,
  refspec = NULL)
fetch(".\\COVID-19","upstream") 
#error authenticating. even after deleting id-rsa pasword,
: error authenticating: failed connecting agent
Co <- read.csv('time_series_19-covid-confirmed-20200313.csv')
#Co0 <-'COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Confirmed.csv')
#co0$row.names<-
#apply(co0[c("Country.Region","Province.State")],2,function(x,y){paste(x,y,sep="_")})
#co2<-transpose(co1[3:ncol(co0)])

minv=1
lagcount <- function(kol=c(0,0,1,2,3),minval=minv){
  return (sum(kol < minval))
  }
delaggedcol <- function(kol,minval=minv){
 return(  c(kol[ kol >= minval ],rep(NA,lagcount(kol,minval))  ))
}

lags<- function(wpdf,minval=minv) {
  as.data.frame(t(apply(wpdf,2,lagcount,minval)))
}
#how long is the dataset actually? 
abovemin.len<- function(wpdf,minval=minv){
 nrow(wpdf) - min(lags(wpdf,minval))
}

countries<- c("Vietnam","Thailand","Indonesia","Japan")
#calculate the lagged dataset, from the first day each country has more than minval cases
synclags<- function(wpdf=Co, minval=minv,cols=countries, 
      rowindexname="day",variables="colname"){
  wpdf = wpdf[,cols,drop = FALSE]
  lagscounts<- lags(wpdf,minval)
  len<- nrow(wpdf) - min(lagscounts)
  print("removing columns:")
  print(names((wpdf[,lagscounts==nrow(wpdf)])))
  wpdf<- wpdf[,lags(wpdf)<nrow(wpdf),drop = FALSE]
  daywpdf<-as.data.frame(1:nrow(wpdf))
  colnames(daywpdf) <-rowindexname
  for (i in 1:ncol(wpdf)){
    daywpdf[[names(wpdf)[i]]] <-  
        delaggedcol(wpdf[i],minval)
  }
  return( melt(daywpdf[1:len,],id=rowindexname, variable=variables) )
} 
#make sure we have the right column names
findcolnames <- function(tentcolnames=c("LL"),df=Co) {
    colnames <- tentcolnames[1]
    for (coun in tentcolnames[1:length(tentcolnames)]){
      #print (paste( coun, names(df[grep(coun,names(df))]),sep =" -> "))
      colnames<-c(colnames, names(df[grep(tolower(coun),tolower(names(df)))]))
    }
    colnames[2:length(colnames)]
  }

```
Now plot
```{r}
graphthem<- function(tentcountries,minval=minv,wpdf=Co,loga=TRUE){
  colnames <- findcolnames(tentcountries,wpdf)
  #xes<- nrow(wpdf)-lags(wpdf)
  #xes<- xes[xes>0]
  #print(t(colnames))
  ldaydf <- synclags(wpdf,minval,colnames)
  lin<- ggplot(ldaydf,aes(x=day,y=value,colour=colname,group=colname)) + geom_line()+ylab(paste("confirmed", ifelse(loga," (log scale)","")))+xlab(paste("days after the first ",minval," cases"))+
  geom_dl(aes(label = colname) , method = list(dl.trans(x = x + 0.2),
          "last.points", cex = 0.8))+  
  scale_color_discrete(guide = FALSE)
  #geom_text(data = ldaydf, aes(label = colname, colour = colname, x =Inf, y =max(value) ), hjust = -10) 
  ifelse(loga,return(lin+scale_y_continuous(trans='log2')),return(lin))
}
WestvsEast<- c("Italy","Iran","Korea","Germany","FranceF","Spain","Norway","Jiangsu","Hunan","Belgium","Netherlands", "Romania","Singapore","Japan","Austria","Shanghai")

EU<- c("Italy","Germany","FranceF","Spain","Poland","Belgium","Netherlands","Austria","Romani","Hunga","Ireland","Sweden","Denma","Norway","Finland","Bulga","Portugal","Greece","Croat","Slov","Cze","Esto","Lithua","Latv","Malta","Luxem","Cyprus","mUK")

WCAsia<-c("Rus", "Georgia", "Armen", "Azerb", "Ukrai","stan","desh","india","Irak","Syria","Lebanon","Turk","Israel","Pal","Bhu","Terr")
findcolnames(WCAsia)
graphthem(WCAsia,1)
findcolnames(EU)
graphthem(EU,50)
graphthem(EU,50,loga=FALSE)
graphthem(countries,10)
graphthem(WestvsEast,10)
graphthem(WestvsEast,50)
graphthem(WestvsEast,50,loga=FALSE)
graphthem(c("..CA"),20)
graphthem(c("Canada"),20)
graphthem(c("..NY"),10)
graphthem(c("US"),1)
SAsiaIO<-c("India","Pakistan","Bangladesh","Sri","Comoros","Maldives","Madagascar","Mauritius","Seychelles")
findcolnames(SAsiaIO)
graphthem(SAsiaIO,8)
MENA<-c("Egypt", "Marocco","Alger","Tunes","Lib","Syr","Turk","Saudi","Kuwait","Oman","arab","UAE","Yemen","Bahrain","Qatar","Irak","Iran")
graphthem(MENA,50)
Africa<- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon","Central African Republic (CAR)","Chad","Comoros", "Congo, Democratic Republic of the", "Congo, Republic of the", "Cote d'Ivoire", "Djibouti", "Egypt", 
"Equatorial Guinea", "Eritrea", "Eswatini (formerly Swaziland)", "Ethiopia", 
"Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", 
"Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
"Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
graphthem(Africa,3)
graphthem("Australia",10)
SAmerica<-c("Chile","Brazil","Argenti","Peru","Colombia","Venezuela","Mexico","Honduras","Salvador","Panama","Ecuador","Surinam","Guyan","Beliz","Guatemals", "Antill")
graphthem(SAmerica,10)
#save.plot(logtoday()) #syntax? 

```
We need to test the functions before running the whole thing 
```{r testing}
########################
###testcases
names(Co[grep("South",names(Co))])

a<- Co[findcolnames(EU)]
View(a)
min(lags(a))
View(a[min(lags(a)):nrow(a),])
nonmis.len(Co[findcolnames(c("..CA"))])
nonmis.len(Co[findcolnames(c("..NY"))])
nonmis.len(Co[findcolnames(EUEast,Co)])
mdayCo <- synclags(Co,10)

```