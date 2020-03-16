---
title: "Covid R Notebook"
output: 
  html_notebook: 
    toc: yes
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 
When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).
The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
```{r}

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
require(plyr)
require(reshape2)


```
**optional more packages, not used yet**
```{r } 
#require(git2r)
#if(!require(data.table)){
#  install.packages("data.table")
#  require(data.table)
#}

```
*Note*: the data of John hopkins, git@github.com:CSSEGISandData/COVID-19.git
(here downloaded to the relative path 'COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Confirmed.csv')
is in csv format, and the 4 first columns can be seen as an id of the data. However, There is no need in line graphs for the location data so we separate those. It can be  done in Excel, by deleting the latitude and longitude, and by concatenating province and country levels into one. But this is cumbersome as everyday the data is updated. The Excel conversion used to be saved it in this script's folder as and could be read with


First, lets read the original data: 
```{r} 
#fetch(repo = ".", name = NULL, credentials = NULL, verbose = TRUE,
  refspec = NULL)
fetch(".\\COVID-19","upstream") #should update the data from Git. 
#error authenticating. even after deleting id-rsa pasword: error authenticating: failed connecting agent

readdata <-function(dataversion="Confirmed",coltype="date", values.name="nr"){ #"Deaths", "Recovered"
  filename<- paste('COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-',dataversion,".csv",sep="") 
  tryCatch( wpdf <-read.csv(filename) ,
    error= function(e) print(paste(e," The data was not found: Are you sure this file exists? ",filename))
    )
  wpdf$CRPS <- as.factor(ifelse(""==wpdf$Province.State, 
                    paste(wpdf$Country.Region,"",sep=""),
                    paste(wpdf$Country.Region,wpdf$Province.State,sep=', ')))
  geo.location <- wpdf[c("Country.Region","Province.State","CRPS","Lat","Long")]
  wpdf$Lat<- NULL
  wpdf$Long<- NULL
  lpdf<-reshape2::melt(wpdf,id=c("CRPS","Province.State","Country.Region"),
                    variable.name=coltype, value.name=values.name) 
  lpdf$Date<- as.Date(paste(lpdf[,coltype],"20",sep=""),format="X%m.%d.%Y") 
  return(lpdf)
}#note if data.table package is added, it has its own "melt" function

confirmed<- readdata("Confirmed",values.name="confirmed")
confirmed$date<- NULL
deaths<- readdata("Deaths",values.name="deaths")
deaths$date<- NULL
recovered<- readdata("Recovered",values.name="recovered")
recovered$date<- NULL
alldata<- merge(confirmed,recovered,all=TRUE,by=c("CRPS","Country.Region","Province.State","Date"))
alldata<- merge(alldata,deaths,all=TRUE,by=c("CRPS","Country.Region","Province.State","Date"))
#alldata$date<-as.Date(alldata$date,format="X%m.%d.%Y")  
#alldata<-alldata[with(alldata, order(CRPS, date)), ]  #get the dates sorted out, later we use the rownames!
alldata[11:22,]
rownames(alldata)<- NULL
alldata[11:22,]
rm(confirmed,deaths,recovered)
sum(is.na(alldata)) #on 20200315: returns 0, i.e. no NA's created or read in!
```

Next, prepare functions to select data we want to line graph, determined by the minimum value and CRPS

```{r}
minv=1
testcountries<- c("Be","Vietnam","Thailand","ind","Japan","France","Ger", "Nethe", "Hunan")

#make sure we have the right column names

multigrep<- function( smalllist,biglist,ignorecase=FALSE){
  unlist(llply(smalllist,function(a) grep(a,biglist, value=TRUE,ignore.case=ignorecase)))
  }

#this one finds exact names when needed
findIDnames <- function(testIDnames=c("Neth","India"), idcol="CRPS",
                      lpdf=alldata, fuzzy=TRUE){ 
    allIDs<- drop(unique(lpdf[,idcol]))
    if (!fuzzy) {return(intersect(testIDnames,allIDs))}
    allIDs[unlist(llply(testIDnames,function(a) grep(a,allIDs, ignore.case=TRUE)))]
} 
datasel<- function(countries=testcountries, minval= minv, lpdf=alldata, var="confirmed",
                  id="CRPS", fuzzy=FALSE){
  if(fuzzy) countries <- findIDnames(countries,"CRPS",lpdf)
  return( lpdf[ (lpdf[var]>=minval)&(lpdf[,id] %in% countries) , ]) # with id=CRPS and var=confirmed it should work.
}
addrownrs<-function(ts,sortby="") {
  #if !(sortby=="") ts[order(ts[,sortby])]
  ts$counter<-as.numeric(row.names(ts))
  return(ts)
}# two problems: if rownames change to chr, errors occur. if they get out of order because of a sort, the graphs will be a mess. 
addcounter<-function(lpdf=alldata,id="CRPS",counter="day"){
    lpdf$counter <- 0 #just to add a column, with a value that will be filled next. otherwise we get too many columns. 
    lpdf<- ddply(lpdf,id, addrownrs)
    #names(lpdf)["counter"]<- counter
    return(lpdf)
}

```
*Now plot*
```{r}
graphit <- function(countries=unique(alldata$CRPS), minval=1, ID="CRPS", varname="confirmed",
                    lpdf=alldata, countname="counter", needfuzzy=TRUE, loga=TRUE,saveit=FALSE){
  lpdf<- addcounter(
        datasel(countries,minval,var=varname,id=ID, lpdf=lpdf, fuzzy=needfuzzy),
        ID,countname)
  lin<- ggplot(lpdf,aes_string(x=countname,y=varname,color=ID,group=ID)) +  
    geom_line()+geom_point(size=0.7,shape=1)+ylab(paste(varname, ifelse(loga," (log scale)","")))+xlab(paste("days after the first",minval,varname))+
  geom_dl(aes_string(label = ID) , method = list(dl.trans(x = x + 0.2),
          "last.points", cex = 0.8))+  
  scale_color_discrete(guide = FALSE) #FALSE , "colorbar" or "legend"
  #geom_text(data = ldaydf, aes_string(label = ID, colour = ID, x =Inf, y =max(value) ), hjust = -10) 
  ifelse(loga,return(lin+scale_y_continuous(trans='log2')),return(lin))
  if (saveit) save.plot(paste("plots/",varname,format(Sys.Date(),format="%Y%m%d")," in ( ", 
                        paste(findIDnames(countries,ID,lpdf,needfuzzy),collapse=", " )," )"),
                        type= "png")
}
paste("confirmed",format(Sys.Date(),format="%Y%m%d"), 
        paste(findIDnames(countries,ID,lpdf,needfuzzy),collapse=", " ),sep="_")

#### test
alldata$recoveredOverDead <- alldata$recovered/alldata$death
##########################################################
#Use it:
graphit(minval=4000,loga=FALSE)
WestvsEast<- c("Italy","Iran","Korea","Germany","France, France","Spain","Norway","China, Jiangsu","China,_Hunan","Belgium","Netherlands", "Romania","Singapore","Japan","Austria","China, Shanghai")
graphit(WestvsEast,10)
graphit(WestvsEast,50)
graphit(WestvsEast,500,saveit=TRUE)#,loga=FALSE)
graphit(WestvsEast,loga=FALSE)

EU<- c("Italy","Germany","France, France","Spain","Poland","Belgium","Netherlands","Austria","Romani","Hunga","Ireland","Sweden","Denmark","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Luxembourg","Cyprus","United K","Swit","Norway")
findIDnames(EU)
graphit(EU,50,loga=FALSE)
graphit(EU,50,varname="confirmed")
graphit(EU,1,varname="deaths")
graphit(EU,1,varname="recovered")
graphit(EU,0,varname="recoveredOverDead",loga=FALSE)
graphit(EU,50,loga=FALSE)

WCAsia<-c("Rus", "Georgia", "Armen", "Azerb", "Ukrai","stan","desh","india","Irak","Syria","Lebanon","Turk","Israel","Pal","Bhu","Palest")

findIDnames(WCAsia)
graphit(WCAsia,10)
graphit(countries,10)
graphit(c("..CA"),20)
graphit(c("Canada"),20)
graphit(c("..NY"),10)
graphit(c("US"),1)
SAsiaIO<-c("India","Pakistan","Bangladesh","Sri","Comoros","Maldives","Madagascar","Mauritius","Seychelles")
findIDnames(SAsiaIO)
graphit(SAsiaIO,8)
MENA<-c("Egypt", "Marocco","Alger","Tunes","Lib","Syr","Turk","Saudi","Kuwait","Oman","arab","UAE","Yemen","Bahrain","Qatar","Irak","Iran")
graphit(MENA,50)
Africa<- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon","Central African Republic (CAR)","Chad","Comoros", "Congo, Democratic Republic of the", "Congo, Republic of the", "Cote d'Ivoire", "Djibouti", "Egypt", 
"Equatorial Guinea", "Eritrea", "Eswatini (formerly Swaziland)", "Ethiopia", 
"Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", 
"Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
"Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
graphit(Africa,3)
graphit("Australia",10)
SAmerica<-c("Chile","Brazil","Argenti","Peru","Colombia","Venezuela","Mexico","Honduras","Salvador","Panama","Ecuador","Surinam","Guyan","Beliz","Guatemals", "Antill")
graphit(SAmerica,10)
graphit(minval=500)
graphit(minval=10, varname="deaths")
graphit(minval=100, varname="recovered")
```
We need to test the functions before running the whole thing 
```{r testing}
###########################testcases

```