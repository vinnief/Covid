#Covid after EXcel manipulations
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


#**optional more packages, not used yet**
require(git2r)
if(!require(data.table)){
  install.packages("data.table")
  require(data.table)
}

#```
#*Note*: the data of John hopkins, git@github.com:CSSEGISandData/COVID-19.git
#(here downloaded to the relative path 'COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Confirmed.csv')
#is in csv format, and the 4 first columns can be seen as an id of the data. However, There is no need in line graphs for the location data so we separate those. It can be  done in Excel, by deleting the latitude and longitude, and by concatenating province and country levels into one. But this is cumbersome as everyday the data is updated. The Excel conversion used to be saved it in this script's folder as and could be read with

#First, lets read the Excel manupulated data: 
#fetch(repo = ".", name = NULL, credentials = NULL, verbose = TRUE, refspec = NULL)
# do a fetch(".\\COVID-19","upstream") #should update the data from Git. and prepare the data in Excel 
#error authenticating. even after deleting id-rsa pasword: error authenticating: failed connecting agent
Co <- read.csv('time_series_19-covid-confirmed.csv') #change the date to the latest version
#deaths<- read.csv("time_series_19-covid-deaths.csv")
#recovered<- read.csv("time_series_19-covid-recovered.csv")
#alldata<- read.csv("time_series_19-covid-alldata.csv")
#the format changed since 20020313. somehow excel mangled the dates: now we see 
#no Xm.d.yy but m/d/y, somehow reinterpreted to yyyy/m/d in some places. (beginning of the months, i.e. day below 12)

#Co$Date<-replace(Co$date,"2020","20")# does not work. 
Co$Date<-as.Date(Co$date,format="%m/%d/%Y")  

#alldata$date<-as.Date(alldata$date,format="X%m.%d.%Y")  
#alldata<-alldata[with(alldata, order(CRPS, date)), ]  #get the dates sorted out, later we use the rownames!
#rm(confirmed,deaths,recovered)
#head(alldata[order(alldata[,c("CRPS" ,"date")]),],30)
sum(is.na(Co)) #on 20200314: returns 0, i.e. no NA's created or read in!


#Next, prepare functions to select data we want to line graph, determined by the minimum value and CRPS

#```{r}
minv=1
testcountries<- c("Be","Vietnam","Thailand","ind","Japan","France","Ger", "Nethe", "Hunan")
lagcount <- function(kol=c(0,0,1,2,3),minval=minv){
  return (sum(kol < minval))
}
delaggedcol <- function(kol,minval=minv){
  return(  c(kol[ kol >= minval ],rep(NA,lagcount(kol,minval))  ))
}
lags<- function(wpdf,minval=minv) {
  as.data.frame(t(apply(wpdf,2,lagcount,minval)))
}


#```
#*tests*:
#  ```{r testing}
testcountries
#how long is the dataset actually? 
abovemin.len<- function(wpdf,minval=minv){
       nrow(wpdf) - min(lags(wpdf,minval))
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
#calculate the lagged dataset, from the first day each country has more than minval cases
 synclags<- function(wpdf=Co, minval=minv,cols=countries, 
                 rowindexname="day",variables="country"){
       wpdf = wpdf[,cols,drop = FALSE]
       lagscounts<- lags(wpdf,minval)
       len<- nrow(wpdf) - min(lagscounts)
       #print("removing columns:")
       #print(names((wpdf[,lagscounts==nrow(wpdf)])))
       wpdf<- wpdf[,lags(wpdf)<nrow(wpdf),drop = FALSE]
       daywpdf<-as.data.frame(1:nrow(wpdf))
       colnames(daywpdf) <-rowindexname
       for (i in 1:ncol(wpdf)){
           daywpdf[[names(wpdf)[i]]] <- delaggedcol(wpdf[i],minval)
        }
       return( melt(daywpdf[1:len,],id=rowindexname, variable=variables) )
} 
           
#           ```
#           Now plot
# ```{r}
graphthem<- function(tentcountries,minval=minv,wpdf=Co,loga=TRUE){
             countries <- findcolnames(tentcountries,wpdf)
             ldaydf <- synclags(wpdf,minval,countries)
             lin<- ggplot(ldaydf,aes(x=day,y=value,colour=country,group=country)) + geom_line()+ylab(paste("confirmed", ifelse(loga," (log scale)","")))+xlab(paste("days after the first ",minval," cases"))+
               geom_dl(aes(label = country) , method = list(dl.trans(x = x + 0.2),
                                                            "last.points", cex = 0.8))+  
               scale_color_discrete(guide = FALSE)
             #geom_text(data = ldaydf, aes(label = country, colour = country, x =Inf, y =max(value) ), hjust = -10) 
             ifelse(loga,return(lin+scale_y_continuous(trans='log2')),return(lin))
}

##########################################################
#do it now!

WestvsEast<- c("Italy","Iran","Korea","Germany","France, France","Spain","Norway","China, Jiangsu","China,_Hunan","Belgium","Netherlands", "Romania","Singapore","Japan","Austria","China, Shanghai")
head(synclags(Co, 300,findcolnames(WestvsEast)))
EU<- c("Italy","Germany","France, France","Spain","Poland","Belgium","Netherlands","Austria","Romani","Hunga","Ireland","Sweden","Denmark","Norway","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czekhia","Estonia","Lithuania","Latvia","Malta","Luxembourg","Cyprus","United Kingdon, UK")

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

#```
#We need to test the functions before running the whole thing 
#```{r testing}
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

##```