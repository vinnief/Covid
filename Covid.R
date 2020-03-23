#---
#Title: "Covid19 R Notebook"
#output: 
#  html_notebook: Covid19.html
#    toc: yes
#---

#This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within #the notebook, the results appear beneath the code. Note this whole markdown notebook thingy #does not work in my setup, so y am just commenting out all text to rus the whole file at #once!

#When you save the notebook, an HTML file containing the code and output will be saved #alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).
#The preview shows you a rendered HTML copy of the contents of the editor. Consequently, #unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk #when it was last run in the editor is displayed.
#```{r}

#names(Co)
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(directlabels)){
  install.packages("directlabels")
  require(directlabels)
}
#getpackage<- function(pname){
#  if(!require(eval(name))){
#    install.packages(pname)
##    require(eval(pname))
#  }}
#getpackage(RColorBrewer)

if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(ggthemes)) {  installed.packages("ggthemes"); require(ggthemes)}

if(!require(ggrepel)){
  install.packages("ggrepel")
  require(ggrepel)
}# note ggrepel also does labels next to lines. 

if(!require(plm)){
  install.packages("plm")
  require(plm)
}
require(plyr)
require(reshape2)


#if(!require(replaceme)){
#  install.packages("replaceme")
#  require(replaceme)
#}
#if(!require()){
#  install.packages("")
#  require()
#}

#```
#**optional more packages, not used yet**
#```{r } 

#if(!require(data.table)){
#  install.packages("data.table")
#  require(data.table)
#}

#```
#*Note*: the data of John hopkins, git@github.com:CSSEGISandData/COVID-19.git
#(here downloaded to the relative path #'COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Confirmed.csv#')
#is in csv format, and the 4 first columns can be seen as an id of the data. However, There #is no need in line graphs for the location data so we separate those. It can be  done in #Excel, by deleting the latitude and longitude, and by concatenating province and country #levels into one. But this is cumbersome as everyday the data is updated. The Excel #conversion used to be saved it in this script's folder as and could be read with


#First, lets read the original data with something like from git2R
#fetch(repo = ".", name = NULL, credentials = NULL, verbose = TRUE, refspec = NULL)
#if(!require(git2r)){  install.packages("git2r");  require(git2r)}
#fetch(".\\COVID-19","upstream") #should update the data from Git. 
#error authenticating. even after deleting id-rsa pasword: error authenticating: failed connecting agent
readdata<-function(dataversion="Confirmed"){
  filename<- paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-',
                    dataversion,".csv",sep="")
  #local path if you download first: COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-'
  tryCatch( wpdf <-read.csv(filename) ,
            error= function(e) print(paste(e," The data was not found: Are you sure this file exists? ",filename))
  )
  return(wpdf)
}
#c<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
US<- read.csv("states.csv",stringsAsFactors=FALSE)
US$Abbrev<-NULL
correctgeos <-function(wpdf){ #"Deaths", "Recovered"
  wpdf$County <- substr(wpdf$Province.State , 1,regexpr(",",wpdf$Province.State)-1)
  wpdf$PS<-( sub("[A-Z,a-z' ]+, ", "",wpdf$Province.State)) #as factor or ordered
  ps<-unique( wpdf[!grepl(",",wpdf$Province.State,fixed=TRUE &wpdf$Province.State!=""),"Province.State"])
  #ps[1]==""
  ps<- rbind(US,data.frame(State=ps,Code=ps))
  merge(wpdf,ps, by.x="PS",by.y="Code", all.x=TRUE)
}

convertdata <-function(wpdf,coltype="date", values.name="count"){ #"Deaths", "Recovered"
  wpdf$CRPS <- ordered(ifelse(""==wpdf$State, 
                              paste(wpdf$Country.Region,"",sep=""),
                              paste(wpdf$State,wpdf$Country.Region,sep=', ')))
  wpdf$Province.State<-NULL
  wpdf$PS<-NULL
  wpdf$Province.State<- NULL
  lpdf<-reshape2::melt(wpdf,id=c("Country.Region","CRPS","State","County","Lat","Long"),
                       variable.name=coltype, value.name=values.name)    #measure.vars=6:... end -2. changes every day so dont fill hard number. 
  lpdf$Date<- as.Date(paste(lpdf[,coltype],"20",sep=""),format="X%m.%d.%Y") 
  lpdf$date<- NULL
  return(lpdf)
} #note if data.table package is added, it has its own "melt" function

mklpdf <- function() {
  wc<-readdata("Confirmed")
  geo.location <- wc[c("Country.Region","Province.State","Lat","Long")]
  write.csv(geo.location,file="geo.location",na="")
  wc<-correctgeos(wc)
  confirmed<- convertdata(wc,values.name="confirmed")
  wd<-readdata("Deaths")
  wd<-correctgeos(wd)
  deaths<- convertdata(wd,values.name="deaths")
  wr<-readdata("Recovered")
  wr<-correctgeos(wr)
  recovered<- convertdata(wr,values.name="recovered")
  alldata<- merge(confirmed,recovered,all=TRUE,by=c("CRPS","Country.Region","State","County","Date","Lat","Long"))
  alldata<- merge(alldata,deaths,all=TRUE,by=c("CRPS","Country.Region","State","County","Date","Lat","Long"))
  
  alldata$recoveredOverDeaths <-     ifelse(alldata$deaths>0,     alldata$recovered/alldata$deaths,     NA)#lag the deaths by 10 days as recovery takes longer?
  alldata$recoveredOverConfirmed<- ifelse(alldata$confirmed>0, alldata$recovered/alldata$confirmed, NA)# lag the confirmed by 30 days as recovery takes a while.... 
  alldata<-alldata[with(alldata, order(CRPS, Date)), ]  
}

#alldata<- mklpdf()

######## make state groups, also useful in tableau
  EU<- c("Italy","Germany","France","Spain","Poland","Belgium","Netherlands","Austria","Romania","Hungary","Ireland","Sweden","Denmark","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Luxembourg","Cyprus")
  EFTA<-c("Iceland","United K","Swit","Norway")
  Europe<- c(EU,EFTA,c("Serbia","Bosnia", "Russia", "Ukraine", "Belarus","Moldova","Georgia", "Armenia", "Azerbaijan", "Monaco", "San Marino", "Vatican", "Albania", "North Macedonia"))
  CIS<- c("Russia", "Belarus","Georgia", "Armenia", "Azerbaijan", "Ukraine","Kazakhstan","Kirgizstan","Turkmenistan", "Tadjikistan")
  CAsia<-c("Pakistan","Afganistan","Iran","Bangladesh","India","Irak","Syria","Lebanon","Turkey","Israel","Bhutan","Palestine","Nepal")
  SouthEastAsia<- c("Indonesia","Thailand","Vietnam","Laos","Malaysia","Cambodia","Taiwan", "Hong Kong","Singapore")
  SAsiaIO<-c("India","Pakistan","Bangladesh","Sri","Comoros","Maldives","Madagascar","Mauritius","Seychelles")
  
  MENA<-c("Egypt", "Marocco","Algeria","Tunesia","Libia","Syria","Turkey","Saudi Arabia","Kuwait","Oman","United Arab Emirates","UAE","Yemen","Bahrain","Qatar","Irak","Iran","Afghanistan")
  SAmerica<-c("Chile","Brazil","Argenti","Peru","Colombia","Venezuela","Mexico","Honduras","Salvador","Panama","Ecuador","Surinam","Guyan","Beliz","Guatemals", "Antilles")
  
  Africa<- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon","Central African Republic (CAR)","Chad","Comoros", "Congo, Democratic Republic of the", "Congo, Republic of the", "Cote d'Ivoire", "Djibouti", "Egypt", 
             "Equatorial Guinea", "Eritrea", "Eswatini (formerly Swaziland)", "Ethiopia", 
             "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", 
             "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
             "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
makeGroups <- function(alldata=alldata,varname="Region") {  
  alldata[,varname]<-""
  alldata[alldata$Country.Region %in% EFTA, varname] <-"EFTA"
  alldata[alldata$Country.Region %in% Europe, varname] <-"Europe"
  alldata[alldata$Country.Region %in% EU, varname] <-"EU27"
  alldata[alldata$Country.Region %in% MENA, varname] <-"MENA"
  alldata[alldata$Country.Region %in% Africa, varname] <-"Africa"
  alldata[alldata$Country.Region %in% SAmerica, varname] <-"South America"
  alldata[alldata$Country.Region %in% WCAsia, varname] <-"WC-Asia"
  alldata[alldata$Country.Region %in% CIS, varname] <-"CIS"
  alldata[alldata$Country.Region %in% c("US","Canada","Mexico"), varname] <-"North America"
  alldata[alldata$Country.Region %in% SAsiaIO, varname] <-"South Asia & Indian Ocean"
  write.csv(alldata,"Covid19.csv",na="")
  alldata
}

#alldata<- makeGroups( mklpdf(),"Region")


#```
#Next, prepare functions to select data we want to line graph, determined by the minimum value and CRPS
#```{r}
testcountries<- c("Belg","Vietnam","Ind","Korea","Japan","France","Germ","Italy",  "Nethe", "Hunan","Taiwan")

#make sure we have the right column names. This version not used. 
multigrep<- function( smalllist,biglist,ignorecase=FALSE){
  unlist(llply(smalllist,function(a) grep(a,biglist, value=TRUE,ignore.case=ignorecase)))
  }
multigrep(testcountries,unique(alldata$Country.Region),ignorecase=TRUE)
#this one finds exact names when needed
findIDnames <- function(testIDnames=c("Neth","India"), idcol="CRPS",
                      lpdf=alldata, fuzzy=TRUE){ 
    allIDs<- drop(unique(lpdf[,idcol]))
    if (!fuzzy) {return(intersect(testIDnames,allIDs))}
    allIDs[unlist(llply(testIDnames,function(a) grep(a,allIDs, ignore.case=TRUE)))]
} 
datasel<- function(countries=testcountries, minval= 0, lpdf=alldata, varname="confirmed",
                  id="CRPS", fuzzy=FALSE){
  if(fuzzy) countries <- findIDnames(countries,"CRPS",lpdf)
  return( lpdf[ (lpdf[varname]>=minval)&(lpdf[,id] %in% countries) , ]) # with id=CRPS and var=confirmed it should work.
}
######################### old, works. but not elegant. 
addrownrs0<-function(lpdf, sortby="") {
  #if !(sortby=="") lpdf[order(lpdf[,sortby])]
  lpdf$counter<-as.numeric(row.names(lpdf))
  return(lpdf)
}# two problems: if rownames change to chr, errors occur. if they get out of order because of a sort, the graphs will be a mess. 
addcounter0<-function(lpdf=alldata,id="CRPS",counter="day"){
  lpdf$counter <- 0 #just to add a column, with a value that will be filled next. otherwise we get too many columns. 
  lpdf<- ddply(lpdf,id, addrownrs0)
  names(lpdf)[names(lpdf)=="counter"] <- counter
  return(lpdf)
}
######################### new, works
addrownrs<-function(lpdf,counter="day", sortby="") {
  if (sortby!="") lpdf[order(lpdf[,sortby]),]
  lpdf[,counter]<-as.numeric(row.names(lpdf))
  return(lpdf)
}# two problems: if rownames change to chr, errors occur. if they get out of order because of a sort, the graphs will be a mess. 
addcounter<-function(lpdf=alldata,id="CRPS",counter="day"){
  lpdf[,counter] <- 0 #just to add a column first
  lpdf<- ddply(lpdf,id, function(lpdf){lpdf[,counter]<- lpdf[,counter]<-as.numeric(row.names(lpdf));lpdf} )
    
  return(lpdf)
}
############### used in graphit2 and for saving to csv
addcounterfrommin<-function(minv=0,lpdf=alldata,varname="confirmed",id="CRPS",counter="day"){
  lpdf[,counter]<-NA
  lpdf[lpdf[,varname]>=minv,]<- 
      ddply(lpdf[lpdf[,varname]>=minv,],id, 
          function(lpdf){lpdf[,counter]<- lpdf[,"Date"] - min(lpdf[,"Date"])+1;lpdf})
  lpdf
}
### make dayvars for tableau
mkcountname <- function(countname,minv){paste(countname,minv,sep="_")}
addcounters<- function(lpdf=alldata){
  for (minv in c(1,20,100,400,1000,2000,5000,10000)){
    lpdf<- addcounterfrommin(minv=minv, lpdf=lpdf,counter=mkcountname("day",minv))
  }
  write.csv(lpdf,file="Covid19_days.csv", na="")
  lpdf
}
#alldata<- addcounters(makeGroups( mklpdf())) #,"Region")


#*Now plot* note graphit is just here for legacy reasons. it is deprecated. graphit2 will become graphit soon. 
#
#```{r}
graphit <- function(countries=unique(alldata$CRPS), minval=1, ID="CRPS", varname="confirmed",
                    lpdf=alldata, countname="counter", needfuzzy=TRUE,  logy=TRUE,
                    saveit=FALSE, legend=FALSE, size=1){
  countries<- findIDnames(countries,ID,lpdf,needfuzzy)
  lpdf<- addcounter(
        datasel(countries,minval,var=varname,id=ID, lpdf=lpdf, fuzzy=FALSE),
        ID,countname)
  #for (varname in varnames)
    lpdf[lpdf[,varname]==0,varname]<- NA
  myplot<- ggplot(lpdf,aes_string(x=countname,y=varname,color=ID,group=ID)) +  
    geom_line(size=size, alpha=0.2)+geom_point(size=0.7*size,shape=1)+
    geom_dl(aes_string(label = ID) , method = list(dl.trans(x = x + 0.2),
                                                   "last.points", cex = 0.8))+
    ylab(paste(varname, ifelse(logy," (log scale)","")))+
    xlab("day")+
    ggtitle(paste("Covid-19 evolution after the first",minval,varname)) +
    theme_light() + #
    theme(plot.title = element_text(size = 12, face="bold"))
    
  if (length(countries)<12) 
    myplot<- myplot + scale_color_brewer(palette="Spectral",guide = ifelse(legend,"legend",FALSE)) 
  else myplot<- myplot + scale_color_discrete(guide = ifelse(legend,"legend",FALSE))
  if(logy) myplot<- myplot+scale_y_continuous(trans='log2')
  #if (saveit) savePlot(paste("plots/",varname,format(Sys.Date(),format="%Y%m%d")," in ( ", 
   #                     paste(findIDnames(countries,ID,lpdf,needfuzzy),collapse=", " )," )"),
    #                    type= "png")
  return(myplot)
}
#scale_color_brewer(palette="Set3",guide = ifelse(legend,"legend",FALSE)) #Dark2
#geom_text(data = lpdf, aes_string(label = ID,color = ID,x =Inf,y =max(value) ), hjust = -10) 

graphit2 <- function(countries="", minval=1, ID="CRPS", 
                     varnames=c("confirmed", "recovered"), lpdf=alldata, countname="counter",
                     needfuzzy=TRUE,  logy=TRUE, saveit=FALSE, legend=FALSE, size=1){
  if (countries=="") countries<= unique(lpdf[,ID])
  countries<- findIDnames(countries,ID,lpdf,needfuzzy)
  lpdf <- addcounterfrommin(minval,datasel(countries,minval=0,id=ID, lpdf=lpdf,varname=varnames[1],fuzzy=FALSE),id=ID,counter=countname)
  for (varname in varnames)  lpdf[lpdf[,varname]==0,varname]<- NA
  myplot<- ggplot(lpdf[,c(countname, ID,varnames)]) 
  for (varname in varnames) {myplot<- myplot +  
    geom_line(aes_string(x=countname,y=varname,color=c(ID),group=c(ID)),alpha=0.2,size=size)+
    geom_point(aes_string(x=countname,y=varname,color=c(ID),group=c(ID)),size=0.7*size,shape=match(varname,varnames))+
    geom_dl(aes_string(x=countname,y=varname,color=ID,label = ID) , 
            method = list(dl.trans(x = x+0.1 ,y=y+0.1),"last.points", cex = 0.6))
  }  
  myplot<-myplot + ylab(paste(paste(varnames,collapse=", "), ifelse(logy,"(log scale)","")))+
            xlab("day")+ 
            ggtitle(paste("Covid-19",ifelse(length(varnames)>1,paste(varnames,collapse=", "),""),":", minval,"+",varnames[1])) +
            theme_light() + theme(plot.title = element_text(size = 12, face="bold"))
  if (length(countries)*length(varnames)<12) 
    myplot<- myplot + scale_color_brewer(palette="Spectral",guide = ifelse(legend,"legend",FALSE)) 
  else myplot<- myplot + scale_color_discrete(guide = ifelse(legend,"legend",FALSE))
  if(logy) myplot<- myplot+scale_y_continuous(trans='log2')
  if (saveit) save.plot(paste("plots/",paste(varnames,collapse=","),format(Sys.Date(),format="%Y%m%d")," in ( ", 
                            paste(findIDnames(countries,ID,lpdf,needfuzzy),collapse=", " )," )"), type= "png")
return(myplot)
}
#### test
#paste("confirmed",format(Sys.Date(),format="%Y%m%d"), 
 #       paste(findIDnames(countries,ID,lpdf,needfuzzy),collapse=", " ),sep="_")
#####
##########################################################
##update the data and Use it:
alldata<-alldata<- addcounters(makeGroups( mklpdf())) #,"Region")

graphit2(c("San Marino","vatican","andorra",'Monaco','Malta','Liechtenstein' ),1,size=4)
WestvsEast<- c("Italy","Iran","Korea","Germany","France, France","Spain","Norway","Hubei","Belgium","Netherlands","Singapore","Japan","Shanghai","denmark")

#for publication
graphit2(WestvsEast,500,size=3)
graphit2(WestvsEast,10,logy=FALSE)
graphit2(MENA,20,varnames=c("confirmed","recovered"), size=1,needfuzzy=FALSE)
graphit2(c(Africa),3,size=2,varnames="confirmed")
graphit2(c(Africa,"Japan","Italy"),3,size=2,varnames="confirmed")
graphit2(smallEurope,50,size=2,varnames=c("confirmed","deaths"))
smallEurope <- c("Poland","Belgium","Netherlands","Austria","Romani","Hunga","Ireland","Sweden","Denmark","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Luxembourg","Cyprus","United K","Swit","Norway","Iceland")
graphit2("Australia",10,size=2)
graphit(SAmerica,10,size=3)
graphit2(c(", US"),100,varname=c("confirmed"), ID="CRPS",size=.5,needfuzzy=TRUE) #,"deaths","recovered"
graphit(c(", US"),100,varname=c("confirmed"), ID="CRPS",size=.5,needfuzzy=TRUE) #,"deaths","recovered"
graphit2(c(", US","Hubei"),100,varname=c("confirmed"),size=.7,needfuzzy=TRUE)

graphit2(c("France", "Belgium","Netherlands","Italy","Germany","United Kingdom"),minval=100, size=2,ID="Country.Region", varnames=c("confirmed" , "deaths"))#,logy=FALSE)
#####


graphit2(EU,50,logy=FALSE)
graphit2(EU,100,size=3,varnames=c("confirmed","deaths"))
graphit2(EU,500,varnames="confirmed",size=2)
graphit(EU,500,varname="confirmed",size=2)
graphit2(EU,10,varnames="deaths")
graphit2(EU,20,varnames="recovered")
graphit2(EU,0,varnames="recoveredOverDead",logy=FALSE)
graphit2(EU,0,varnames="recoveredOverConfirmed",logy=FALSE)
graphit2(smallEurope,50,size=2,varnames=c("confirmed","deaths"))
graphit2(c("Germany","France","Italy","Netherlands","Belgium","Japan", "Korea, South"),ID="Country.Region", varnames=c("recovered","deaths"),size=2)#,logy=FALSE)

graphit2(c("Netherlands, Ne","Belg", "France, Fra","Germany","Italy"),100,size=2)
graphit2("France",varnames=c("confirmed","deaths","recovered"))
lpdf=totals(rows="France", id="Country.Region")
names(lpdf)
lpdf <- addcounterfrommin(10,
            datasel("France",minval=0,id=, lpdf=lpdf,varname=varnames[1],fuzzy=FALSE)
                          ,id=ID,counter=countname)
graphit2("France",100)
graphit2("France",100,lpdf=lpdf,ID="Country.Region")
graphit2("France",ID="Country.Region",lpdf=totals(rows="France", id="Country.Region"))
graphit2(c("Belgium","Netherlands"),minval=100,varnames=c("confirmed","deaths"),ID="CRPS",needfuzzy=TRUE)

graphit2(testcountries, ID="Country.Region",varnames=c("confirmed","deaths","recovered"),needfuzzy=TRUE)
graphit2(EU,minval=100,varnames="confirmed",logy=FALSE,size=2)
graphit2(Europe,minval=100,varnames="confirmed",logy=FALSE,size=2)

graphit2(WestvsEast,100,size=1,varnames="confirmed")
graphit2(CIS,10,varnames=c("confirmed"))




graphit2(WestvsEast,0,size=1,varnames=c("confirmed"))
graphit2(WestvsEast,0,size=1,varnames=c("confirmed", "deaths"))
graphit2(WestvsEast,100,size=1,varnames=c("confirmed", "recovered"))

graphit2(WCAsia,10,size=2)

graphit2("china", 1000,size=1)
graphit2("china", 1000,size=2)

graphit("china",0,varname="recoveredOverDeaths")
graphit2("china",10,varname="recovered", logy=FALSE)
names(alldata)
graphit("china",0,varname="recoveredOverConfirmed",logy=FALSE,legend=TRUE)
graphit(EU,0.25,varname="recoveredOverConfirmed",logy=FALSE,legend=TRUE)
####### Canada
graphit2(c("Canada"),20)


##### USA

graphit2(c("US","China"),10,varname=c("confirmed"), ID="Country.Region",size=.5,needfuzzy=FALSE) #,"deaths","recovered"
graphit2(c(", US","Hubei"),100,varname=c("confirmed","deaths","recovered"),size=.7,needfuzzy=TRUE)
names(totals(rows="US", id="Country.Region"))
graphit2("California", minval=1, lpdf=totals(rows="California", id="State"), ID="State")
graphit2(c("California","New York"),lpdf=totals(rows="US", id="CRPS"),ID="State")
graphit2("US",ID="Country.Region",lpdf=totals(rows="US", id="Country.Region"))


#findIDnames(SAsiaIO)
graphit2(SAsiaIO,8, size=2)
graphit2(CIS,10,varnames=c("confirmed"))
graphit2(MENA,20,varnames=c("confirmed","recovered"), size=1,needfuzzy=FALSE)
graphit2(c(Africa),3,size=2,varnames="confirmed")
graphit2(c(Africa,"Japan","Italy"),3,size=2,varnames="confirmed")
graphit2("Italy" , minval=500)
graphit("Italy" ,minval=10, varname="deaths")
graphit("Italy" ,minval=500, varname="recovered")
graphit2(CIS,minval=10)
