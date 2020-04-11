verbose=1
source("requirements.R")

#```
#*Note*: the data of John hopkins, git@github.com:CSSEGISandData/COVID-19.git
#before 20200325:#c<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
#after 20200326:
#c<-read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'

#for the USA: 
  #https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
#RAW: 
#https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
#US headers: UID,iso2,iso3,code3,FIPS,Admin2,Province_State,Country_Region,Lat,Long_,Combined_Key,1/22/20,
readUSdata<-function(dataversion="confirmed"){#deaths and recovered are the other options. 
  filename<- paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_',dataversion,"_US.csv",sep="")
  tryCatch( wpdf <-read.csv(filename) ,
            error= function(e) print(paste(e," The data was not found: Are you sure this file exists? ",filename))
  )
  return(wpdf)
}

readdata<-function(dataversion="confirmed"){#deaths and recovered are the other options. 
  filename<- paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_',
                    dataversion,"_global.csv",sep="")
  tryCatch( wpdf <-read.csv(filename) ,
            error= function(e) print(paste(e," The data was not found: Are you sure this file exists? ",filename))
  )
  return(wpdf)
}
convertdata <-function(wpdf,coltype="date", values.name="count",US=FALSE){ 
  id<-c("Country.Region","CRPS","Province.State","Lat","Long")
  if (US) { id<-c(id,"Combined_Key")   }
  wpdf$CRPS <- (ifelse(""==wpdf$Province.State, 
                              as.character(wpdf$Country.Region),
                              paste(wpdf$Province.State,wpdf$Country.Region,sep=', ')))
  lpdf<-reshape2::melt(wpdf,id=id,
                       variable.name=coltype, value.name=values.name)   
  lpdf$Date<- as.Date(paste(lpdf[,coltype],"20",sep=""),format="X%m.%d.%Y") 
  lpdf$date<- NULL
  return(lpdf)
} #note if data.table package is added, it has its own "melt" function

findidnames <- function(testidnames=c("Neth","India"), searchid="CRPS",
                        lpdf=covid19, fuzzy=TRUE, returnid=""){
  if (returnid=="") returnid=searchid
  allids<- drop(unique(lpdf[,searchid]))   #error maybe? deleted drop
  if (!fuzzy) {a1<- intersect(testidnames,allids)
  }else a1<- allids[unlist(llply(testidnames,function(a) grep(a,allids, ignore.case=TRUE)))]
  unique(lpdf[lpdf[,searchid] %in% a1,returnid])
} 
aggreg<- function(avector){
  if(length(unique(avector))==1){avector[1]
    }else #if (length(unique(avector))==2) {paste(avector,collapse="_") }else
           paste(avector[1],length(unique(avector)),avector[length(avector)],sep="_")
}
#following regions' number are given by state/province/country and we want the whole( the country, or for Europe: the continent's numbers)
#c("Canada","US","Australia","China",Europe
## "active","new_confirmed","new_deaths","new_recovered"
total<- function(rows="", 
                 id="CRPS", newrow="" ,lpdf=covid19,
                 varnames=c("confirmed","deaths","recovered")  
                 ) {
  ans<- ddply(lpdf[lpdf[,id] %in% rows,],c("Date"),
    function(a) {
      Country.Region=ifelse(newrow!="", newrow,
                            aggreg(as.character(a$Country.Region)))
      Province.State=ifelse(newrow!="",newrow,aggreg(a$Province.State))
      CRPS=ifelse(newrow!="",
                  newrow,
                  ifelse(id=="CRPS",
                         aggreg(a$CRPS) ,
                         ifelse(Province.State=="",
                                Country.Region,
                                paste(Province.State,Country.Region,sep=', ')
                                )
                         )
                  )
      b1<- colSums(a[,varnames],na.rm=TRUE)
      nam<- names(b1)
      dim(b1)<- c(1,length(b1))
      b2<-data.frame(b1)
      colnames(b2)<- nam
      cbind(Country.Region,
        CRPS,
        Province.State,
        Lat=mean(a$Lat) ,Long=mean(a$Long),
        b2
  )  }) #,County=aggreg(a$County)#Region=aggreg(a$Region)
  ans[,setdiff(names(lpdf),names(ans))]<-NA
  ans
}


totals<- function(rows="", #c("Canada","US","Australia","China",Europe
                  id="Country.Region", 
                  varnames=c("confirmed","deaths","recovered"),
                  # "active","new_confirmed","new_deaths","new_recovered"
                  lpdf=covid19){
  if (rows[1]=="") rows=unique(lpdf[,id])
  if (verbose>0) print(paste("Making totals of ",paste(rows,collapse=","),"in",id))
  ans<-ldply(rows, function (a) total(a,id,a, lpdf,varnames))
}
             
totals2<- function(rows="", #c("Canada","US","Australia","China",Europe
                  id="Country.Region", 
                  varnames=c("confirmed","deaths","recovered"),
                      # "active","new_confirmed","new_deaths","new_recovered"
                  lpdf=covid19){
  if (rows[1]=="") rows=unique(lpdf[,id])
  if (verbose>0) print(paste("Making totals of ",paste(rows,collapse=","),"in",id))
  ans<-ddply(lpdf[lpdf[,id] %in% rows,],c("Date", id),
             function(a) {
          Country.Region=aggreg(as.character(a$Country.Region))
          Province.State= aggreg(a$Province.State)
          CRPS=ifelse(Province.State=="",Country.Region,paste(Province.State,Country.Region,sep=', '))
          b1<- colSums(a[,varnames],na.rm=TRUE)
          nam<- names(b1)
          dim(b1)<- c(1,length(b1))
          b2<-data.frame(b1)
          colnames(b2)<- nam
          cbind(Country.Region,
               CRPS,
               Province.State,
               Lat=mean(a$Lat) ,Long=mean(a$Long),
               b2
               )  }) #,County=aggreg(a$County)#Region=aggreg(a$Region)
  ans[,setdiff(names(lpdf),names(ans))]<-NA
  ans
}

correctnames<- function(df){
  names(df)[match("Long_",names(df))]<-"Long"
  names(df)[match("Province_State",names(df))]<-"Province.State"
  names(df)[match("Country_Region",names(df))]<-"Country.Region"
  df[,!names(df) %in% c("Admin2" ,"UID","iso2","iso3","code3","FIPS")]
}

makelpdfUS <- function() {
  wc<-readUSdata('confirmed') 
  geo.location <- wc[,c("Combined_Key","Country_Region","Province_State","Admin2","UID","Lat","Long_")]
  wc<- correctnames(wc)
  write.csv(geo.location,file="geo.location.US.csv",na="")
  rm(geo.location)
#  wc<-wc[,!names(wc) %in% c("Admin2" ,"UID","iso2","iso3","code3","FIPS")]
  confirmed<- convertdata(wc,values.name="confirmed",US=TRUE)
  wd<-readUSdata("deaths")
  wd<- correctnames(wd)
  #wd<-wd[,!names(wd) %in% c("Admin2" ,"UID","iso2","iso3","code3","FIPS")]
  deaths<- convertdata(wd,values.name="deaths",US=TRUE)
  lpdf<- merge(confirmed,deaths,all.x=TRUE,
               by=c("Country.Region","Province.State","CRPS","Combined_Key","Lat","Long","Date"))
  #tryCatch( wr<-readUSdata("recovered"),error=function(e){print(e)})
  #if ("wr" %in% ls()){
  #  wr<- correctnames(wr)
  #  recovered<- convertdata(wr,values.name="recovered")
  #  lpdf<-merge(lpdf,recovered,all=TRUE,
  #    by=c("Country.Region","Province.State","CRPS","Combined_Key","Lat","Long","Date"))
  #}else 
    lpdf$recovered<- NA 
  lpdf<- (totals2("","Province.State",lpdf=lpdf)) # depends on all provinces being chosen to sum! 
  #lpdf<-lpdf[with(lpdf, order(CRPS, Date)), ]  
  lpdf$Combined_Key<-NULL
  lpdf  <- rbind(lpdf,(totals("US","Country.Region",lpdf=lpdf)))
}
#covid19<- (makelpdfUS())
#summary(covid19)

makelpdf <- function() {
  wc<-readdata('confirmed') #"Confirmed")
  geo.location <- wc[c("Country.Region","Province.State","Lat","Long")]
  #write.csv(geo.location,file="geo.location.csv",na="")
  confirmed<- convertdata(wc,values.name="confirmed")
  wd<-readdata("deaths")
  deaths<- convertdata(wd,values.name="deaths")
  wr<-readdata("recovered")
  names(wr)[1]<-names(wc)[1] #"Province.State without strange characters
  recovered<- convertdata(wr,values.name="recovered")
  lpdf<- merge(confirmed,recovered,all.x=TRUE,#,sort=FALSE,
                  by=c("CRPS","Date","Country.Region","Province.State","Lat","Long"))
  lpdf<- merge(lpdf,deaths,all.x=TRUE,
                  by=c("Country.Region","CRPS","Province.State","Date","Lat","Long"))
  lpdf[lpdf$CRPS=="US",]$CRPS<- as.character("USA") #to distinguish from the detailed data
  lpdf[lpdf$CRPS=="USA","Country.Region"] <- as.character("USA") #bug: this results in NA. probably it is a factor and this is not one of the existing levels. 
  lpdf#<- rbind(lpdf,totals(c("Canada", "China", "Australia"), varnames=c("confirmed","recovered",'deaths'),lpdf=lpdf))
}
updateDataFromWeb <- function(nameUS="Covid19_US.csv",namenonUS="Covid19_non_US.csv") {
  CUS<-makelpdfUS() 
  write.csv(CUS,nameUS)
  Cworld<- makelpdf() 
  write.csv(Cworld,namenonUS)
  lpdf<- rbind(Cworld,CUS)
}
readLocalData<- function(nameUS="Covid19_US.csv",namenonUS="Covid19_non_US.csv"){
  CUS<- read.csv(nameUS,stringsAsFactors=FALSE)#colClasses= ("Date"="character"))
  Cworld<-read.csv(namenonUS,stringsAsFactors=FALSE)
  lpdf<- rbind(Cworld,CUS)
  #lpdf[,"X"]<-NULL
  lpdf
}

diff.lpdf<- function(lpdf,id="CRPS", varnames=c("confirmed","recovered", "deaths"),prefix="new_"){
  ans<- ddply(lpdf, id, 
        function (lpdf){
          data.frame(llply(lpdf[,varnames], 
                           function(a){c(NA,base::diff(a))}
                           ))
          }
        )
  names(ans)<- paste(prefix,names(ans),sep="")
  ans
}
###### make fractions and diffs this should happen only AFTER PLM
extravars<- function(lpdf,lagr=0,lagd=0){ #note only use lags if lpdf is a real lpdf a la plm
  lpdf<- lpdf[!("Princess" %in% lpdf$Province.State),]
  population<- read.csv('population.csv')[c(1,3)]
  names(population)[2]<- "population"
  if (!("population"%in% names(lpdf)))  lpdf<- merge(lpdf,population,by.x="Country.Region",by.y="Country.Name",all.x=TRUE)
  popUS<- read.csv('USstatespop2019.csv')[c('State','p2019')]
  names(popUS)[2]<- "population"
  rownames(popUS)<- popUS$State
  #View(lpdf[is.na(lpdf$population)&lpdf$Date=="2020/04/01",])
  #View(lpdf[lpdf$Country.Region=="US" &lpdf$Date=="2020/04/01",])
  lpdf[",US" %in% lpdf$CRPS,"population"]<- popUS[as.character(lpdf[",US" %in% lpdf$CRPS,"Province.State"]),"population"]
  
  lpdf$active <- lpdf$confirmed - lpdf$deaths - lpdf$recovered
  lpdf$recovered_per_confirmed<- ifelse(lag(lpdf$confirmed,lagr)>0, lpdf$recovered/lag(lpdf$confirmed,lagr), NA)# lag the confirmed by 30 days as recovery takes a while.... 
  lpdf$deaths_per_confirmed<- ifelse(lag(lpdf$confirmed,lagd)>0, lpdf$deaths/lag(lpdf$confirmed,lagd), NA)
  lpdf$recovered_per_deaths <-ifelse(lag(lpdf$deaths,lagr-lagd)>0,lpdf$recovered/lag(lpdf$deaths,lagr-lagd), NA)   #lag the deaths by 10 days as recovery takes longer?
  #### for diffs, make sure is ordered by date! 
  lpdf<-lpdf[with(lpdf, order(CRPS, Date)), ]
  lpdf$new_confirmed<-NULL
  lpdf$new_recovered<-NULL #just to make sure if we twice use this function we dont get into problems. 
  lpdf$new_deaths<- NULL
  lpdf<- cbind(lpdf,diff.lpdf(lpdf))
  
  lpdf$confirmed_pM <- 1000000*lpdf$confirmed/lpdf$population
  lpdf$active_pM    <-1000000* lpdf$active  /lpdf$population
  lpdf$recovered_pM <- 1000000*lpdf$recovered/lpdf$population
  lpdf$deaths_pM    <- 1000000*lpdf$deaths   /lpdf$population
  
  lpdf$new_confirmed_pM <- 1000000*lpdf$new_confirmed/lpdf$population
  lpdf$new_recovered_pM <- 1000000*lpdf$new_recovered/lpdf$population
  lpdf$new_deaths_pM    <- 1000000*lpdf$new_deaths  /lpdf$population
  lpdf$Date<- as.Date(lpdf$Date,format="%Y-%m-%d") #otherwise we cannot calculate min confirmed. 
  lpdf$CRPS<- sortCountries(lpdf)
  lpdf
}


######## make state groups, also useful in tableau
WestvsEast<- c("WestvsEast","USA","UK","Italy","Iran","Korea","Germany","France","Spain","Sweden","Norway","Hubei","Belgium","Netherlands","Singapore","Japan","Shanghai","Denmark","Hubei, China", "Hongkong, China", "Jiangsu, China")
Benelux<- c("Benelux","Belgium","Netherlands","Luxembourg")
EU6<-c("EU6", Benelux[2:4], "Germany","France","Italy")
EU<- c("EU",EU6[2:7],"Spain","Poland","Austria","Romania","Hungary","Ireland","Sweden","Denmark","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Cyprus")
EFTA<-c("EFTA","Iceland","Liechtenstein","Switzerland","Norway")
Europe<- c("Europe",EU[2:28],EFTA[2:5], "United Kingdom", "Russia", "Ukraine", "Belarus","Moldova","Georgia", "Armenia", "Azerbaijan","Andorra", "Monaco", "San Marino", "Vatican","Holy See", "Albania", "North Macedonia","Kosovo","Croatia","Montenegro","Bosnia and Herzegovina","Serbia","Gibraltar","Faroe Islands", "Isle of Man","Channel Islands","Greenland")

CIS<- c("CIS","Russia", "Belarus","Georgia", "Armenia", "Azerbaijan", "Ukraine","Kazakhstan","Kyrgyzstan","Turkmenistan", "Tadjikistan","Uzbekistan")
SouthWestAsia<-c("South West Asia","Afganistan","Iran","Irak","Syria","Lebanon","Turkey","Israel","Palestine")
SouthEastAsia<- c("South East Asia","Indonesia","Thailand","Vietnam","Laos","Malaysia", "Cambodia", "Papua New Guinea","Myanmar", "Burma","Brunei","Philippines","Timor-Leste")
SAsiaIO<-c ("South Asia & Indian Ocean","India","Pakistan","Bangladesh","Sri Lanka","Comoros", "Maldives","Madagascar","Mauritius", "Seychelles","Bhutan","Nepal","Mayotte","Reunion")
MENA<-c("MENA", "Marocco","Algeria","Tunesia","Libia","Egypt","Lebanon","Syria","Turkey","Jordan","Saudi Arabia","Kuwait","Oman","United Arab Emirates","UAE","Yemen","Bahrain","Qatar","Iraq","Iran","Afghanistan", "West Bank and Gaza")
SAmerica<-c("South America","Argentina","Bolivia","Brazil","Chile","Colombia","Costa Rica","Honduras","El Salvador","Panama","Ecuador","Suriname","Guyana","Belize","Guatemala", "Antilles","Paraguay","Peru","Venezuela","Nicaragua"       , "Uruguay","French Guiana","Falkland Islands (Malvinas)","Nicaragua")
US<-c("US","USA")
NAmerica<- c("NAmerica",US,"Canada","Mexico","Saint Pierre and Miquelon")
Caribbean<- c("Caribbean",'Anguilla',"Antigua and Barbuda","Bahamas" ,  "Barbados","Bermuda","Cayman Islands","Cuba","Dominica" ,"Dominican Republic","Grenada", "Haiti" , "Jamaica","Saint Kitts and Nevis" ,"Saint Vincent and the Grenadines","Saint Lucia"  ,"Trinidad and Tobago",'Aruba','Curacao',"Bonaire, Sint Eustatius and Saba","British Virgin Islands",'Guadeloupe','Martinique','Sint Maarten','St Martin','Saint Barthelemy','Turks and Caicos Islands','Montserrat')
Africa<- c("Africa","Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon","Central African Republic","Chad","Comoros", "Congo (Kinshasa)", "Congo (Brazzaville)", "Cote d'Ivoire", "Djibouti", "Egypt",  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana","Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara","Zambia", "Zimbabwe")
EastAsia<- c("East Asia","Japan","Korea, South", "Korea, North","Taiwan*", "Hong Kong","Singapore","Mongolia")
Oceania<- c("Oceania","Australia","New Zealand","Vanuatu","Tuvalu", "Fiji","Guam","French Polynesia","New Caledonia"  )
China<- c("China")
MSM<-c("MSM","Netherlands","Belgium","United Kingdom","Germany","Malta","Egypt","Suriname","China","Vietnam","Hungary","Romania","Kuwait","Italy","Ireland","Iran","Kazakstan","Liberia","Indonesia","Ethiopia","Nigeria","Ghana","Uganda","South Africa","Canada","Spain","France")
Vincent<- c("Some Selected Regions","Belgium","Germany","Italy","France","Kazakhstan","Indonesia","Spain","Netherlands","Japan","New York")
getname <- function (variab, name=deparse(substitute(variab))) { 
  name 
}
############### used in graphit2+ and for saving to csv
addcounterfrommin<-function(minv=0,lpdf=covid19,varname="confirmed",id="CRPS",counter="day"){
  lpdf[,counter]<-NA
  lpdf<- lpdf[!is.na(lpdf[,varname]),]
  if(sum(lpdf[,varname]>=minv)>0)  
    lpdf[lpdf[,varname]>=minv,]<- 
          ddply(lpdf[lpdf[,varname]>=minv,],id, 
            function(lpdf){
              lpdf[,counter]<- lpdf[,"Date"] - min(lpdf[,"Date"])+1
              lpdf}
            )
    lpdf
}

### make day vars for tableau & Excel
makecountname <- function(countname,minv){paste(countname,minv,sep="_")}

writewithcounters<- function(lpdf=covid19,varname="confirmed",id="CRPS",name="Covid19"){
    lpdf<- lpdf[!is.na(lpdf[c(varname)]),]
    for (minv in c(1,20,100,400,1000,2000,5000,10000)){
      lpdf<- addcounterfrommin(minv=minv, lpdf=lpdf,
                               varname=varname,id=id,
                               counter=makecountname("day",minv))
    }
    filename=paste(name,"days.csv",sep="_")
    write.csv(lpdf,file=filename, na="")
    if (verbose>0) print(paste("Written the current data with counters to disk as",filename,"for use in Tableau or Excel"))
}

makeCovid19 <- function(name="Covid19",force=FALSE) {
  nameUS<- paste( paste(name,"US",sep="_"),           "csv",sep=".")
  namenonUS<- paste( paste(name,"non","US",sep="_"),  "csv",sep=".")
  namedays<- paste(name,"_days.csv",sep="")
  if(force|(difftime(Sys.time(), file.info(namedays)[,"mtime"], units = "hours")>6)) {
    lpdf<- updateDataFromWeb(nameUS,namenonUS)
    if (verbose>0) print("updating from Github")
  } else {
    if (verbose>1) print(namedays)
    lpdf<- read.csv(namedays,stringsAsFactors = FALSE)
    }
  if (typeof(lpdf$Date)=="character") 
    lpdf$Date <- as.Date(lpdf$Date, "%Y-%m-%d")  #strptime gives timezones! no need
  if (verbose>0) 
    print(paste(max(lpdf$Date), "missing values:",sum(is.na(lpdf))))
  lpdf
}
provincialize<- function(countries,lpdf=covid19){ 
  cl<- intersect(countries,Worldcountrylist)
  cl1<-unique(lpdf[lpdf$Country.Region %in% cl,]$CRPS)
  c(paste(countries[1],"Provinces&States"),setdiff(cl1,countries))
}
CanadaP<-provincialize('Canada')
USS<-provincialize(US)
NAmericaS<- provincialize(NAmerica)
OceaniaP<- provincialize(Oceania)
ChinaP<- provincialize(China)
makeGroups <- function(lpdf=covid19,varname="Region",list=Regiolist) {  
  if(!varname %in% names(lpdf)) lpdf[,varname]<-""
  for (Regio in list) {
    lpdf[lpdf$Province.State %in% Regio,varname] <- Regio[[1]]
    lpdf[lpdf$CRPS %in% Regio,varname]<- Regio[[1]]}
  lpdf
}

sortCountries<- function(lpdf,varname="confirmed",id="CRPS"){
  ordre<- lpdf[lpdf$Date== max(lpdf$Date),c(varname,id)]
  ordre<- ordre[order(ordre[,varname],ordre[,id],decreasing=TRUE),] 
  #c(TRUE,FALSE)),]
  lpdf[,id]<- factor(lpdf[,id],levels=ordre[,id])
}
#Next, prepare functions to select data we want to line graph, determined by the minimum value , date, and country/id

testcountries<- c("Belgium","Vietnam","Indonesia","Korea","Japan","France","Germany","Italy",  "Netherlands", "Hubei, China","Taiwan")

datasel<- function(countries=MSM, minval= 0, lpdf=covid19, 
                   varname="confirmed", id="CRPS"){
  return( lpdf[ (lpdf[varname]>=minval)&(lpdf[,id] %in% countries) , ])
}

dataprep1<- function(countries=NULL, minval=1, id="CRPS", 
                     xvar="day", yvars=c("confirmed", "recovered"), 
                     lpdf=covid19, fuzzy=TRUE,logx=FALSE, logy=TRUE, 
                     size=3,until=Sys.Date(),returnid="CRPS"){
  lpdf<- lpdf[lpdf$Date<=until,]
  if (length(countries)==0) countries<- unique(lpdf[,id])
  countries<- findidnames(countries,searchid=id,lpdf=lpdf,fuzzy=fuzzy,returnid)
  lpdf <- datasel(countries,minval=minval,varname="confirmed", lpdf=lpdf,id=returnid)#,varname=yvars[1])
  if (!(xvar %in% names(lpdf))) lpdf<- addcounterfrommin(minval,lpdf,varname="confirmed", id=returnid,counter=xvar)
  if (logy) for (varname in yvars)  {
    lpdf[(is.na(lpdf[,varname])),varname]<- 1
    lpdf[lpdf[,varname]  <= 0,varname]<- 1 
  }
  if(logx) lpdf[lpdf[,xvar]<=0,xvar]<- 1 
  lpdf[,id]<- sortCountries(lpdf=lpdf,varname=yvars[1],id=id)
  lpdf[,c(xvar, returnid,yvars)]
}


dataprep2<- function(id, xvar, yvars, lpdf, until, 
                     variable.name="varname", value.name="count"){
  lpdf<- melt(lpdf ,id=c(id,xvar),measure.vars=yvars,
              variable.name=variable.name, value.name=value.name)
  lpdf[,variable.name]<- factor(lpdf[,variable.name], levels = yvars)
  lpdf$mygroup<- paste(lpdf[,id],lpdf[,variable.name],sep=", ")
  lpdf
}
#for readable labels on facet titles, split them into lines with
#reformat <- function(x,splitt=", ",lab="\n"){ sapply(x, function(c){ paste(unlist(strsplit(as.character(c) , split=splitt)),collapse=lab) }) }
#dataset$variable <- factor(dataset$variable, labels=reformat(dataset$variable, lab='\n')
#And upon facetting, all labels will be very readable:
 # ggplot(data=dataset, aes(x,y)) + geom_point() + facet_grid(. ~ variable)
#that is for factor labels. we need it for variable lables. 

#Better, complete legends.
graphit5 <- function(countries=NULL, minval=1, id="CRPS", xvar="Date", 
                     yvars=c("confirmed","active", "recovered","deaths"), 
                     lpdf=covid19, fuzzy=FALSE, logx=FALSE, logy=FALSE, 
                     savename="", putlegend=TRUE, size=3, returnid="CRPS", 
                     area=FALSE,facet=FALSE, until=Sys.Date()){
  lastdate<- min(max(lpdf$Date),until)
  lpdf<- dataprep1(countries,minval,id,xvar,yvars,lpdf,fuzzy,logx,logy,until, returnid=returnid)
  id=returnid
  if (nrow(lpdf)==0) return(print ("Too little data to graph. Maybe lower the mininum value?"))
  lpdf<- dataprep2(id=id,  xvar=xvar, yvars=yvars, lpdf=lpdf,
                  variable.name="varname", value.name="count")
  if (facet=='varname') lpdf$mygroup<- lpdf$id 
  if (facet==id) lpdf$mygroup <- lpdf$varname
  myplot<- ggplot(lpdf, aes_string(y="count",x=xvar,group='mygroup')) 
  if(area){myplot<- myplot + geom_area(aes_string(color='mygroup',fill='mygroup'), position = 'stack',alpha=.8)+scale_fill_manual(values = c("orange", "green","red","blue"))
  }else {myplot<- myplot+geom_line(alpha=0.3,size=size*0.7,aes_string(color=ifelse(facet==id,'mygroup',id)))+
    geom_point(aes_string(color=ifelse(facet==id,'mygroup',id),shape='varname'),    size=size)+
    geom_dl(aes_string(x=xvar,y="count",color=ifelse(facet==id,'mygroup',id),label='mygroup'),
            method = list(dl.trans(x = x+0.1 ,y=y+0.1),"last.points", cex = 1.2))
  }  
  if (!isFALSE(facet)) {
    myplot<- myplot+ facet_wrap(as.formula(paste("~",facet)))
  }
    mytitle<- paste("Covid-19",format(lastdate,format="%Y%m%d"),
                  savename, paste(yvars,collapse=" & "),
                  "by",xvar,"for",paste(minval,"+",sep=""),"confirmed")
    palette=ifelse (length(unique(lpdf$mygroup))<8, "Dark2","Paired") #Spectral Set2  
    if (length(unique(lpdf$mygroup))<=4){
      myscale<- scale_color_manual(values=c("orange", "green", "red","black"),guide= ifelse(putlegend,"legend",FALSE))  ##the colors need to be in the right order: 1 orange 2 green 3 red 4 black scale_color_manual(values=["black","orange", "green", "red"])
      }else if(length(unique(lpdf$mygroup))<13) {
    myscale<- scale_color_brewer(palette=palette) #
  }  else myscale<- scale_color_discrete(guide = ifelse(putlegend,"legend",FALSE)) #
    if(xvar=="Date" ) myplot<- myplot+scale_x_date(labels = date_format("%d-%m"))
    myplot<-myplot + 
    ylab(paste(paste(yvars,collapse=" & "), ifelse(logy,"(log scale)","")))+
    xlab(paste(xvar,                       ifelse(logx,"(log scale)","")))+ 
    ggtitle(mytitle) +
    theme_light() +
    
    myscale 
  if(logy) myplot<- myplot+scale_y_continuous(trans='log10')
  if(logx) myplot<- myplot+scale_x_continuous(trans='log10')
  if (savename!="") {
    myplot<- myplot+ theme(text=element_text(size=20)#,
                           #title=element_text(size=18),
                           #strip.text=element_text(size=20)
                           )
    png(filename=paste("plots/",#format(Sys.Date(),format="%Y%m%d"),"/",
                       mytitle, ifelse(logy,", log scale",""),
                       ".png",sep=""),
        width=1600,height=900)
    print(myplot);dev.off()
    #svg(filename=paste("plots/",mytitle, ifelse(logy,", log scale",""),
     #                  ".svg",sep=""), width=16,height=9) #in inches?
    #print(myplot);dev.off()
    print(paste("Plot saved:",mytitle))
  }else return(myplot+theme(title = element_text(size = 11)))
}# 

#geom_point(aes(shape=cyl, color=cyl, size=cyl))
#+scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))

#1 conf+active+recovered+deaths         areaplot          facet per id
#2 new conf , new rec, new deaths line plot by date, facets by D
#3 Conf, active, recovered, deaths by day line graph all in one graph
#4 Conf, active, recovered, deaths by date line graph facets

#4 new confpM , new recpM, new deathspM line plot by date, facets by D
#5 deathspM recopM, confpM line graphs
#6 reco over conf by date line graph per id
#7 reco&deaths by conf 
#8 like 2 with areas
graph0<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group, minval, id,xvar='day', yvars=c('active','recovered','deaths' ,"confirmed"), lpdf=lpdf,savename= name,facet=id,putlegend=TRUE)
}
graph1<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group,1,id, xvar="Date",yvars=c('active','recovered','deaths'), 
           lpdf=lpdf,
           savename= ifelse(name=="","",paste(name,"areaplot")),area=TRUE,facet=id) 
}
graph2<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group,minval,id, xvar="Date",yvars=c('new_confirmed','new_recovered','new_deaths'), 
           lpdf=lpdf, logy=TRUE,savename= name,facet=id) 
}
graph8<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group,minval,id, xvar="Date",yvars=c('new_confirmed','new_recovered','new_deaths'), 
           lpdf=lpdf, logy=TRUE,savename= ifelse(name=="",name,paste(name,"areaplot")),facet=id,area = TRUE) 
}

graph3<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group, minval, id,xvar='day', yvars=c( 'active','recovered','deaths',"confirmed"), 
           lpdf=lpdf,savename= ifelse(name=="","",paste(name,"all-in-one")),putlegend=TRUE)
}

graph4<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group,minval,id, xvar="Date",yvars=c('new_confirmed_pM','new_recovered_pM','new_deaths_pM'), 
           lpdf=lpdf,savename= name,facet="CRPS")
}
graph5<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group,minval,id, xvar="Date",yvars=c('active_pM','recovered_pM','deaths_pM','confirmed_pM'), 
           lpdf=lpdf,savename= name,facet="CRPS")
}
graph6<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group,1,id, xvar="Date",yvars=c('recovered_per_confirmed'), 
           lpdf=lpdf,savename= name,putlegend=FALSE )
}
graph7<- function(group,name="",minval=100, id="CRPS",lpdf=covid19){
  graphit5(group,minval,id, xvar='confirmed',yvars=c('recovered','deaths'),
           lpdf=lpdf,  logx=FALSE,savename=name)
}

graphs<- function(group,name="",minval=100,nrs=c(1,2,3), id="CRPS",lpdf=covid19){
  for (j in nrs)
      do.call (paste("graph",j,sep=""),args=list(group,name,minval, id,lpdf))
  }

writeRegiographs<- function(Regionlist,minval=100,nrs=1:4, id="CRPS",lpdf=covid19){
  for (i in 1:(length(Regionlist))){
    print(paste("Region",i,Regionlist[[i]][1]))
    ids<-(findidnames((Regionlist[[i]]),searchid=id,lpdf=lpdf, fuzzy=FALSE,returnid="CRPS"))
    graphs(ids,Regionlist[[i]][1],minval,nrs, id,lpdf)
  }
}
writeRegiograph<- function(Regionlist,minval=100,nrs=c(1,2,3), id="CRPS",lpdf=covid19){
  if (typeof(Regionlist)=="character") {
    name= getname(Regionlist)
    Regionlist=list(c(name,Regionlist))
}
  for (j in nrs)
    for (i in 1:(length(Regionlist))){
      ids<-(findidnames((Regionlist[[i]]),searchid=id,lpdf=lpdf, fuzzy=FALSE,returnid="CRPS"))
      do.call (paste("graph",j,sep=""),args=list(ids,Regionlist[[i]][1],minval, id,lpdf))
    }
}

ccf.vf<- function(var1="new_confirmed", var2="new_recovered", CRPS="Hubei, China",lpdf=covid19,lag.max=30,saveit=FALSE){
  title=paste("ccf of ",var1  , " vs ",var2," for ",CRPS,".png", sep="")
  if (saveit) {png(filename=paste("plots/ccf/",title))}
  myplot=ccf(lpdf[lpdf$CRPS==CRPS&lpdf$Date>"2020-01-22", var1],
             lpdf[lpdf$CRPS==CRPS&lpdf$Date>"2020-01-22", var2],
             lag.max=lag.max,main= title)
  print(myplot)
  if (saveit) {dev.off()} #else {myplot}
  }


#end. Now run loadData.R
