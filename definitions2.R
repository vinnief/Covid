if(!exists("verbose")) verbose=1
myDateFormat<- "%Y-%m-%d"
source("requirements.R")


#```
#*Note*: the data of John hopkins, git@github.com:CSSEGISandData/COVID-19.git
#before 20200325:#c<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
#after 20200326:
#c<-read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_global.csv'

#for the USA: 
  #https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_US.csv
#RAW: 
#https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_US.csv
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
  ID<-c("Country.Region","CRPS","Province.State","Lat","Long")
  if (US) { ID<-c(ID,"Combined_Key")   }
  wpdf$CRPS <- (ifelse(""==wpdf$Province.State, 
                              as.character(wpdf$Country.Region),
                              wpdf$Province.State %,% wpdf$Country.Region ))
  lpdf<-reshape2::melt(wpdf,id=ID,
                       variable.name=coltype, value.name=values.name)  
  #lpdf<- wpdf %>% pivot_longer(????)
  lpdf$Date<- as.Date(paste(lpdf[,coltype],"20",sep=""),format="X%m.%d.%Y") 
  lpdf$date<- NULL
  return(lpdf)
} 

findIDnames <- function(lpdf=JHH, testIDnames=c("Neth","India"), searchID="CRPS",
                         fuzzy=TRUE, returnID=""){
  lpdf<- as.data.frame(lpdf)
  allIDs<- (unique(lpdf[,searchID]))   #error maybe? [ for dataframe 
  if (!fuzzy) {a1<- intersect(testIDnames,allIDs)
  }else a1<- allIDs[unlist(llply(testIDnames,function(a) grep(a,allIDs, ignore.case=TRUE)))]
  if (returnID=="")  return ( a1) #returnID=searchID
  unique(lpdf[lpdf[,searchID] %in% a1,returnID])
  #filter(lpdf, .vars(searchID)%in% a1)%>% select(vars(returnID))
} 
aggreg<- function(avector){
  if(length(unique(avector))==1){avector[1]
  }else #if (length(unique(avector))==2) {paste(avector,collapse="_") }else
    paste(avector[1],length(unique(avector))-2,avector[length(avector)],sep="_")
}

total.tibble<- function(lpt=JHH,ID=Country.Region,varnames=c('confirmed',#deaths,
                                                             'recovered')){
  lpttot<-lpt%>%
    group_by({{ID}},Date)
  lpttot<- cbind(
    #lpttot%>% summarize(newCRPS=aggreg(CRPS),
    #          newCountry.Region=aggreg(Country.Region),
    #          newProvince.State=aggreg(Province.State)), #!! mean_name := mean(!! expr)
    lpttot%>% summarize_at(.vars=!!!varnames, .funs=colSums)
            )
              #confirmed=sum(confirmed),
              #active=sum(active),
              #recovered=sum(recovered),
              #deaths=sum(deaths),
              #colSums(.[,])
  #if (!!ID=="CRPS")lpttot%>% rename(Coun)
}
#JHH[JHH$Country.Region==c('Netherlands'),]%>%total.tibble(CRPS)%>% view()
#JHH[JHH$Country.Region==c('Netherlands'),]%>%total.tibble(Country.Region)%>% view()
total<- function(lpdf=JHH, rows="", 
                 ID="CRPS", newrow="" ,
                 varnames=c("confirmed","deaths","recovered")  
                 ) {
  if (rows[1]=="")rows=unique(lpdf[[ID]])    
  ans<- ddply(lpdf[lpdf[[ID]] %in% rows,],c("Date"),
    function(df) {
      Country.Region=ifelse(newrow!="", newrow,
                            aggreg(as.character(df$Country.Region)))
      Province.State=ifelse(newrow!="",newrow,aggreg(df$Province.State))
      CRPS=ifelse(newrow!="",
                  newrow,
                  ifelse(ID=="CRPS",
                         aggreg(df$CRPS) ,
                         ifelse(Province.State=="",
                                Country.Region,
                                paste(Province.State,Country.Region,sep=', ')
                                )
                         )
                  )
      b1<- colSums(df[,varnames],na.rm=TRUE)
      nam<- names(b1)
      dim(b1)<- c(1,length(b1))
      b2<-data.frame(b1)#as_tibble_row()
      colnames(b2)<- nam
      ans<- cbind(Country.Region,
                  CRPS,
                  Province.State,
                  Lat=mean(df$Lat) ,Long=mean(df$Long),
                  b2
                  )
      if("imputed"%in% names(df)) ans$imputed<- any(df$imputed)
      if("Region" %in% names(lpdf)) 
        ans$Region<- ifelse(newrow!="", newrow, aggreg(df$Region))
      if ("County" %in% names(df)) 
        County=ifelse(newrow!="", newrow,  as.character(aggreg(df$County)))
      ans
      }) 
  ans[,setdiff(names(lpdf),names(ans))]<-NA
  ans
}
#c("Canada","US","Australia","China",america, Africa
totals<- function(lpdf=JHH, rows="",
                  ID="Country.Region", 
                  varnames=c("confirmed","deaths","recovered")
                  ){
  if (rows[1]=="") rows=as.list(unique(lpdf[[ID]]))
  if (verbose>=6) print(paste("Making the total for ",paste(rows,collapse="/ "),"in",ID))
  ans<-ldply(rows, function (a) lpdf%>% total(a,ID,a,varnames))
}
             
totals2<- function(lpdf, rows="", # used only for county to state totalling. 
                  ID="Country.Region", 
                  varnames=c("confirmed","deaths")
                      # "recovered", "active","new_confirmed","new_deaths","new_recovered"
                  ){
  if (rows[1]=="") rows=unique(lpdf[,ID])
  if (verbose>5) print(paste("Making totals2 for ",paste(rows,collapse=","),"in",ID))
  ans<-ddply(lpdf[lpdf[,ID] %in% rows,],c("Date", ID),
             function(a) {
          Country.Region=aggreg(as.character(a$Country.Region))
          Province.State= aggreg(a$Province.State)
          CRPS=ifelse(Province.State=="",Country.Region,paste(Province.State,Country.Region,sep=', '))
          b1<- colSums(a[,varnames],na.rm=TRUE) #this creates 0 for recovered in the US if included. 
          nam<- names(b1)
          dim(b1)<- c(1,length(b1))
          b2<-data.frame(b1)
          colnames(b2)<- nam
          cbind(Country.Region,
               CRPS,
               Province.State,
               Lat=mean(a$Lat) ,Long=mean(a$Long),
               b2
               )  }) 
  ans[,setdiff(names(lpdf),names(ans))]<-NA
  ans
}

correctnames<- function(df){
  names(df)[match("Long_",names(df))]<-"Long"
  names(df)[match("Province_State",names(df))]<-"Province.State"
  names(df)[match("Country_Region",names(df))]<-"Country.Region"
  df[,!names(df) %in% c("Admin2" ,"UID","iso2","iso3","code3","FIPS")]
}
makelpdfUSStates<- function(){
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
  if(all(lpdf$recovered==0)) lpdf$recovered<- as.numeric(NA )
  lpdf
}
  
makelpdfUS <- function() {
  lpdf<- makelpdfUSStates()
  lpdf$recovered<- NA 
  lpdf<- lpdf%>% totals2("","Province.State") # depends on all provinces being chosen to sum! 
  lpdf$recovered<- NA 
  lpdf$Combined_Key<-NULL
  lpdf 
}

makelpdf <- function() {
  wc<-readdata('confirmed') #"Confirmed")
  geo.location <- wc[c("Country.Region","Province.State","Lat","Long")]
  #write.csv(geo.location,file="geo.location.csv",na="")
  confirmed<- convertdata(wc,values.name="confirmed")
  wd<-readdata("deaths")
  deaths<- convertdata(wd,values.name="deaths")
  wr<-readdata("recovered")
  names(wr)[1]<-names(wc)[1] #"Province.State without strange characters BOM?
  recovered<- convertdata(wr,values.name="recovered")
  lpdf<- merge(confirmed,recovered,all.x=TRUE,#,sort=FALSE,
                  by=c("Country.Region","CRPS","Province.State","Date","Lat","Long"))
  lpdf<- merge(lpdf,deaths,all.x=TRUE,
                  by=c("Country.Region","CRPS","Province.State","Date","Lat","Long"))
  lpdf[lpdf$CRPS=="US",]$CRPS<- as.character("USA") #to distinguish from the detailed data
  #levels(lpdf$Country.Region) <- c(levels(lpdf$Country.Region),"USA")
  lpdf[lpdf$CRPS=="USA","Country.Region"] <- "USA" 
  lpdf
}

updateJHHFromWeb <- function(nameUS="JHH_US.csv",namenonUS="JHH_non_US.csv") {
  CUS0<- makelpdfUS()
  CUS<-as_tibble( CUS0) #was as.tibble and that worked, but there was a warning. 
  #write.csv(CUS,nameUS)
  Cworld0<-makelpdf() 
  Cworld<-  as_tibble(Cworld0)
  #write.csv(Cworld,namenonUS)
  lpdf<- rbind.data.frame(Cworld,CUS)#,StringsAsFactors=FALSE)
  #lpdf<- merge(Cworld,CUS, all=TRUE)
}

readLocalData<- function(nameUS="JHH_US.csv",namenonUS="JHH_non_US.csv"){
  #CUS<- read.csv(nameUS,stringsAsFactors=FALSE)#colClasses= ("Date"="character"))
  CUS2<- read_csv(nameUS)
  #Cworld<-read.csv(namenonUS,stringsAsFactors=FALSE)
  Cworld2<-read_csv(namenonUS)
  #lpdf<- rbind(Cworld,CUS)
  lpdf2<- rbind(Cworld2,CUS2)
  #as_tibble(lpdf)
}

sortIDlevels1<- function(lpdf,varname=confirmed,ondate=""){
  varname<- enquo(varname)
  if (ondate==""){ondate= max(lpdf$Date) } else 
    if (nrow(lpdf[lpdf$Date== ondate,]==0)){
      stop("Cannot sort on values of a date which is not present in the Data")}
  CRPSlevels<- lpdf  %>% select(c(CRPS,Date, !!varname)) %>% 
    filter(Date==ondate) %>% 
    arrange(-eval(parse(text=substitute(!!varname))),CRPS,.by_group = FALSE) 
    arrange(desc({{varname}}),CRPS,.by_group = FALSE) 
    #arrange(-(!!varname),CRPS,.by_group = FALSE) 
  lpdf<-lpdf%>%ungroup%>% mutate(CRPS= factor(lpdf$CRPS,levels=CRPSlevels$CRPS))%>%
    group_by(CRPS)
  lpdf$CRPS
} #the desc eval parse substitute  !!  should have been desc !! according to the manuals. but desc does not respect unquo. 

#lpdf=JHH

sortIDlevels<- function(lpdf,varname="confirmed",ID="CRPS",ondate=""){
  if (ondate==""){ondate= max(lpdf$Date) } else 
    if (nrow(lpdf[lpdf$Date== ondate,]==0)){
      stop("Cannot sort on values of a date which is not present in the Data")}
  ordre<- (lpdf[lpdf$Date== ondate,c(varname,ID)])      #as.data.frame not needed if select columns by [[]]
  levels<- ordre[order(-ordre[[varname]],ordre[[ID]]                 ),][[ID]]
               # added -             omitted        ,decreasing=TRUE 
  lpdf[[ID]]<- factor(lpdf[[ID]],levels=levels) #ordre[[ID]]
}
sortbyvar<- function(lpt,varname='confirmed',ID='CRPS',ondate=""){
lpt[[ID]] <- lpt%>% sortIDlevels(varname=varname, ID=ID,ondate=ondate) 
lpt<- lpt[order(lpt[[ID]],lpt[[varname]]),] 
}
#JHH %>% select(CRPS,Date,active)
#varnames=c( 'recovered','confirmed','active','net_active','new_confirmed')

#(JHH)%>% sortIDlevels(varnames[1])  %>%  head
#(JHH)%>% sortbyvar(varnames[1])  %>%  head
#(JHH)%>% sortbyvar(varnames[2])  %>%  head
#(JHH)%>% sortIDlevels(varnames[2])  %>%  head
#JHH%>% sortIDlevels(varnames[3])  %>% head
#JHH%>% sortIDlevels(varnames[4])  %>% levels()%>% head

makeJHH <- function(name="JHH",force=FALSE) {
  nameUS<- paste( paste(name,"US",sep="_"),           "csv",sep=".")
  namenonUS<- paste( paste(name,"non","US",sep="_"),  "csv",sep=".")
  namedays<- paste(name,"_days.csv",sep="")
  if(force|(difftime(Sys.time(), file.info(namedays)[,"mtime"], units = "hours")>6)) {
    lpdf<- updateJHHFromWeb(nameUS,namenonUS)
    if (verbose>=1) print("updating JHH from Github")
  } else {
    if (verbose>=1) print(paste("loading local",namedays))
    lpdf<- read.csv(namedays,stringsAsFactors = FALSE)
  }
  if (typeof(lpdf$Date)=="character") 
    lpdf$Date <- as.Date(lpdf$Date, "%Y-%m-%d")  #strptime gives timezones! no need for timezones
  if (verbose>0) {a=as.numeric(max(lpdf$Date)-min(lpdf$Date)+1)
    print(a% %"dates"%, % (nrow(lpdf)/a)% %"regions, last date:"% % 
            max(lpdf$Date)%, % "with" % %
            sum(is.na(lpdf[lpdf$Date>="2020-02-02",])) % %
            "missing values after 2020-02-01")}
  lpdf
}


######## make state groups, also useful in tableau

#SouthWestAsia<-c("South West Asia","Afganistan","Iran","Irak","Syria","Lebanon","Turkey","Israel", "West Bank and Gaza","Palestine")
#SouthEastAsia<- c("South East Asia","Indonesia","Thailand","Vietnam","Laos","Malaysia", "Cambodia", "Papua New Guinea","Myanmar", "Burma","Brunei","Philippines","Timor-Leste")
#SAsiaIO<-c ("South Asia & Indian Ocean","India","Pakistan","Bangladesh","Sri Lanka","Comoros", "Maldives","Madagascar","Mauritius", "Seychelles","Bhutan","Nepal","Mayotte","Reunion")
#EastAsia<- c("East Asia","Japan","Korea, South", "Korea, North","Taiwan*", "Hong Kong","Singapore","Mongolia")
#China<- c("China")


#Oceania<- c("Oceania","Australia","New Zealand","Vanuatu","Tuvalu", "Fiji","Guam","French Polynesia","New Caledonia"  )
#Benelux= c("Benelux","Belgium","Netherlands","Luxembourg")
#EU6<-c("EU6", Benelux[2:4], "Germany","France","Italy")
#EU<- c("EU",EU6[2:7],"Spain","Poland","Austria","Romania","Hungary","Ireland","Sweden","Denmark","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Cyprus")
#EFTA<-c("EFTA","Iceland","Liechtenstein","Switzerland","Norway")
#US<-c("US","USA")
#NAmerica<- c("North America",US,"Canada","Mexico","Saint Pierre and Miquelon")
regios<- list(EFTA=c("EFTA","Iceland","Liechtenstein","Switzerland","Norway"),
              Benelux=c("Benelux","Belgium","Netherlands","Luxembourg"),
              US=c("US","USA"),
              SouthWestAsia=c("South West Asia","Afganistan","Iran","Irak","Syria","Lebanon","Turkey","Israel", "West Bank and Gaza","Palestine"),
              SouthEastAsia=c("South East Asia","Indonesia","Thailand","Vietnam","Laos","Malaysia", "Cambodia", "Papua New Guinea","Myanmar", "Burma","Brunei","Philippines","Timor-Leste"),
              SAsiaIO=c ("South Asia & Indian Ocean","India","Pakistan","Bangladesh","Sri Lanka","Comoros", "Maldives","Madagascar","Mauritius", "Seychelles","Bhutan","Nepal","Mayotte","Reunion"),
              EastAsia= c("East Asia","Japan","Korea, South", "Korea, North","Taiwan*", "Hong Kong","Singapore","Mongolia"),
              CIS= c("CIS","Russia", "Belarus", "Armenia", "Azerbaijan","Kazakhstan","Kyrgyzstan","Turkmenistan","Tajikistan","Uzbekistan","Moldova"),
              China= c("China"))
regios<- c(list(EU6=c("EU6", regios$Benelux[2:4], "Germany","France","Italy"),
           Asia= setdiff(c("Asia regions", regios$SAsiaIO, regios$SouthEastAsia,
                           regios$SouthWestAsia,regios$EastAsia, regios$China,
                           regios$CIS),
                         c("Madagascar","East Asia","South Asia & Indian Ocean",
                           "South East Asia","South West Asia","CIS","Moldova",
                           "Belarus", "Georgia", "Azerbaijan","Armenia"))), 
           regios)
regios<- c(list(EU= c("EU",regios$EU6[2:7],"Spain","Poland","Austria","Romania","Hungary","Ireland","Sweden","Denmark","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Cyprus")),regios)
regios=c(list(MSM = c("MSM","Netherlands","Belgium","United Kingdom","Germany","Malta","Egypt","Suriname","China","Vietnam","Hungary","Romania","Kuwait","Italy","Ireland","Iran","Kazakstan","Liberia","Indonesia","Ethiopia","Nigeria","Ghana","Uganda","South Africa","Canada","Spain","France"),
            Vincent= c("Some Selected Regions","Belgium","Germany","Italy","France","Kazakhstan","Indonesia","Spain","Netherlands","Japan","New York"),
            continents= c("Continents","Europe","USA","US",'North America', "Africa","South America","Asia"),
            WestvsEast= c("WestvsEast","USA","UK","Italy","Iran","Korea","Germany","France","Spain","Sweden","Norway","Hubei","Belgium","Netherlands","Singapore","Japan","Taiwan*","Denmark","Hubei, China", "Hongkong, China", "Jiangsu, China"),
            Caribbean= c("Caribbean",'Anguilla',"Antigua and Barbuda","Bahamas" ,  "Barbados","Bermuda","Cayman Islands","Cuba","Dominica" ,"Dominican Republic","Grenada", "Haiti" , "Jamaica","Saint Kitts and Nevis" ,"Saint Vincent and the Grenadines","Saint Lucia"  ,"Trinidad and Tobago",'Aruba','Curacao',"Bonaire, Sint Eustatius and Saba","British Virgin Islands",'Guadeloupe','Martinique','Sint Maarten','St Martin','Saint Barthelemy','Turks and Caicos Islands','Montserrat'),            MENA=c("MENA", "Marocco","Algeria","Tunesia","Libia","Egypt", "West Bank and Gaza","Palestine","Lebanon","Syria","Turkey","Iraq","Iran","Afghanistan","Jordan","Saudi Arabia","Kuwait","Oman","United Arab Emirates","UAE","Yemen","Bahrain","Qatar"),
            SAmerica=c("South America countries","Argentina","Bolivia","Brazil","Chile","Colombia","Costa Rica","Honduras","El Salvador","Panama","Ecuador","Suriname","Guyana","Belize","Guatemala", "Antilles","Paraguay","Peru","Venezuela","Nicaragua" , "Uruguay","French Guiana","Falkland Islands (Malvinas)","Nicaragua"),
            Europe= c("Europe",regios$EU[2:28],regios$EFTA[2:5], "United Kingdom", "Russia", "Ukraine", "Belarus","Moldova","Georgia", "Armenia", "Azerbaijan","Andorra", "Monaco", "San Marino", "Vatican","Holy See", "Albania", "North Macedonia","Kosovo","Croatia","Montenegro","Bosnia and Herzegovina","Serbia","Gibraltar","Faroe Islands", "Isle of Man","Channel Islands","Greenland"),
            NAmerica= c("North America",regios$US,"Canada","Mexico","Saint Pierre and Miquelon"),
            Africa= c("Africa countries","Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon","Central African Republic","Chad","Comoros", "Congo (Kinshasa)", "Congo (Brazzaville)", "Cote d'Ivoire", "Djibouti", "Egypt",  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana","Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara","Zambia", "Zimbabwe"),
            Oceania= c("Oceania","Australia","New Zealand","Vanuatu","Tuvalu", "Fiji","Guam","French Polynesia","New Caledonia"  )
            ),regios)

### data from ECDC - World bank. 
### https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
makeecdc<- function (){
  ecdcdata <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na = "" ) %>%  #fileEncoding = "UTF-8-BOM" doesn use bom in readr tidyverse. 
    mutate( CRPS=countriesAndTerritories, 
            confirmed_today=cases, 
            deaths_today=deaths,
            Date= as.Date(dateRep, format="%d/%m/%Y"), 
            population= popData2018, popData2018=NULL) %>%
    select(-geoId, -day, -month, -year, -cases ,-countriesAndTerritories,-dateRep,
           -countryterritoryCode) %>%  
    arrange(CRPS,Date) %>% group_by(CRPS) %>% 
    mutate(confirmed = cumsum(confirmed_today),
           deaths = cumsum(deaths_today))%>%
    mutate(recovered=as.numeric(NA))
}

makeGroups <- function(lpdf=JHH,varname="Region",Regiolist="") {  
  if(!varname %in% names(lpdf)) lpdf[[varname]]<-as.character(NA)
  for (Regio in Regiolist) {
    region= Regio[1] %>% strsplit(" countries") %>% unlist %>% strsplit(" Provinces&States")%>% unlist
    lpdf[lpdf$Province.State %in% Regio,varname] <- region
    lpdf[lpdf$CRPS %in% Regio,varname]<- region
    }
  if (verbose>= 1){
    print("Regions added:"% % paste(unique(JHH$Region),collapse="/ "))
    print(paste("Not attributed regions: (add them to their regions in variable 'regios')"))
    print(unique(lpdf[is.na(lpdf$Region),c('CRPS','Province.State','Country.Region')]))
  }
  lpdf
}

makeDynRegions2 <- function(lpt=JHH,gridsize=7*6,piecename='World') {
  lpt<- lpt%>% ungroup %>% 
    filter( Date==max(Date))%>%
    select( CRPS, confirmed)%>% 
    arrange(desc(confirmed)) 
  nr=1
  lijst=vector(mode = "list", length =0)
  while (nrow(lpt)>gridsize){
    piece<-c(piecename% %nr,as.character(lpt[1:gridsize,]$CRPS ))
    lijst[[piece[1]]]<- piece
    lpt<- lpt%>% filter(row_number()>=gridsize+1)
    nrow(lpt)
    nr<- nr+1
    }
  piece<-c(piecename% %nr,as.character(lpt[1:nrow(lpt),]$CRPS))
  #lijst<- c(lijst,laatst=list(piece))
  lijst[[piece[1]]]=piece
  lijst
}

provincialize<- function(countries,lpdf=JHH){
  cl1<-unique(lpdf[lpdf$Country.Region %in% countries,]$CRPS)
  c(paste(countries[1],"Provinces&States"),setdiff(cl1,countries))
}

provincializeJHH<- function(){
  lpdf=JHH
  ChinaP<- provincialize(regios$China,lpdf)
  list(CanadaP=provincialize("Canada",lpdf),
       USS=provincialize(regios$US,lpdf), 
       NorthAmericaS=c(provincialize(regios$NAmerica,lpdf),"Mexico"), 
       OceaniaP=setdiff(c(provincialize(regios$Oceania,lpdf),regios$Oceania),
                        "Australia"),
       AsiaP= c(regios$Asia, (setdiff(ChinaP,"China Provinces&States"))),
       ChinaP=ChinaP
  )
}

makeregios<- function(lpt=JHH){
  regios =c(World="World",
    regios['continents'], 
    lpt%>% filter(!(CRPS %in% c("USA","US","Australia","China","Canada","South America","Asia","Africa","World","Europe")))%>% 
      makeDynRegions2(),
    lpt%>% filter(CRPS %in% regios$Europe &!(CRPS %in% c(regios$continents,"World")))%>% 
      makeDynRegions2(gridsize=20,piecename='Europe'), 
    lpt%>% filter(CRPS %in% c(regios$AsiaP,'Russia')) %>% makeDynRegions2(piecename='Asia'),
    lpt%>%filter(CRPS %in% (regios$NamericaS)) %>% makeDynRegions2(piecename='North America'), 
    regios[c('WestvsEast','SouthEastAsia','Africa','SAmerica', 'Caribbean','OceaniaP')] 
  ) 
}

addPopulation <- function(lpdf) {
  population<- read.csv('population.csv')[c(1,3)]
  names(population)[2]<- "population"
  rownames(population)<- population$Country.Name
  lpdf[,"population"]<- population[lpdf%>% pull(CRPS),"population"]
  popUS<- read.csv('USstatespop2019.csv')[c('State','p2019')]
  names(popUS)[2]<- "population"
  rownames(popUS)<- popUS$State
  lpdf[grepl(", US", lpdf$CRPS),"population"]<- 
    popUS[lpdf[grep(", US", lpdf$CRPS),]$Province.State,"population"]
  if (verbose>=1)print("population unknown: ")
  if (verbose>=1)print(unique(lpdf[is.na(lpdf$population),"CRPS"]))
  lpdf
}
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


imputeRecovered2<- function(lpdf=ecdcdata, lagrc=22,lagrd=16,
                            dothese=FALSE,redo=FALSE){
  varname="recovered"
  if(!('recovered' %in% names(lpdf))) lpdf$recovered<- as.numeric(NA)
  if(any(dothese)) lpdf[,varname%+% "_old"]<- lpdf[,varname]
  if (!"imputed"%in% names(lpdf))lpdf$imputed<-FALSE
  lpdf<- lpdf%>% mutate(recovered_imputed = min(confirmed-deaths,max(recovered,
      dplyr::lag(confirmed,lagrc)- dplyr::lag(deaths,lagrd) ,na.rm=TRUE),na.rm=TRUE)
      ) 
  rowstodo<- drop(is.na(lpdf$recovered)|dothese|(redo & lpdf$imputed))
  
  if (verbose>=2) print(paste("imputing recovered for:",
                 paste(unique(lpdf[rowstodo,][["Country.Region"]]),collapse=" / ")))
  if (sum(rowstodo)==0)return(lpdf)
  lpdf<- lpdf%>% group_by(CRPS) %>%
    mutate_cond(rowstodo,imputed=TRUE )%>% 
    mutate_cond(rowstodo, recovered= recovered_imputed)
  #bug disappeared: cannot find lagrc suddenly, as it is not part of the dataframe. only complains about it if i use the function on a data frame (JHH) not on ecdcdata. Why? 
}

frac<- function (n,d){ifelse(n!=0, n/d, NA)}
diff.sl<- function(avector,n=1){c(rep(NA,n),diff(avector,n))}

extravars2<- function(lpdf,lagrc=0,lagdc=0){
  lpdf<- lpdf %>%  ungroup %>% 
    arrange(CRPS, Date) %>%
    group_by(CRPS) %>% 
    mutate(active            =  confirmed - deaths - recovered,
           active_imputed    =  confirmed - deaths - recovered_imputed,
           new_confirmed     =   ma(diff.sl(confirmed)), 
           net_active        =   ma(diff.sl(active)),
           net_active_imputed=   ma(diff.sl(active_imputed)),
           new_recovered     =ma(diff.sl(recovered)), 
           new_recovered_imputed=ma(diff.sl(recovered_imputed)), 
           new_deaths        =   ma(diff.sl(deaths)),
           confirmed_pM      = 1000000*confirmed/population,
           active_pM         = 1000000* active  /population,
           recovered_pM      = 1000000*recovered/population,
           deaths_pM    = 1000000*deaths   /population,
           new_confirmed_pM = 1000000*new_confirmed/population,
           net_active_pM    = 1000000*net_active  /population,
           new_recovered_pM = 1000000*new_recovered/population,
           new_deaths_pM    = 1000000*new_deaths  /population,
           recovered_per_confirmed= frac(recovered,dplyr::lag(confirmed,lagrc)),
           deaths_per_confirmed= frac(deaths,dplyr::lag(confirmed,lagdc)),
           recovered_per_deaths= frac(recovered,dplyr::lag(deaths,lagrc-lagdc))              )
}

addtotals<- function(lpdf=JHH,ID='CRPS'){
  lpt<- lpdf %>%
    #just to be sure, that if i do it twice i dont get double counts. 
    #And omit USA as country, as we have the individual states already. 
    filter(! (!!ID %in% c("South America", "Asia", "Africa", "Europe","China","Australia","Canada","USA","US","World"))) 
  
  World<- unique(lpt[[ID]])
  varnames=c("confirmed","recovered", "deaths","population",'recovered_imputed',"active_imputed")
  rbind(lpdf, 
        lpt%>% total(World ,ID=ID,newrow="World", varnames= varnames)%>%
          extravars2,
        lpt%>% totals("US",ID="Country.Region", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Europe,ID=ID,newrow="Europe", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Africa,ID=ID,newrow="Africa", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Asia,ID=ID,newrow="Asia", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$SAmerica,ID=ID,newrow="South America",varnames= varnames)%>%
          extravars2,
        lpt%>% totals("China",  ID='Country.Region',varnames= varnames)%>%
          extravars2,
        lpt%>% totals("Australia", ID='Country.Region',varnames= varnames)%>%
          extravars2,
        lpt%>% totals("Canada",ID='Country.Region',varnames= varnames)%>%
          extravars2
  )# %>% sortbyvar('confirmed','CRPS')
}
############### used in graphit and for saving to csv
addcounterfrommin<-function(lpdf=JHH,minv=0,varname="confirmed",ID="CRPS",counter="day"){
  lpdf[,counter]<-as.numeric(NA)
  lpdf<- lpdf%>% filter(!is.na(!!varname))   #[!is.na(lpdf[,varname]),] #should not have any! effect for "confirmed" 
  if(sum(lpdf[,varname]>=minv)>0)  
  #lpdf<- lpdf%>% mutate_cond((!!varname)>=minv) %>% group_by(ID) %>%
   #       mutate(counter=seq_along(Date))  
    
   lpdf[lpdf[[varname]]>=minv,]<- 
         ddply(lpdf[lpdf[[varname]]>=minv,],ID, 
            function(lpdf){
              lpdf[[counter]]<- seq_along(lpdf$Date)
              lpdf}
            )
    #lpdf[lpdf[[varname]]>=minv,]
}

### make day vars for tableau & Excel
makecountname <- function(countname,minv){paste(countname,minv,sep="_")}

writewithcounters<- function(lpdf=JHH,varname="confirmed",ID="CRPS",name="JHH"){
    lpdf<- as.data.frame(lpdf)
      lpdf<- lpdf[!is.na(lpdf[c(varname)]),]
    for (minv in c(1,20,100,400,1000,2000,5000,10000)){
      lpdf<- addcounterfrommin(lpdf=lpdf, minv=minv, 
                               varname=varname,ID=ID,
                               counter=makecountname("day",minv))
    }
    filename=paste(name,"days.csv",sep="_")
    write.csv(lpdf,file=filename, na="")
    if (verbose>0) print(paste("Written the current data with counters to disk as",filename,"forr use in Tableau or Excel"))
}
#Next, prepare functions to select data we want to line graph, determined by the minimum value , date, and country/ID


dataprep<- function(lpdf=JHH, minval=1, ID="CRPS", 
                     xvar="day", yvars=c("confirmed", "recovered"), 
                     logx=FALSE, logy=TRUE, sorted=TRUE){
  if (!(xvar %in% names(lpdf))) 
    lpdf<- lpdf%>% addcounterfrommin(minval,varname="confirmed", ID=ID,counter=xvar)
  if (logy) 
    for (varname in yvars)  {
      if (sum((!is.na(lpdf[,varname]))&lpdf[,varname]<= 0)>0) 
        #if no NAs, one row replaces zero rows -> error!
        lpdf[(!is.na(lpdf[,varname]))&lpdf[,varname] <= 0,varname]<- NA 
    }
  if(logx) lpdf[lpdf[[xvar]]<=0,xvar]<- 1 
  if(sorted) {
    lpdf[[ID]] <- sortIDlevels(lpdf=lpdf,varname=yvars[1]) 
    lpdf<- lpdf[order(lpdf[[ID]],lpdf[[xvar]]),] 
  }
  lpdf<- lpdf[,c(xvar, ID, yvars)]# ,'Date'  ungroup %>% arrange(desc(CRPS),1)
  
}#Bug potential: after the sort, CRPS is a factor. before, it was  character!

#JHH%>% filter(CRPS %in% c('United Kingdom','Russia','France'))%>% dataprep( xvar='Date',yvars='active',sorted=FALSE) %>% pull(CRPS)%>% typeof()# %>% unique()
#JHH%>% filter(CRPS %in% c('United Kingdom','Russia','France'))%>%   dataprep( xvar='Day',yvars='active',sorted=TRUE) %>% pull(CRPS)%>% typeof()# %>% unique()
#JHH%>% filter(CRPS %in% c('United Kingdom','Russia','France'))%>%   dataprep(xvar='Day',yvars='recovered',sorted=FALSE)%>% typeof(.$CRPS) #pull(CRPS) %>% unique()
#graphit(JHH, c('Russia','France','United Kingdom'),facet='CRPS',yvars='active',sorted=TRUE)
#graphit(JHH, c('Russia','France','United Kingdom'),facet='CRPS',yvars='active',sorted=FALSE)

graphit <- function(lpdf, countries, minval=1, ID="CRPS", xvar="Date", 
                     yvars=c("active", "recovered","deaths","confirmed"), 
                     fuzzy=FALSE, logx=FALSE, logy=FALSE, 
                     myfolder="",savename="", putlegend=TRUE, size=1, returnID="CRPS", 
                     area=FALSE,position='stack',facet=FALSE, sorted=TRUE, until=Sys.Date()){
  lpdf<- as.data.frame(lpdf)
  if (typeof(until)=="character") until=as.Date(until,format="%Y-%m-%d")
  lastdate<- min(max(lpdf$Date),until)
  #if (length(countries)==0) {countries<- unique(lpdf[,returnID])
      #} else{ 
    countries<- lpdf%>% findIDnames(testIDnames=countries,searchID=ID,fuzzy=fuzzy,returnID=returnID) #}
  ID<- returnID
  lpdf <- lpdf%>% filter((confirmed>=minval)&(CRPS %in% countries)&Date<=until)  #%>%
  if (verbose>=6) print(paste("in graphit, until is",until,"and last date in dataset is",max(lpdf$Date)))
  if (nrow(lpdf)==0) {warning("no data -> no plot");    return()  }
  lpdf <- lpdf%>%
      dataprep(ID=ID,minval=minval, xvar=xvar,yvars=yvars,logx=logx,logy=logy, sorted=sorted)
  
  mytitle<- paste("Covid-19",format(lastdate,format="%Y%m%d"),
                  savename, paste(yvars,collapse=" & "),
                  "by",xvar,"for",paste(minval,"+",sep=""),"confirmed")
  
  if (nrow(lpdf)==0| all(is.na(lpdf[,xvar]))|all(is.na(lpdf[,yvars])))
    return(if (verbose>=5) print (paste(mytitle, "Too little data to graph. Maybe lower the mininum value, take more regions?")))
   lpdf<- lpdf%>% melt(lpdf ,id=c(ID,xvar),measure.vars=yvars,
                    variable.name="varname", value.name="count")%>%
       mutate ( mygroup=CRPS %, % varname,
                varname=factor(varname, levels = yvars))#
  if (verbose>=6) print(summary(lpdf))
  #lpdf<- dataprep2(lpdf=lpdf, ID=ID,  xvar=xvar, yvars=yvars, 
   #               variable.name="varname", value.name="count")
  if (facet=='varname') lpdf$mygroup<- lpdf[[ID]] 
  else if (facet==ID) lpdf$mygroup <- lpdf$varname
  if (verbose>=4) print( parse(text=substitute(xvar))% %"from"% % 
                           min(lpdf[,xvar])% % "to"% %max(lpdf[,xvar]) %, % 
                           "group by "% % lpdf$mygroup[1]%, %
                           "facets" % % facet)
  myplot<- ggplot(lpdf, aes_string(y="count",x=xvar,group='mygroup',
                              color= ifelse(length(unique(lpdf[,ID]))==1,  
                                            'varname' , 
                                            ifelse(facet==ID,'mygroup',ID))
                                   ),na.action=na.omit) 
  if(area){myplot<- myplot + geom_area(aes_string(color='mygroup',fill='mygroup'), position = position,alpha=.6)+scale_fill_manual(values = c("red", "green","black","orange"))
  }else {#lpdf$label <- ""
    #lpdf$label[lpdf$Date==max(lpdf$Date)] <- lpdf$mygroup
    myplot<- myplot+
      geom_line(alpha=0.3,size=size*0.7)+#,
                #aes_string(color= ifelse(length(unique(lpdf[,ID]))==1,  
                                      #'varname' , 
                                      #ifelse(facet==ID,'mygroup',ID))))+
      geom_point(size=size, aes_string(#color= ifelse(length(unique(lpdf[,ID]))==1,
                                        #'varname' , 
                                        #ifelse(facet==ID,'mygroup',ID)),
                          shape='varname'))+
    #geom_label_repel(aes_string(x=xvar,y="count",color=ifelse(facet==ID,'mygroup',ID),label='label')) #labels LAST DATE pointS!
    geom_dl(aes_string(x=xvar,y="count",#,color=ifelse(facet==ID,'mygroup',ID),
                        label='mygroup'),      
            method = list(dl.trans(x = x+0.1 ,y=y+0.1),"last.points", cex = 1.2))
  }  
  if (!isFALSE(facet)) {myplot<- myplot+ facet_wrap(as.formula(paste("~",facet)))}
  
  palette=ifelse (length(unique(lpdf$mygroup))<8, "Dark2","Paired") #Spectral Set2  
  if (length(unique(lpdf$varname))<=4 ) 
       myplot<- myplot + scale_shape_manual(values = c(0,1,3,2,4,5,6,7,8,9,10)) #shape="\u2620"
  if (length(unique(lpdf$mygroup))<=4){
      myscale<- scale_color_manual(values=c("red", "green", "black","orange",
                                            "pink","lavender","cyan","mauve"),
                                   guide= ifelse(putlegend,"legend",FALSE))
      }else if(length(unique(lpdf$mygroup))<13) {
               myscale<- scale_color_brewer(palette=palette)
    }  else myscale<- scale_color_discrete(guide = ifelse(putlegend,"legend",FALSE))
  
  if(xvar=="Date" ) myplot<- myplot+scale_x_date(labels = date_format("%d-%m"))
 
  myplot<-myplot + 
    ylab(paste(paste(yvars,collapse=" & "), ifelse(logy,"(log scale)","")))+
    xlab(paste(xvar,                       ifelse(logx,"(log scale)","")))+ 
    ggtitle(mytitle) +  theme_light() +  myscale 
  
  if(logy) myplot<- myplot+scale_y_continuous(trans='log10')
  if(logx) myplot<- myplot+scale_x_continuous(trans='log10')
  if (savename!="") {
    if (verbose>= 3) print("making Plot" % % myfolder %+% "/" %+% mytitle)
    myplot<- myplot+ theme(text=element_text(size=20)#,
                           #title=element_text(size=18),
                           #strip.text=element_text(size=20)
                           )
    mypath<- paste("G:/My Drive/Covid19_plots",lastdate,sep="/") 
    if(myfolder!="") mypath<- paste(mypath,myfolder,"",sep="/")
    if (!dir.exists(mypath)) dir.create(mypath,recursive=TRUE)
    png(filename=paste(mypath,#format(Sys.Date(),format="%Y%m%d"),"/",
                       mytitle, ifelse(logy,", log scale",""),
                       ".png",sep=""),
        width=1600,height=900)
    print(myplot);dev.off()
  }else return(myplot+theme(title = element_text(size = 11)))
}# 
# geom_point(shape="\u2620", size = 4)  #skulls
#geom_point(aes(shape=cyl, color=cyl, size=cyl))
#+scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))
 
graph_numbers<- function(){ 
  d1=tibble(d1=c(1,2,4),meaning=c('date','crd','new'))
  d2=tibble(d2=c(1,2,4),meaning=c('facet','imputed','per million'))
  d3=tibble(d3=c('a','l',NA),meaning=c('area','line','NA'))
  print(cbind(d1,d2,d3))
}
# first digit  1 for date, 2 for CRD ipv arcd, 4 for "new" , 
# second digit 1 for facet. 2 for imputed, 4 for per million



#00  a per day, line log, all in one graph (nearly the original)
graph00l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group, minval, ID,xvar='day',  logy=logy,
          yvars=c( 'active'), 
          myfolder="a by day all-in-one", 
          savename= ifelse(name=="","",paste(name,"all-in-one")),putlegend=TRUE,until=until)
}
#01 like 00 a+r+d+c+r_i by day line, log, but facets by ID
graph01l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",until=Sys.Date(),
                   logy=TRUE){
  lpdf%>%graphit(group, minval, ID,xvar='day', 
                 yvars=c('active','recovered','deaths' ,"confirmed",'recovered_imputed'), 
                 myfolder="a,r,d,c by day", logy=TRUE,
                 savename= name,facet=ID,putlegend=TRUE,until=until)
}
#02 like1 00 a+r+d+c line log, all in one graph but imputed
graph02l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group, minval, ID,xvar='day',  logy=logy,
                 yvars=c( 'active_imputed','recovered_imputed','deaths',"confirmed"), 
                 myfolder="a_i,r_i,d,c by day all-in-one", 
                 savename= ifelse(name=="","",paste(name,"all-in-one")),putlegend=TRUE,until=until)
}
#03 like 00,01,02 a+r+d+c by day line,log,facets by ID AND imputed
graph03l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS", until=Sys.Date(),
                   logy=TRUE){
  lpdf%>%graphit(group, minval, ID,xvar='day', 
                 yvars=c('active_imputed','recovered_imputed','deaths' ,"confirmed"), 
                 myfolder="a_i,r_i,d,c by day", logy=TRUE,
                 savename= name,facet=ID,putlegend=TRUE,until=until)
}
##05 a+r+d+c per Million by day line, log
graph05l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=TRUE, 
                   until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Day", myfolder="a,r,d,c per Million by day", 
               yvars=c('active_pM','recovered_pM','deaths_pM','confirmed_pM'), 
               logy=logy,savename= name,facet="CRPS",until=until)
}


#11 a+r+d by Date    areaplot    facet per ID
graph11a<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=FALSE
                  , until=Sys.Date()){
  lpdf%>%graphit(group,1,ID, xvar="Date",yvars=c('active','recovered','deaths'),
          myfolder="a,r,d by Date areaplot", area=TRUE,facet=ID,
          savename= ifelse(name=="","",paste(name,"areaplot")),until=until) 
}

#13 active_i+recovered_i+deaths     areaplot          facet per ID
graph13a<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=FALSE
                  , until=Sys.Date()){
  lpdf%>%graphit(group,1,ID, xvar="Date",yvars=c('active_imputed','recovered_imputed','deaths'), 
                 myfolder="a_i,r_i,d by Date areaplot", area=TRUE,facet=ID,
                 savename= ifelse(name=="","",paste(name,"areaplot")),until=until) 
}

##15 a+r+d+c  per Million line, log
graph15l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date", myfolder="a,r,d,c per Million by Date", 
          yvars=c('active_pM','recovered_pM','deaths_pM','confirmed_pM'), 
          logy=logy,savename= name,facet="CRPS",until=until)
}
#50 new a+r by Date line, logs, all-in-one
graph50l<- function(lpdf=JHH,group,name="",minval=1, ID="CRPS",logy=TRUE, 
                   until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar='Date',yvars=c('net_active','new_recovered'),
                 myfolder="new a,r by Date, all-in-one", logy=logy,  logx=FALSE,
                 savename=name,until=until)
}

#51a new a+r+d by date, area, facet by ID
graph51a<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=FALSE,
                    until=Sys.Date()){
  lpdf%>% graphit(group,minval,ID, xvar="Date",
                  yvars=c('net_active','new_recovered','new_deaths'), 
                  myfolder="new a,r,d by Date areaplot", logy=FALSE,
                  savename= ifelse(name=="",name,paste(name,"areaplot")),
                  facet=ID, area = TRUE, until=until) 
}

#51l new a+r+d+c line  by date, facets by ID
graph51l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=TRUE,
                    until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('net_active','new_recovered','new_deaths','new_confirmed','net_active_imputed','new_recovered_imputed'),  
                 myfolder="new a,r,d,c by Date", logy=TRUE,savename= name,facet=ID,until=until) 
}

#53 new a_i+r_i+d+c line  by date, facets by ID
graph53l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=TRUE,
                    until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('net_active_imputed','new_recovered_imputed','new_deaths','new_confirmed'),  
                 myfolder="new a_i,r_i,d,c by Date", logy=TRUE,savename= name,facet=ID,until=until) 
}


#53a new a_i+r_i+d by day, area, facet by ID
graph53a<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=FALSE,
                    until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('net_active_imputed','new_recovered_imputed','new_deaths'), 
                 myfolder="new a_i,r_i,d by Date areaplot", logy=FALSE,
                 savename= ifelse(name=="",name,paste(name,"areaplot, imputed")),
                 facet=ID, area = TRUE, until=until) 
}

#71 facet by ID, new confirmed, recovered, deaths by date, area,  no stacking
graph71a<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=FALSE,
                   until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('new_confirmed','new_recovered','new_deaths'), 
                 myfolder="new c,r,d areaplot", logy=FALSE,
                 savename= ifelse(name=="",name,paste(name,"areaplot")),
                 facet=ID,area = TRUE,position='identity',until=until) 
}

#73 new c+r_i+d by date, area, facet by ID, no stacking
graph73a<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=FALSE,
                   until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('new_confirmed','new_recovered_imputed','new_deaths'), 
                 myfolder="new c,r_i,d areaplot", logy=FALSE,
                 savename= ifelse(name=="",name,paste(name,"areaplot")),
                 facet=ID,area = TRUE,position='identity',until=until) 
}

#75 new c+r+d per Million by date line plot, facets by ID
graph75l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=TRUE,
                   until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('new_confirmed_pM','new_recovered_pM','new_deaths_pM'), 
                 myfolder="new c,r,d per Million by Date", logy=logy,
                 savename= name,facet="CRPS",until=until)
}

#80 r+d by conf nolog
graph80l<- function(lpdf=JHH,group,name="",minval=1, ID="CRPS",logy=FALSE, 
                  until=Sys.Date()){
  lpdf%>% graphit(group,minval,ID, xvar='confirmed',yvars=c('recovered','deaths'),
          myfolder="r,d by c", logy=logy,  logx=FALSE,savename=name,until=until)
}

#82 r_i+d by conf nolog
graph82l<- function(lpdf=JHH,group,name="",minval=1, ID="CRPS",logy=FALSE, 
                  until=Sys.Date()){
  lpdf%>% graphit(group,minval,ID, xvar='confirmed',yvars=c('recovered','deaths'),
                  myfolder="r_i,d by c", logy=logy,  logx=FALSE,savename=name,until=until)
}

#90 r by d all in one
graph90l<- function(lpdf=JHH,group,name="",minval=1, ID="CRPS",logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar='deaths',yvars=c('recovered'),
                 myfolder="r_i by d",logy=logy,logx=TRUE,savename=name,until=until)
}

#92 r_i by d all in one
graph92l<- function(lpdf=JHH,group,name="",minval=1, ID="CRPS",logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar='deaths',yvars=c('recovered_imputed'),
                 myfolder="r by d",logy=logy,logx=TRUE,savename=name,until=until)
}

#100l reco over conf by date line graph per ID
graph100l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=FALSE, 
                   until=Sys.Date()){
  lpdf%>%graphit(group,1,ID, xvar="Date",yvars=c('recovered_per_confirmed'), 
          myfolder="r per c", logy=logy,savename= name,putlegend=FALSE ,
          until=until)
}

#102l reco over conf by date line graph per ID
graph102l<- function(lpdf=JHH,group,name="",minval=100, ID="CRPS",logy=FALSE, 
                    until=Sys.Date()){
  lpdf%>%graphit(group,1,ID, xvar="Date",yvars=c('recovered_per_confirmed'), 
                 myfolder="r per c", logy=logy,savename= name,putlegend=FALSE ,
                 until=until)
}

graphnrs<- ls(pattern="graph[[:digit:]]")%>% gsub("graph","", .) 
#graphnrs
#rm(list=ls(pattern="graph[[:digit:]]"))

graphs<- function(lpdf=JHH,group,name="",minval=100,nrs=graphnrs, ID="CRPS", 
                  until=Sys.Date()){
  for (j in nrs)
      do.call ("graph"%+%j,args=list(lpdf,group,name,minval, ID,until=until))
}


#this one does it by group of countries first. when all graphs are in one folder, this makes it not easy to compare between groups. 
#this one does the graphs per type, then per countrygroup. 
#given each graph goes into its own type's folder, it doesn t matter. 
writeRegiograph<- function(lpdf=JHH,regions,minval=100,nrs=graphnrs, ID="CRPS", until=Sys.Date()){
  pars <- as.list(match.call()[-1]) # the paremeters. 
  if (typeof(regions)=="character") {
    regions=list(c(as.character(pars$regions),regions))
    print(as.character(pars$lpdf))
    return()}
  for (j in nrs)
    {if(verbose>=2) {ti=Sys.time(); print(paste(ti,"Graph ",j))}
    for (i in 1:(length(regions))){
      IDs<-findIDnames(lpdf=lpdf, testIDnames=regions[[i]],searchID=ID, fuzzy=FALSE,returnID="CRPS")
      if(verbose>=3) print(paste(Sys.time(),paste(IDs,collapse="/ ")))
      do.call ("graph"%+%j,
               args=list(lpdf,IDs,regions[[i]][1],minval, ID, until=until))
    }
    if(verbose>=2) {print (paste("duration: ",difftime(Sys.time(),ti,units='mins')))}
  }
}

makeDate<- function(chardate="",format=myDateFormat){
  tryCatch(as.Date(chardate, format=format),
           error=function(e){print(paste("Either enter a date or a string (please use the following Date format for the string:",format ))})
}

makehistory<- function(lpdf=JHH,regions=JHHRegios,ID='CRPS', 
                       dates =as.Date(max(JHH$Date), format=myDateFormat),
                       nrs=graphnrs
                        ){
  if (typeof(dates)=="character") {  makeDate(dates)}
  if(any(is.na(dates))) print(paste("Not all dates recognized: ",paste(dates,collapse=","),". Either enter an R date or a string (please use the following Date format for the string:",format ))
  options(warn=-1)
  for (until in dates ){
    if(verbose>=1) {ti1=Sys.time(); print(paste(ti1, "doing", as.Date(until,origin="1970-01-01")))}
    if(nrow(lpdf[lpdf$Date<=until,])>0) 
    {#lpdf%>% writeRegiograph(regions,nrs=nrs,until=until) 
      for (i in 1:(length(regions))){
        ti2=Sys.time()
        IDs<-findIDnames(lpdf=lpdf, testIDnames=regions[[i]],searchID=ID, fuzzy=FALSE,returnID="CRPS")
        if(verbose>=3) print(paste(Sys.time(),regions[[i]][1]%: % paste(IDs,collapse="/ ")))
        lpdf%>% 
          graphs(IDs,name=regions[[i]][1],minval=100, ID=ID, until=until)
        if(verbose>=2) {print (paste("duration: ",difftime(Sys.time(),ti2,units='mins')))}
        }
    }
    else print("no data")
    if(verbose>=1) {print (paste("duration for ",Sys.Date(),": ",difftime(Sys.time(),ti1,units='mins')))}
    while(!is.null(dev.list())) dev.off() 
  }
  options(warn=0)
}




# check th lags between recovered, confirmed and deaths 
ccf.vf<- function(var1=c(1,2), var2=c(2,2),lag.max=30,saveit=FALSE, plotit=FALSE,printit=FALSE){
  title=paste("ccf of ",var1  , " vs ",var2,".png", sep="")
  if (saveit) {png(filename=paste("plots/ccf/",title))}
  myplot=ccf(var1,var2,lag.max=lag.max,main= title, plot=plotit,na.action=na.omit)
  if (printit) print(myplot)
  if (saveit) {dev.off()} #else {myplot}
  myplot
  }

findMaxCCF<- function(var1="new_recovered",var2="new_confirmed",myCRPS="Hubei, China", lpdf=JHH,N=5){
  if (myCRPS!="") lpdf<- lpdf[lpdf$CRPS==myCRPS,]
  lpdf<- lpdf[lpdf$Date>"2020-01-22",c("Date", var1,var2)]
  if(all(is.na(lpdf[,var1]))|all(is.na(lpdf[,var2]))) 
    return(data.frame( cor=NA,lag=NA)) #
  d <- ccf.vf(lpdf[,var1],lpdf[,var2],lag.max=30, plot = FALSE)
  if (verbose>=2) print (myCRPS)
  res = data.frame( cor=d$acf[,,1],lag=d$lag[,,1])
  if (N%%2==0)N=N-1
  a<-res[order(res$cor, decreasing=TRUE)[1:N],]
  if (verbose>=3) print(a)
  res_max = median( a$lag) #which.max(res$cor) #instead of max, take the n largest: order(R, decreasing=TRUE)[1:N]
  return(res[res$lag==res_max,])  
} 

findMaxCCFs<- function(var1="new_recovered",var2="new_confirmed", myCRPS="", lpdf=JHH){
  a<- ddply( lpdf, "CRPS", function (lpdfp){findMaxCCF(var1=var1,var2=var2,myCRPS= myCRPS, lpdf=lpdfp)})
  a[!is.na(a$lag),]
}


#end. Now run loadData.R
