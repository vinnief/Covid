source("requirements.R")

if(!exists("verbose")) verbose=1

myDateFormat<- "%Y-%m-%d"

datapath='./data'
if (!dir.exists(datapath)) dir.create(datapath,recursive=TRUE)


#*Note*: the data of John hopkins, 
#git@github.com:CSSEGISandData/COVID-19.git
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
makePSCR <- function(PS,CR){PS%,%CR}

convertdata <-function(wpdf,coltype="date", values.name="count",US=FALSE){ 
  ID<-c("Country.Region","PSCR","Province.State","Lat","Long")
  if (US) { ID<-c(ID,"Combined_Key")   }
  wpdf$PSCR <- (ifelse(""==wpdf$Province.State, 
                      as.character(wpdf$Country.Region),
                      makePSCR(wpdf$Province.State , wpdf$Country.Region) ))
  lpdf<-reshape2::melt(wpdf,id=ID,
                       variable.name=coltype, value.name=values.name)  
  #lpdf<- wpdf %>% pivot_longer(????)
  lpdf$Date<- as.Date(paste(lpdf[,coltype],"20",sep=""),format="X%m.%d.%Y") 
  lpdf$date<- NULL
  return(lpdf)
} 

findIDnames <- function(lpdf=JHH, testIDnames=c("Neth","India"), searchID="PSCR",
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


total<- function(lpdf=JHH, rows="", 
                 ID="PSCR" ,
                 varnames=c("confirmed","deaths","recovered") ,
                 newrow=""
                 ) {
  if (rows[1]=="") rows=unique(lpdf[[ID]])    
  ans<- ddply(lpdf[lpdf[[ID]] %in% rows,],c("Date"),
    function(df) {
      Country.Region=ifelse(newrow!="", newrow,
                            aggreg(as.character(df$Country.Region)))
      Province.State=ifelse(newrow!="",newrow,aggreg(df$Province.State))
      PSCR=ifelse(newrow!="",
                  newrow,
                  ifelse(ID=="PSCR",
                         aggreg(df$PSCR) ,
                         ifelse(Province.State=="",
                                Country.Region,
                                makePSCR(Province.State,Country.Region)
                                )
                         )
                  )
      b1<- colSums(df[,varnames],na.rm=TRUE)
      nam<- names(b1)
      dim(b1)<- c(1,length(b1))
      b2<-data.frame(b1)#as_tibble_row()
      colnames(b2)<- nam
      ans<- cbind(Country.Region,
                  PSCR,
                  Province.State,
                  Lat=mean(df$Lat) ,Long=mean(df$Long),
                  b2
                  )
      if("population"%in% names(df)) ans$population<- sum(df$population)
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
  if (verbose>=3) print(paste("Making the total for ",paste(rows,collapse="/ "),"in",ID))
  ans<-ldply(rows, function (a) lpdf%>% total(a,ID,varnames,a))
}
             
totals2<- function(lpdf, rows="", # used only for county to state totalling. 
                  ID="Country.Region", 
                  varnames=c("confirmed","deaths")
                      # "recovered"?
                  ){
  if (rows[1]=="") rows=unique(lpdf[,ID])
  if (verbose>5) print(paste("Making totals2 for ",paste(rows,collapse=","),"in",ID))
  ans<-ddply(lpdf[lpdf[,ID] %in% rows,],c("Date", ID),
             function(a) {
          Country.Region=aggreg(as.character(a$Country.Region))
          Province.State= aggreg(a$Province.State)
          PSCR=ifelse(Province.State=="",Country.Region,makePSCR(Province.State,Country.Region))
          b1<- colSums(a[,varnames],na.rm=TRUE) #this creates 0 for recovered in the US if included. 
          nam<- names(b1)
          dim(b1)<- c(1,length(b1))
          b2<-data.frame(b1)
          colnames(b2)<- nam
          cbind(Country.Region,
               PSCR,
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
  write.csv( geo.location, file=datapath%+%"/"%+%"geo.location.US.csv",na="")
  rm(geo.location)
  #  wc<-wc[,!names(wc) %in% c("Admin2" ,"UID","iso2","iso3","code3","FIPS")]
  confirmed<- convertdata(wc,values.name="confirmed",US=TRUE)
  wd<-readUSdata("deaths")
  wd<- correctnames(wd)
  #wd<-wd[,!names(wd) %in% c("Admin2" ,"UID","iso2","iso3","code3","FIPS")]
  deaths<- convertdata(wd,values.name="deaths",US=TRUE)
  lpdf<- merge(confirmed,deaths,all.x=TRUE,
               by=c("Country.Region","Province.State","PSCR","Combined_Key","Lat","Long","Date"))
  #tryCatch( wr<-readUSdata("recovered"),error=function(e){print(e)})
  #if ("wr" %in% ls()){
  #  wr<- correctnames(wr)
  #  recovered<- convertdata(wr,values.name="recovered")
  #  lpdf<-merge(lpdf,recovered,all=TRUE,
  #    by=c("Country.Region","Province.State","PSCR","Combined_Key","Lat","Long","Date"))
  #}else 
  lpdf$recovered<- as.numeric(NA ) #just in case NA totalled into 0. 
  lpdf
}
  
makelpdfUS <- function() {
  lpdf<- makelpdfUSStates()
  lpdf<- lpdf%>% totals2("","Province.State") # depends on all provinces being chosen to sum! 
  lpdf$Combined_Key<-NULL
  if(!all(is.na(lpdf$recovered)))
    if( max(lpdf$recovered, na.rm=TRUE)<=0) lpdf$recovered<- as.numeric(NA ) #just in case NA totalled into 0. 
  lpdf 
}

makelpdf <- function() {
  wc<-readdata('confirmed') #"Confirmed")
  geo.location <- wc[c("Country.Region","Province.State","Lat","Long")]
  #write.csv( geo.location,file=datapath%+%"/"%+% "geo.location.csv",na="")
  confirmed<- convertdata(wc,values.name="confirmed")
  wd<-readdata("deaths")
  deaths<- convertdata(wd,values.name="deaths")
  wr<-readdata("recovered")
  names(wr)[1]<-names(wc)[1] #"Province.State without strange characters BOM?
  recovered<- convertdata(wr,values.name="recovered")
  lpdf<- merge(confirmed,recovered,all.x=TRUE,#,sort=FALSE,
                  by=c("Country.Region","PSCR","Province.State","Date","Lat","Long"))
  lpdf<- merge(lpdf,deaths,all.x=TRUE,
                  by=c("Country.Region","PSCR","Province.State","Date","Lat","Long"))
  lpdf[lpdf$PSCR=="US",]$PSCR<- as.character("USA") #to distinguish from the detailed data
  #levels(lpdf$Country.Region) <- c(levels(lpdf$Country.Region),"USA")
  lpdf[lpdf$PSCR=="USA","Country.Region"] <- "USA" 
  lpdf
}

updateJHHFromWeb <- function(nameUS="JHH_US.csv",namenonUS="JHH_non_US.csv") {
  CUS0<- makelpdfUS()
  CUS<-as_tibble( CUS0) 
  #write.csv(CUS,datapath%+%"/"%+%nameUS)
  Cworld0<-makelpdf() 
  Cworld<-  as_tibble(Cworld0)
  #write.csv(Cworld,datapath%+%"/"%+%namenonUS)
  lpdf<- rbind.data.frame(Cworld,CUS)#,StringsAsFactors=FALSE)
  #lpdf<- merge(Cworld,CUS, all=TRUE)
}

readLocalData<- function(nameUS="JHH_US.csv",namenonUS="JHH_non_US.csv"){
  #CUS<- read.csv(datapath%+%"/"%+% nameUS,stringsAsFactors=FALSE)#colClasses= ("Date"="character"))
  CUS2<- read_csv(nameUS)
  #Cworld<-read.csv(datapath%+%"/"%+% namenonUS,stringsAsFactors=FALSE)
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
  PSCRlevels<- lpdf  %>% select(c(PSCR,Date, !!varname)) %>% 
    filter(Date==ondate) %>% 
    arrange(-eval(parse(text=substitute(!!varname))),PSCR,.by_group = FALSE) 
    arrange(desc({{varname}}),PSCR,.by_group = FALSE) 
    #arrange(-(!!varname),PSCR,.by_group = FALSE) 
  lpdf<-lpdf%>%ungroup%>% mutate(PSCR= factor(lpdf$PSCR,levels=PSCRlevels$PSCR))%>%
    group_by(PSCR)
  lpdf$PSCR
} #the desc eval parse substitute  !!  should have been desc !! according to the manuals. but desc does not respect unquo. 

#lpdf=JHH

sortIDlevels<- function(lpdf,varname="confirmed",ID="PSCR",ondate=""){
  if (ondate==""){ondate= max(lpdf$Date) } else 
    if (nrow(lpdf[lpdf$Date== ondate,]==0)){
      stop("Cannot sort on values of a date which is not present in the Data")}
  ordre<- (lpdf[lpdf$Date== ondate,c(varname,ID)])      #as.data.frame not needed if select columns by [[]]
  levels<- ordre[order(-ordre[[varname]],ordre[[ID]]                 ),][[ID]]
               # added -             omitted        ,decreasing=TRUE 
  lpdf[[ID]]<- factor(lpdf[[ID]],levels=levels) #ordre[[ID]]
}
sortbyvar<- function(lpt,varname='confirmed',ID='PSCR',ondate=""){
lpt[[ID]] <- lpt%>% sortIDlevels(varname=varname, ID=ID,ondate=ondate) 
lpt<- lpt[order(lpt[[ID]],lpt[[varname]]),] #bUG? WHY SORT AGAIN BY same var?  
}
#JHH%>% sortbyvar('confirmed')
#JHH %>% select(PSCR,Date,active)
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
    lpdf<- read.csv(datapath%+%"/"%+% namedays,stringsAsFactors = FALSE)
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
regios<- c(list(EU= c("EU",regios$EU6[2:7],"Ireland","Denmark","Greece","Spain","Portugal","Austria","Sweden","Finland","Poland","Hungary","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Cyprus","Romania","Bulgaria","Croatia")),
           regios)

regios=c(list(
  other=c("Other", 'Diamond Princess','MS Zaandam','World'),
  MSM = c("MSM","Netherlands","Belgium","United Kingdom","Germany","Malta","Egypt","Suriname","China","Vietnam","Hungary","Romania","Kuwait","Italy","Ireland","Iran","Kazakstan","Liberia","Indonesia","Ethiopia","Nigeria","Ghana","Uganda","South Africa","Canada","Spain","France"),
  Vincent= c("Some Selected Regions","Belgium","Germany","Italy","France","Kazakhstan","Indonesia","Spain","Netherlands","Japan","New York"),
  continents= c("Continents","Europe","USA","US",'North America', "Africa","South America","Asia"),
  WestvsEast= c("WestvsEast","USA","United Kingdom","Italy","Iran","Korea, South","Germany","France","Spain","Sweden","Norway","Belgium","Netherlands","Singapore","Japan","Taiwan*","Denmark","Hubei,China", "Hongkong,China", "Jiangsu,China", 'Indonesia'),
  Caribbean= c("Caribbean",'Anguilla',"Antigua and Barbuda","Bahamas" ,  "Barbados","Bermuda","Cayman Islands","Cuba","Dominica" ,"Dominican Republic","Grenada", "Haiti" , "Jamaica","Saint Kitts and Nevis" ,"Saint Vincent and the Grenadines","Saint Lucia"  ,"Trinidad and Tobago",'Aruba','Curacao',"Bonaire, Sint Eustatius and Saba","British Virgin Islands",'Guadeloupe','Martinique','Sint Maarten','St Martin','Saint Barthelemy','Turks and Caicos Islands','Montserrat'), 
  MENA=c("MENA", "Marocco","Algeria","Tunesia","Libia","Egypt", "West Bank and Gaza","Palestine","Lebanon","Syria","Turkey","Iraq","Iran","Afghanistan","Jordan","Saudi Arabia","Kuwait","Oman","United Arab Emirates","UAE","Yemen","Bahrain","Qatar"),
  SAmerica=c("South America countries","Argentina","Bolivia","Brazil","Chile","Colombia","Costa Rica","Honduras","El Salvador","Panama","Ecuador","Suriname","Guyana","Belize","Guatemala", "Antilles","Paraguay","Peru","Venezuela","Nicaragua" , "Uruguay","French Guiana","Falkland Islands (Malvinas)","Nicaragua"),
  Europe= c("Europe",regios$EU[2:28],regios$EFTA[2:5], "United Kingdom", "Russia", "Ukraine", "Belarus","Moldova","Georgia", "Armenia", "Azerbaijan","Andorra", "Monaco", "San Marino", "Vatican","Holy See", "Albania", "North Macedonia","Kosovo","Croatia","Montenegro","Bosnia and Herzegovina","Serbia","Gibraltar","Faroe Islands", "Isle of Man","Channel Islands","Greenland"),
  NAmerica= c("North America",regios$US,"Canada","Mexico","Saint Pierre and Miquelon"),
  Africa= c("Africa countries","Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon","Central African Republic","Chad","Comoros", "Congo (Kinshasa)", "Congo (Brazzaville)", "Cote d'Ivoire", "Djibouti", "Egypt",  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana","Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara","Zambia", "Zimbabwe"),
    Oceania= c("Oceania","Australia","New Zealand","Vanuatu","Tuvalu", "Fiji","Guam","French Polynesia","New Caledonia"  )
            ),
         regios)

### data from ECDC - World bank. 
### https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
makeECDC<- function (){
  lpt <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na = "" ) %>%  #fileEncoding = "UTF-8-BOM" doesn use bom in readr tidyverse. 
    mutate( PSCR=countriesAndTerritories, 
            confirmed_today=cases, 
            deaths_today=deaths,
            Date= as.Date(dateRep, format="%d/%m/%Y"), 
            population= popData2018, popData2018=NULL,
            Region= continentExp) %>%
    select(-geoId, -day, -month, -year, -cases ,-countriesAndTerritories,
           -dateRep,-continentExp,-countryterritoryCode) %>%  
    arrange(PSCR,Date) %>% group_by(PSCR) %>% 
    mutate(confirmed = cumsum(confirmed_today),
           deaths = cumsum(deaths_today))%>%
    mutate(recovered=as.numeric(NA))
  if (verbose>0) {a=as.numeric(max(lpt$Date)-min(lpt$Date)+1)
    print(a% %"dates"%, % (nrow(lpt)/a)% %"regions, last date:"% % 
          max(lpt$Date)%, % "with" % %
          sum(is.na(lpt[lpt$Date>="2020-02-02",])) % %
          "missing values after 2020-02-01")}
  lpt
}

makeGroups <- function(lpdf=JHH,varname="Region",Regiolist="") {  
  if(!varname %in% names(lpdf)) lpdf[[varname]]<-as.character(NA)
  for (Regio in Regiolist) {
    region= Regio[1] %>% strsplit(" countries") %>% unlist %>% strsplit(" Provinces&States")%>% unlist
    lpdf[lpdf$Province.State %in% Regio,varname] <- region
    lpdf[lpdf$PSCR %in% Regio,varname]<- region
    }
  if (verbose>= 1){
    print("Regions added:"% % paste(unique(JHH$Region),collapse=", "))
    if(sum(is.na(lpdf$Region))>0){
      print(paste("Not attributed regions:") % %
      paste(unique(lpdf[is.na(lpdf$Region),]$PSCR),collapse="; "))
    }
  }
  lpdf
}

makeDynRegions2 <- function(lpt=JHH,gridsize=5*6,piecename='World') {
  lpt<- lpt%>% ungroup %>% 
    filter( Date==max(Date))%>%
    select( PSCR, confirmed)%>% 
    arrange(desc(confirmed)) 
  nr=1
  mylist=vector(mode = "list", length =0)
  while (nrow(lpt)>gridsize){
    piece<-c(piecename%+%nr,as.character(lpt[1:gridsize,]$PSCR ))
    mylist[[piece[1]]]<- piece
    lpt<- lpt%>% filter(row_number()>=gridsize+1)
    nrow(lpt)
    nr<- nr+1
    }
  piece<-c(piecename%+%nr,as.character(lpt[1:nrow(lpt),]$PSCR))
  mylist[[piece[1]]]=piece
  mylist
}

provincialize<- function(countries,lpdf=JHH){
  cl1<-unique(lpdf[lpdf$Country.Region %in% countries,]$PSCR)
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

makeregios<- function(lpt=JHH,piecename="JHH"){
  regios =c(World="World",
    regios['continents'], 
    lpt%>% filter(!(PSCR %in% c("USA","US","Australia","China","Canada","South America","Asia","Africa","World","Europe")))%>% 
      makeDynRegions2(piecename=piecename% %'World'),
    lpt%>% filter(PSCR %in% regios$Europe &!(PSCR %in% c(regios$continents,"World")))%>% 
      makeDynRegions2(gridsize=20,piecename=piecename% %'Europe'), 
    lpt%>% filter(PSCR %in% c(regios$AsiaP,'Russia')) %>% makeDynRegions2(piecename=piecename% %'Asia'),
    lpt%>%filter(PSCR %in% (regios$NorthAmericaS)) %>% makeDynRegions2(piecename='North America'), 
    regios[c('WestvsEast','Africa','SAmerica', 'Caribbean','OceaniaP')] 
  ) 
}

addPopulation <- function(lpdf) {
  population<- read.csv(datapath%+%"/"%+% 'population.csv')[c(1,3)]
  names(population)[2]<- "population"
  rownames(population)<- population$Country.Name
  lpdf[,"population"]<- population[lpdf%>% pull(PSCR),"population"]
  popUS<- read.csv(datapath%+%"/"%+% 'USstatespop2019.csv')[c('State','p2019')]
  names(popUS)[2]<- "population"
  rownames(popUS)<- popUS$State
  lpdf[grepl(",US", lpdf$PSCR),"population"]<- 
    popUS[lpdf[grep(",US", lpdf$PSCR),]$Province.State,"population"]
  if (verbose>=1)print("population unknown:" % % 
        paste(unique(lpdf[is.na(lpdf$population),]$PSCR),collapse="; "))
  lpdf
}
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


imputeRecovered2<- function(lpdf=ECDCdata, lagrc=42,lagrd=36, # was 22, 16
                            dothese=FALSE,correct=FALSE){
  varname="recovered"
  if(!('recovered' %in% names(lpdf))) lpdf$recovered<- as.numeric(NA)
  #if(any(dothese)) lpdf[,varname%+% "_old"]<- lpdf[,varname]
  if (!"imputed"%in% names(lpdf))lpdf$imputed<-FALSE
  lpdf<- lpdf%>% group_by(PSCR) %>%
    mutate(recovered_imputed = 
              pmin(confirmed-deaths,
                   pmax(0,recovered, 
                        lag(confirmed,lagrc)- lag(deaths,lagrd),
                        na.rm=TRUE),
                   na.rm=FALSE)
                      ) 
  rowstobecorrected= correct & ( lpdf$recovered < lpdf$recovered_imputed )
  rowstodo<- is.na(lpdf$recovered)|dothese| rowstobecorrected
  #  %>% drop() #no need: is vector already
  if (verbose>=2)print("imputing recovered for:"% %
                        length(unique(lpdf[rowstodo,][["PSCR"]]))
                       % % 'Regions.')
  if (verbose>=3) print(
        paste(unique(lpdf[rowstodo,][["PSCR"]]),collapse="; "))
  if (sum(rowstodo)==0) return(lpdf)
  lpdf<- lpdf%>% group_by(PSCR) %>%
    mutate_cond(rowstodo,imputed=TRUE )#%>%     mutate_cond(rowstodo, recovered= recovered_imputed)
}

frac<- function (n,d){ifelse(n!=0, n/d, NA)}
diff.sl<- function(avector,n=1){c(rep(NA,n),diff(avector,n))}
p_M<- function(a,b)1e6*a/b
extravars2<- function(lpdf,lagrc=0,lagdc=0){
  options(warn=-1)
  lpdf<- lpdf %>%  ungroup %>% 
    arrange(PSCR, Date) %>%
    group_by(PSCR) %>% 
    mutate(  active          =   confirmed - deaths - recovered,
           active_imputed    =   confirmed - deaths - recovered_imputed,
           NAs               =   NA,
           new_confirmed     =   mac(diff.sl(confirmed)), 
           net_active        =   mac(diff.sl(active)),
           net_active_imputed=   mac(diff.sl(active_imputed)),
           new_recovered     =   mac(diff.sl(recovered)), 
           new_recovered_imputed=mac(diff.sl(recovered_imputed)), 
           new_deaths        =   mac(diff.sl(deaths)),
           confirmed_p_M      = p_M(confirmed,population),
           active_p_M   = p_M(active  ,population),
           active_imputed_p_M         = p_M(active_imputed  ,population),
           recovered_p_M      = p_M(recovered,population),
           recovered_imputed_p_M      = p_M(recovered_imputed,population),
           deaths_p_M         = p_M(deaths   ,population),
           new_confirmed_p_M = p_M(new_confirmed,population),
           net_active_p_M    = p_M(net_active   ,population),
           new_recovered_p_M = p_M(new_recovered,population),
           net_active_imputed_p_M    = p_M(net_active_imputed   ,population),
           new_recovered_imputed_p_M = p_M(new_recovered_imputed,population),
           new_deaths_p_M    = p_M(new_deaths  ,population),
           recovered_per_confirmed= frac(recovered,dplyr::lag(confirmed,lagrc)),
           recovered_imputed_per_confirmed= 
             frac(recovered_imputed,dplyr::lag(confirmed,lagrc)),
           deaths_per_confirmed= frac(deaths,dplyr::lag(confirmed,lagdc)),
           recovered_per_deaths= frac(recovered,dplyr::lag(deaths,lagrc-lagdc)),
           recovered_imputed_per_deaths= 
             frac(recovered_imputed,dplyr::lag(deaths,lagrc-lagdc))   )
}


addtotals2<- function(lpdf=JHH,ID='PSCR'){
  lpt<- lpdf %>%
    #just to be sure, that if i do it twice i dont get double counts. 
    #And omit USA as country, as we have the individual states already. 
    filter(! (!!ID %in% c("South America", "Asia", "Africa", "Europe","China","Australia","Canada","USA","US","World"))) 
  
  World<- unique(lpt[[ID]])
  varnames=c("confirmed","recovered", "deaths","population")
       #,'recovered_imputed',"active_imputed") # we impute AFTER this function so no need. 
  rbind(lpdf, 
        lpt%>% total(World ,ID=ID,newrow="World", varnames= varnames),
        lpt%>% total(regios$Europe,ID=ID,newrow="Europe", varnames= varnames),
        lpt%>% total(regios$Africa,ID=ID,newrow="Africa", varnames= varnames),
        lpt%>% total(regios$Asia,ID=ID,newrow="Asia", varnames= varnames),
        lpt%>% total(regios$SAmerica,ID=ID,newrow="South America",varnames= varnames),
        lpt%>% totals(c("US","China","Australia", "Canada"),
                      ID="Country.Region", varnames= varnames)
        ) 
}
addtotals3<- function(lpt=ECDC,totregions="", ID='Region'){
  if (totregions[1]=="") totregions<- c("World",unique(lpt$Region))
  lpt1<- lpt %>%
    #just to be sure, that if i do it twice i dont get double counts. 
    #And omit USA as country, as we have the individual states already. 
    filter(! (!!ID %in% totregions)) 
  World<- unique(lpt1[[ID]])
  varnames=c("confirmed","recovered", "deaths","population")
  #,'recovered_imputed',"active_imputed") # we impute AFTER this function so no need. 
  for (regio in totregions[!is.na(totregions)]) 
    lpt<- rbind(lpt, 
              lpt1%>% total(regio ,ID=ID, varnames= varnames,newrow=regio))
}




############### used in graphit and for saving to csv
addcounterfrommin<-function(lpdf=JHH,minv=0,varname="confirmed",ID="PSCR",counter="day"){
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

writewithcounters<- function(lpdf=JHH,varname="confirmed",ID="PSCR",name="JHH"){
    lpdf<- as.data.frame(lpdf)
      lpdf<- lpdf[!is.na(lpdf[c(varname)]),]
    for (minv in c(1,20,100,400,1000,2000,5000,10000)){
      lpdf<- addcounterfrommin(lpdf=lpdf, minv=minv, 
                               varname=varname,ID=ID,
                               counter=makecountname("day",minv))
    }
    filename=paste(name,"days.csv",sep="_")
    write_csv(lpdf,path=datapath%+%"/"%+% filename, na="")
    if (verbose>0) print(paste("Written the current data with counters to disk as",filename,"for use in Tableau or Excel"))
}
#Next, prepare functions to select data we want to line graph, determined by the minimum value , date, and country/ID


dataprep<- function(lpdf=JHH, minval=1, ID="PSCR", 
                     xvar="day", yvars=c("confirmed", "recovered"), 
                     logx=FALSE, logy=TRUE, sorted=TRUE){
  if (!(xvar %in% names(lpdf))) 
    lpdf<- lpdf%>% addcounterfrommin(minval,varname="confirmed", ID=ID,counter=xvar)
  if (logy){ #get rid of negative and zeros for the log
    eps<-  1e-5
    for (varname in yvars)  {
      if (sum((!is.na(lpdf[,varname]))&lpdf[,varname]<= eps)>0) 
        #if all NAs, one row replaces zero rows -> error!
        lpdf[(!is.na(lpdf[,varname]))&lpdf[,varname] <= eps,varname]<- NA 
  } }
  if(logx) lpdf[lpdf[[xvar]]<=0,xvar]<- 1 #bug? should be NA to be honest 
  if(sorted) {
    lpdf[[ID]] <- sortIDlevels(lpdf=lpdf,varname=yvars[1]) 
    lpdf<- lpdf[order(lpdf[[ID]],lpdf[[xvar]]),] 
    if (verbose>=4) print(levels(lpdf[[ID]]))
  } 
  lpdf<- lpdf[,c(xvar, ID, yvars)]# ,'Date'  ungroup %>% arrange(PSCR)
}
#Bug potential: after the sort, PSCR is a factor. before, it was  character!

graphit <- function(lpdf, countries, minval=1, ID="PSCR", xvar="Date", 
                    yvars=c("active", "recovered","deaths","confirmed"), 
                    fuzzy=FALSE, logx=FALSE, logy=FALSE, 
                    myfolder="",savename="", putlegend=TRUE, size=2,
                    returnID="PSCR", area=FALSE,position='stack',facet=FALSE, 
                    sorted=TRUE, until=Sys.Date()){
  lpdf<- as.data.frame(lpdf)
  if (typeof(until)=="character") until=as.Date(until,format="%Y-%m-%d")
  lastdate<- min(max(lpdf$Date),until)
  #if (length(countries)==0) {countries<- unique(lpdf[,returnID])
      #} else{ 
    countries<- lpdf%>% findIDnames(testIDnames=countries,searchID=ID,fuzzy=fuzzy,returnID=returnID) #}
  ID<- returnID
  lpdf <- lpdf%>% filter((confirmed>=minval)&(PSCR %in% countries)&Date<=until)  
  mytitle<- paste(format(lastdate,format="%Y%m%d"),"C19",
                  savename, paste(yvars,collapse=", "),
                  "by",xvar,"for",paste(minval,"+",sep=""),"confirmed")
  myshorttitle<- format(lastdate,format="%Y%m%d")% %"C19"% % savename% %
    initials(yvars)% % 'by'% %xvar % % "for" % % paste(minval,"+",sep="") % %
    "confirmed"
  if (str_length(mytitle)>= 80) mytitle<- myshorttitle
  if (verbose>=6) print(paste(mytitle %+%". Last required date is",until,"and last date in dataset is",max(lpdf$Date)))
  if (nrow(lpdf)==0 ) {if (verbose>=4) {print(mytitle %+% " No data -> no plot")}
                      return()  }
  lpdf <- lpdf%>%
      dataprep(ID=ID,minval=minval, xvar=xvar,yvars=yvars,logx=logx,logy=logy, sorted=sorted)
  
  if (nrow(lpdf)==0| all(is.na(lpdf[,xvar]))|all(is.na(lpdf[,yvars])))
    return(if (verbose>=5) print (paste(mytitle, "Too little data to graph. Maybe lower the mininum value, take more regions?")))
   lpdf<- lpdf%>% 
     melt(lpdf ,id=c(ID,xvar),measure.vars=yvars,
                    variable.name="varname", value.name="count")%>%
     mutate ( mygroup=PSCR %, % varname,
                varname=factor(varname, levels = yvars))#
  if (verbose>=7) print(summary(lpdf))
  
  if (facet=='varname') lpdf$mygroup<- lpdf[[ID]] 
  else if (facet==ID) lpdf$mygroup <- lpdf$varname
  nrgroups<- length(unique(lpdf$mygroup))
  if (verbose>=4) print( parse(text=substitute(xvar))% %"from"% % 
                         min(lpdf[,xvar])% % "to"% %max(lpdf[,xvar]) %, % 
                        "group by "% % lpdf$mygroup[1]%, % "facets" % % facet)
  myplot<- ggplot(lpdf, aes_string(y="count",x=xvar,group='mygroup',
                              color= ifelse(length(unique(lpdf[,ID]))==1,  
                                            'varname' , 
                                            ifelse(facet==ID,'mygroup',ID))
                                   ),na.action=na.omit) 
  if(area){posalpha<- ifelse(position=='identity', 0.4, 1)
    myplot<- myplot + geom_area(aes_string(color='mygroup',fill='mygroup'), 
                                position = position,alpha= posalpha)
    scale_f<- scale_fill_manual(values = c("red", "green","black","darkorange","lawngreen"))
    if (nrgroups<=2) scale_f<- scale_fill_manual(values = c("lawngreen", "cyan"))#,"black","darkorange","lawngreen"))
    myplot<- myplot+scale_f+  
      scale_color_manual(values = c("red", "green","black","darkorange","lawngreen"))
  }else {
    myplot<- myplot+
      geom_line(alpha=0.3,size=size*0.7)+
      geom_point(size=size, aes_string(   shape='varname'))+
      geom_dl(aes_string(x=xvar,y="count",#,color=ifelse(facet==ID,'mygroup',ID),
                        label='mygroup'),      
            method = list(dl.trans(x = x+0.1 ,y=y+0.1),"last.points", cex = 1.2))
    if (length(unique(lpdf$varname))<=6 ) 
      myplot<- myplot + scale_shape_manual(values = c(0,1,3,2,4,5,6,7,8,9,10)) #shape="\u2620"
    if (nrgroups<=6){
          myscale<- scale_color_manual(values=c("red", "darkgreen", "black","orange",
                                                  "lawngreen","darkorange"),
                                        guide= ifelse(putlegend,"legend",FALSE))
    }else if(nrgroups<13) {
      palette=ifelse (nrgroups <8, "Dark2","Paired") #Spectral Set2  
      myscale<- scale_color_brewer(palette=palette)
    } else myscale<-scale_color_discrete(guide= ifelse(putlegend,"legend",FALSE))
    myplot<- myplot +  myscale 
  }  
  
  if (!isFALSE(facet)) {myplot<- myplot+ facet_wrap(as.formula(paste("~",facet)))}
  if(xvar=="Date") myplot<- myplot+scale_x_date(labels = date_format("%d-%m"))
  y_lab <- paste(rev(yvars),collapse=" & ")% %ifelse(logy,"(log scale)","")
  if (str_length(y_lab)>75) 
    y_lab<- paste(initials(rev(yvars)),collapse="&") %+% 
            ifelse(logy,"(log scale)","")
  myplot<-myplot +  ylab(y_lab)+
    xlab(paste(xvar, ifelse(logx,"(log scale)","")))+ 
    ggtitle(mytitle) +  theme_light()+      
    guides(col = guide_legend(nrow=30, ncol = min(2,(nrgroups-1) %/% 30+1)))  
  if(logy) myplot<- myplot+scale_y_continuous(trans='log10')
  if(logx) myplot<- myplot+scale_x_continuous(trans='log10')
  if (savename!="") {
    if(facet==FALSE) savename<-  paste(savename,"all-in-one")
    if(area) savename<- paste(savename,"area plot")
    if (myfolder=="") { 
      myfolder=initials(yvars)% %'by'% %xvar}
    if(area)myfolder<- myfolder % % "area plot"
    if (logy) myfolder <- myfolder % %'log scale'
    if(facet==FALSE) myfolder<-  paste(myfolder,"all-in-one")
    if (verbose>= 3) print("making plot" % % myfolder %+% "/" %+% mytitle)
    myplot<- myplot+ theme(text=element_text(size=20) )
    mypath<- paste("G:/My Drive/Covid19_plots",lastdate,sep="/") 
    if(myfolder!="") mypath<- paste(mypath,myfolder,"",sep="/")
    if (!dir.exists(mypath)) dir.create(mypath,recursive=TRUE)
    png(filename=paste(mypath,#format(Sys.Date(),format="%Y%m%d"),"/",
                       mytitle, ifelse(logy,", log scale",""),
                       ".png",sep=""),
        width=1600,height=900)
    suppressWarnings(print(myplot));dev.off()
  }else return(myplot+theme(title = element_text(size = 11)))
}# 
#geom_point(shape="\u2620", size = 4)  #skulls
#geom_point(aes(shape=cyl, color=cyl, size=cyl))
#+scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))
 
graphcodes<- function(){ 
  print(list(nrvars=1:6,
          letters=tibble(codes=c('d/D/c/d','acrd_','y','f','i','n','M','a/l'),
          meaning=c('day,Date,confirmed,death xvar','yvars','logy','facet per ID','imputed','new','per million','area or line')
          )   ))
  print('example graph4_DcyfinmMa' % % 'graphlist contains all interesting graph function names 
        to call with makehistory')
}
# first digit  1 for date, 2 for CRD ipv arcd, 4 for "new" , 
# second digit 1 for facet. 2 for imputed, 4 for per million

text=c('test_1','of ','initials')
initials<- function (text=c('test_1','of_imputation','new_recovered_imputed_per_Million')){
  paste(unlist(lapply(
    lapply ( strsplit(text,'_'), function (st) substr(st,1,1)),
    function(vec) paste(vec, collapse="_")
  )),collapse='+')
}
rm(list=ls(pattern="graph[[:digit:]]"))
#in alfphabetical order of outputs
#graph4dardc_yfiMl<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR",
#                             logy=TRUE, until=Sys.Date()){
#  lpdf%>%graphit(group,minval,ID, xvar="Day", 
#                 yvars=c('active_imputed_p_M','recovered_imputed_p_M','deaths_p_M','confirmed_p_M'), 
#                 logy=logy,savename= savename,facet="PSCR",until=until)
#}
graph4Dardc_yfiMl<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR",
                             logy=TRUE, until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date", 
                 yvars=c('active_imputed_p_M','recovered_imputed_p_M','deaths_p_M','confirmed_p_M'), 
                 logy=logy,savename=savename,facet="PSCR",until=until)
}

#graph3darc_yfil<-function(lpdf=JHH,group,savename="",minval=100, ID="PSCR", 
#                           logy=TRUE, until=Sys.Date()){
#  lpdf%>%graphit(group, minval, ID,xvar='day', 
#                 yvars=c('active_imputed','recovered_imputed',"confirmed"), 
#                 savename= savename,facet=ID,putlegend=TRUE,until=until)
#}

graph6Dardcra_yfil<- function(lpdf=JHH,group,savename="",minval=100, 
                             ID="PSCR", logy=TRUE,until=Sys.Date()){
  lpdf%>%graphit(group, minval, ID,xvar='Date', 
               yvars=c('active_imputed','recovered_imputed',"confirmed",'deaths', 'recovered','active'), 
               logy=logy, savename= savename, facet=ID,putlegend=TRUE,until=until)
}
#same as areas
graph3Dard_fia<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR",
                          logy=FALSE, until=Sys.Date()){
  lpdf%>%graphit(group,1,ID, xvar="Date",
                 yvars=c('active_imputed','recovered_imputed','deaths'), 
                 area=TRUE,facet=ID,
                 savename=savename,
                 until=until) 
}


#new --
graph3Dard_fina<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR",logy=FALSE,
                           until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('net_active_imputed','new_recovered_imputed',
                         'new_deaths'), 
                 logy=FALSE, savename= savename,
                 facet=ID, area = TRUE, until=until) 
}

graph6Dardcra_yfinl<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR",
                               logy=TRUE, until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('net_active_imputed','new_recovered_imputed','new_deaths',
                         'new_confirmed','new_recovered', 'net_active'),  
                 logy=TRUE,savename=savename,facet=ID,until=until) 
}
graph4Dardc_yfinMl<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR",
                             logy=TRUE, until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar="Date",
                 yvars=c('new_confirmed_p_M','new_recovered_imputed_p_M','new_deaths_p_M'), 
                 logy=logy, savename=savename,facet="PSCR",until=until)
}

#other graphs
graph2cr_il<- function(lpdf=JHH,group,savename="",minval=1, ID="PSCR",logy=FALSE, 
                  until=Sys.Date()){
  lpdf%>% graphit(group,minval,ID, xvar='confirmed',
                  yvars=c('recovered_imputed','deaths'),
                  logy=logy,  logx=FALSE,savename=savename,until=until)
}

graph1dr_yil<- function(lpdf=JHH,group,savename="",minval=1, ID="PSCR",logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,ID, xvar='deaths',yvars=c('recovered_imputed'),
                 logy=logy,logx=TRUE,savename=savename,until=until)
}
graph1Dr_p_C_il<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR",
                           logy=FALSE, until=Sys.Date()){
  lpdf%>%graphit(group,1,ID, xvar="Date",
                 yvars=c('recovered_imputed_per_confirmed'), 
                 myfolder="recovery rate", 
                 logy=logy,savename=savename,putlegend=FALSE ,
                 until=until)
}


# for testing imputation quality, do not add numbers otherwise they get done for all regios.
graphDaa_fia<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR"
,logy=FALSE, until=Sys.Date()){
  lpdf%>%graphit(group,1,ID, xvar="Date",
                 yvars=c('active_imputed', 'active'),
                 area=TRUE,position='identity',facet=ID,
                 savename=savename,  until=until) 
}
graphDaa_yfil<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR",
                         logy=TRUE, until=Sys.Date()){
  lpdf%>%graphit(group, minval, ID,xvar='Date', 
          yvars=c( 'active_imputed','active'),facet=ID, 
          logy=logy, savename= savename,
          putlegend=TRUE,until=until)
}

graphDrr_fia<- function(lpdf=JHH,group,savename="",minval=100, ID="PSCR"
                        ,logy=FALSE, until=Sys.Date()){
  lpdf%>%graphit(group,1,ID, xvar="Date",
                 yvars=c('recovered_imputed','recovered'),
                 area=TRUE,position='identity',facet=ID,
                 savename=savename,  until=until) 
}


mygraphlist<- ls(pattern="graph[[:digit:]]")
mygraphnrs<- mygraphlist%>% gsub("graph","", .) 
graphcodes()
print(mygraphlist)


graphs<- function(lpdf=JHH,group="World",savename="",minval=100, ID="PSCR", 
                  until=Sys.Date() ,graphlist=mygraphlist){
  for (mygraph in graphlist){
    if (verbose>= 3) print( mygraph)
    do.call (mygraph,args=list(lpdf,group,savename,minval, ID,until=until))
  }
}


#this one does it by group of countries first. when all graphs are in one folder, this makes it not easy to compare between groups. 
#this one does the graphs per type, then per countrygroup. 
#given each graph goes into its own type's folder, it doesn t matter. 
writeRegiograph<- function(lpdf=JHH,regions,minval=100,graphlist=mygraphlist, ID="PSCR", until=Sys.Date()){
  if (typeof(regions)=="character") { regions=list(regions) }
  for (graph in graphlist)
    {if(verbose>=2) {ti=Sys.time(); print(paste(format(Sys.time(),"%H:%M:%S "),graph))}
    for (i in 1:(length(regions))){
      IDs<-findIDnames(lpdf=lpdf, testIDnames=regions[[i]],searchID=ID, fuzzy=FALSE,returnID="PSCR")
      if(verbose>=3) print(Sys.time()%: % regions[[i]][1] )
      if(verbose>=4) print(     paste(IDs,collapse="/ "))
      do.call (graph,
               args=list(lpdf,IDs,regions[[i]][1],minval, ID, until=until))
    }
    if(verbose>=3) {
      print (paste(regions[[i]][1]% % "duration: ",difftime(Sys.time(),ti,units='mins')))}
  }
}

makeDate<- function(chardate="",format=myDateFormat){
  tryCatch(as.Date(chardate, format=format),
           error=function(e){print(paste("Either enter a date or a string (please use the following Date format for the string:",myDateFormat ))})
}

makehistory<- function(lpdf,regions="",ID='PSCR', 
                       dates =as.Date(max(JHH$Date), format=myDateFormat),
                       graphlist=mygraphlist
                        ){
  if (regions[1]==""){ #bug: if wrong dimensions, we get  Error in Ops.data.frame(lpdf, JHH) :    ‘==’ only defined for equally-sized data frames 
    if (dim(lpdf)==dim(JHH)& (lpdf==JHH)) regions=JHHRegios
    if (dim(lpdf)==dim(ECDC) & (lpdf==ECDC)) regions=ECDCRegios
  }
  if (typeof(dates)=="character") {  makeDate(dates)}
  if(any(is.na(dates))) print(paste("Not all dates recognized: ",paste(dates,collapse=","),". Either enter an R date or a string (please use the following Date format for the string:",myDateFormat ))
  for (until in dates ){
    if(verbose>=1) {
      ti_da=Sys.time() 
      print(format(ti_da,"%H:%M:%S ") % % "doing" % % as.Date(until,origin="1970-01-01"))
      }
    if(nrow(lpdf[lpdf$Date<=until,])>0) {  
      #lpdf%>% writeRegiograph(regions,graphlist=graphlist,until=until) 
      for (i in 1:(length(regions))){
        ti_reg=Sys.time()
        IDs<-findIDnames(lpdf=lpdf, testIDnames=regions[[i]],searchID=ID, 
                         fuzzy=FALSE,returnID="PSCR")
        if(verbose>=2) 
          print('At' % %format( ti_reg,"%H:%M:%S ") % % 
                  'doing'% %regions[[i]][1])
        if (verbose>= 4) print( paste(IDs,collapse="/ "))
        lpdf%>% 
          graphs(group =IDs,savename=regions[[i]][1],minval=100,  ID=ID, 
                 until=until,graphlist=graphlist)
        if(verbose>=2) {print (regions[[i]][1] % %"duration"%: % 
                              round(difftime(Sys.time(),ti_reg,units='mins'),2)%+%"mins")}
      }
      if(verbose>=1) {
        print ( as.Date(until,origin="1970-01-01") % % "duration " %: % 
              round( difftime(Sys.time(),ti_da,units='mins'),1)%+%"mins")}
      
    }
    else print("no data")
    while(!is.null(dev.list())) dev.off() 
  }
  options(warn=0) #traceback() # to trace the nested calls leading to an error. 
}

#options(warn= 2 ) #all warnings become errors, stops execution. 



# check th lags between recovered, confirmed and deaths 
ccf.vf<- function(var1=c(1,2), var2=c(2,2),lag.max=30,saveit=FALSE, plotit=FALSE,printit=FALSE){
  title=paste("ccf of ",var1  , " vs ",var2,".png", sep="")
  if (saveit) {png(filename=paste("plots/ccf/",title))}
  myplot=ccf(var1,var2,lag.max=lag.max,main= title, plot=plotit,na.action=na.omit)
  if (printit) print(myplot)
  if (saveit) {dev.off()} #else {myplot}
  myplot
  }

findMaxCCF<- function(var1="new_recovered",var2="new_confirmed",myPSCR="Hubei, China", lpdf=JHH,N=5){
  if (myPSCR!="") lpdf<- lpdf[lpdf$PSCR==myPSCR,]
  lpdf<- lpdf[lpdf$Date>"2020-01-22",c("Date", var1,var2)]
  if(all(is.na(lpdf[,var1]))|all(is.na(lpdf[,var2]))) 
    return(data.frame( cor=NA,lag=NA)) #
  d <- ccf.vf(lpdf[,var1],lpdf[,var2],lag.max=30, plot = FALSE)
  if (verbose>=2) print (myPSCR)
  res = data.frame( cor=d$acf[,,1],lag=d$lag[,,1])
  if (N%%2==0)N=N-1
  a<-res[order(res$cor, decreasing=TRUE)[1:N],]
  if (verbose>=3) print(a)
  res_max = median( a$lag) #which.max(res$cor) #instead of max, take the n largest: order(R, decreasing=TRUE)[1:N]
  return(res[res$lag==res_max,])  
} 

findMaxCCFs<- function(var1="new_recovered",var2="new_confirmed", myPSCR="", lpdf=JHH){
  a<- ddply( lpdf, "PSCR", function (lpdfp){findMaxCCF(var1=var1,var2=var2,myPSCR= myPSCR, lpdf=lpdfp)})
  a[!is.na(a$lag),]
}


#end. Now run loadData.R

