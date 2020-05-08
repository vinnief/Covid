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
  id<-c("Country.Region","CRPS","Province.State","Lat","Long")
  if (US) { id<-c(id,"Combined_Key")   }
  wpdf$CRPS <- (ifelse(""==wpdf$Province.State, 
                              as.character(wpdf$Country.Region),
                              paste(wpdf$Province.State,wpdf$Country.Region,sep=', ')))
  lpdf<-reshape2::melt(wpdf,id=id,
                       variable.name=coltype, value.name=values.name)  
  #lpdf<- wpdf %>% pivot_longer(????)
  lpdf$Date<- as.Date(paste(lpdf[,coltype],"20",sep=""),format="X%m.%d.%Y") 
  lpdf$date<- NULL
  return(lpdf)
} #note if data.table package is added, it has its own "melt" function

findidnames <- function(lpdf=JHH, testidnames=c("Neth","India"), searchid="CRPS",
                         fuzzy=TRUE, returnid=""){
  lpdf<- as.data.frame(lpdf)
  allids<- (unique(lpdf[,searchid]))   #error maybe? [ for dataframe 
  if (!fuzzy) {a1<- intersect(testidnames,allids)
  }else a1<- allids[unlist(llply(testidnames,function(a) grep(a,allids, ignore.case=TRUE)))]
  if (returnid=="")  return ( a1) #returnid=searchid
  unique(lpdf[lpdf[,searchid] %in% a1,returnid])
  #filter(lpdf, .vars(searchid)%in% a1)%>% select(vars(returnid))
} 
aggreg<- function(avector){
  if(length(unique(avector))==1){avector[1]
  }else #if (length(unique(avector))==2) {paste(avector,collapse="_") }else
    paste(avector[1],length(unique(avector))-2,avector[length(avector)],sep="_")
}

total.tibble<- function(lpt=JHH,id=Country.Region,varnames=c('confirmed',#deaths,
                                                             'recovered')){
  lpttot<-lpt%>%
    group_by({{id}},Date)
  lpttot<- cbind(
    #lpttot%>% summarize(newCRPS=aggreg(CRPS),
    #          newCountry.Region=aggreg(Country.Region),
    #          newProvince.State=aggreg(Province.State)),
    lpttot%>% summarize_at(.vars=!!!varnames, .funs=colSums)
            )
              #confirmed=sum(confirmed),
              #active=sum(active),
              #recovered=sum(recovered),
              #deaths=sum(deaths),
              #colSums(.[,])
  #if (!!id=="CRPS")lpttot%>% rename(Coun)
}
#JHH[JHH$Country.Region==c('Netherlands'),]%>%total.tibble(CRPS)%>% view()
#JHH[JHH$Country.Region==c('Netherlands'),]%>%total.tibble(Country.Region)%>% view()
total<- function(rows="", 
                 id="CRPS", newrow="" ,lpdf=JHH,
                 varnames=c("confirmed","deaths","recovered")  
                 ) {
  ans<- ddply(lpdf[lpdf[,id] %in% rows,],c("Date"),
    function(df) {
      Country.Region=ifelse(newrow!="", newrow,
                            aggreg(as.character(df$Country.Region)))
      Province.State=ifelse(newrow!="",newrow,aggreg(df$Province.State))
      CRPS=ifelse(newrow!="",
                  newrow,
                  ifelse(id=="CRPS",
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

totals<- function(rows="", #c("Canada","US","Australia","China",Europe,Samerica, Africa
                  id="Country.Region", 
                  varnames=c("confirmed","deaths","recovered"),
                  lpdf=JHH){
  if (rows[1]=="") rows=as.list(unique(lpdf[,id]))
  if (verbose>=6) print(paste("Making the total for ",paste(rows,collapse="/ "),"in",id))
  ans<-ldply((rows), function (a) total(a,id,a, lpdf,varnames))
}
             
totals2<- function(rows="", # used only for county to state totalling. 
                  id="Country.Region", 
                  varnames=c("confirmed","deaths"),
                      # "recovered", "active","new_confirmed","new_deaths","new_recovered"
                  lpdf=JHH){
  if (rows[1]=="") rows=unique(lpdf[,id])
  if (verbose>5) print(paste("Making totals2 for ",paste(rows,collapse=","),"in",id))
  ans<-ddply(lpdf[lpdf[,id] %in% rows,],c("Date", id),
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
  lpdf<- (totals2("","Province.State",lpdf=lpdf)) # depends on all provinces being chosen to sum! 
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
  CUS<-as.tibble( CUS0)
  #write.csv(CUS,nameUS)
  Cworld0<-makelpdf() 
  Cworld<-  as.tibble(Cworld0)
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

sortCRPS1<- function(lpdf,varname="confirmed",date1=""){
  varname<- enquo(varname)
  if (date1==""){date1= max(lpdf$Date) } else 
    if (nrow(lpdf[lpdf$Date== date1,]==0)){
      stop("Cannot sort on values of a date which is not present in the Data")}
  CRPSlevels<- lpdf  %>% select(c(CRPS,Date, !!varname)) %>% 
    filter(Date==date1) %>% 
    #arrange(desc({{varname}}),CRPS,.by_group = FALSE) 
    arrange(-eval(parse(text=substitute(!!varname))),CRPS,.by_group = FALSE) 
    #arrange(-(!!varname),CRPS,.by_group = FALSE) 
  lpdf<-lpdf%>%ungroup%>% mutate(CRPS= factor(lpdf$CRPS,levels=CRPSlevels$CRPS))%>%
    group_by(CRPS)
  lpdf$CRPS
} #the desc eval parse substitute  !!  should have been desc !! according to the manuals. but desc does not respect unquo. 

sortCRPS<- function(lpdf,varname="confirmed",id="CRPS",date1=""){
  if (date1==""){date1= max(lpdf$Date) } else 
    if (nrow(lpdf[lpdf$Date== date1,]==0)){
      stop("Cannot sort on values of a date which is not present in the Data")}
  ordre<- as.data.frame(lpdf[lpdf$Date== date1,c(varname,id)])
  ordre<- ordre[order(ordre[,varname],ordre[,id],decreasing=TRUE),] 
  lpdf[,id]<- factor(lpdf[,id],levels=ordre[,id])
}

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
  #lpdf$CRPS<- sortCRPS(lpdf)
  lpdf
}


######## make state groups, also useful in tableau
WestvsEast<- c("WestvsEast","USA","UK","Italy","Iran","Korea","Germany","France","Spain","Sweden","Norway","Hubei","Belgium","Netherlands","Singapore","Japan","Taiwan*","Denmark","Hubei, China", "Hongkong, China", "Jiangsu, China")
Benelux<- c("Benelux","Belgium","Netherlands","Luxembourg")
EU6<-c("EU6", Benelux[2:4], "Germany","France","Italy")
EU<- c("EU",EU6[2:7],"Spain","Poland","Austria","Romania","Hungary","Ireland","Sweden","Denmark","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Cyprus")
EFTA<-c("EFTA","Iceland","Liechtenstein","Switzerland","Norway")
Europe<- c("Europe",EU[2:28],EFTA[2:5], "United Kingdom", "Russia", "Ukraine", "Belarus","Moldova","Georgia", "Armenia", "Azerbaijan","Andorra", "Monaco", "San Marino", "Vatican","Holy See", "Albania", "North Macedonia","Kosovo","Croatia","Montenegro","Bosnia and Herzegovina","Serbia","Gibraltar","Faroe Islands", "Isle of Man","Channel Islands","Greenland")

CIS<- c("CIS","Russia", "Belarus", "Armenia", "Azerbaijan","Kazakhstan","Kyrgyzstan","Turkmenistan","Tajikistan","Uzbekistan","Moldova")
SouthWestAsia<-c("South West Asia","Afganistan","Iran","Irak","Syria","Lebanon","Turkey","Israel", "West Bank and Gaza","Palestine")
SouthEastAsia<- c("South East Asia","Indonesia","Thailand","Vietnam","Laos","Malaysia", "Cambodia", "Papua New Guinea","Myanmar", "Burma","Brunei","Philippines","Timor-Leste")
SAsiaIO<-c ("South Asia & Indian Ocean","India","Pakistan","Bangladesh","Sri Lanka","Comoros", "Maldives","Madagascar","Mauritius", "Seychelles","Bhutan","Nepal","Mayotte","Reunion")
EastAsia<- c("East Asia","Japan","Korea, South", "Korea, North","Taiwan*", "Hong Kong","Singapore","Mongolia")

Asia<- setdiff(c("Asia regions", SAsiaIO, SouthEastAsia,SouthWestAsia,EastAsia,"China", CIS),c("Madagascar","East Asia","South Asia & Indian Ocean","South East Asia","South West Asia","CIS","Russia","Moldova", "Belarus", "Georgia", "Azerbaijan","Armenia"))

MENA<-c("MENA", "Marocco","Algeria","Tunesia","Libia","Egypt", "West Bank and Gaza","Palestine","Lebanon","Syria","Turkey","Iraq","Iran","Afghanistan","Jordan","Saudi Arabia","Kuwait","Oman","United Arab Emirates","UAE","Yemen","Bahrain","Qatar")
SAmerica<-c("South America countries","Argentina","Bolivia","Brazil","Chile","Colombia","Costa Rica","Honduras","El Salvador","Panama","Ecuador","Suriname","Guyana","Belize","Guatemala", "Antilles","Paraguay","Peru","Venezuela","Nicaragua"       , "Uruguay","French Guiana","Falkland Islands (Malvinas)","Nicaragua")
US<-c("US","USA")
NAmerica<- c("North America",US,"Canada","Mexico","Saint Pierre and Miquelon")
Caribbean<- c("Caribbean",'Anguilla',"Antigua and Barbuda","Bahamas" ,  "Barbados","Bermuda","Cayman Islands","Cuba","Dominica" ,"Dominican Republic","Grenada", "Haiti" , "Jamaica","Saint Kitts and Nevis" ,"Saint Vincent and the Grenadines","Saint Lucia"  ,"Trinidad and Tobago",'Aruba','Curacao',"Bonaire, Sint Eustatius and Saba","British Virgin Islands",'Guadeloupe','Martinique','Sint Maarten','St Martin','Saint Barthelemy','Turks and Caicos Islands','Montserrat')
Africa<- c("Africa countries","Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon","Central African Republic","Chad","Comoros", "Congo (Kinshasa)", "Congo (Brazzaville)", "Cote d'Ivoire", "Djibouti", "Egypt",  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana","Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara","Zambia", "Zimbabwe")
Oceania<- c("Oceania","Australia","New Zealand","Vanuatu","Tuvalu", "Fiji","Guam","French Polynesia","New Caledonia"  )
China<- c("China")
MSM<-c("MSM","Netherlands","Belgium","United Kingdom","Germany","Malta","Egypt","Suriname","China","Vietnam","Hungary","Romania","Kuwait","Italy","Ireland","Iran","Kazakstan","Liberia","Indonesia","Ethiopia","Nigeria","Ghana","Uganda","South Africa","Canada","Spain","France")
regios=list(Europe=Europe,EU=EU,EFTA=EFTA,NAmerica=NAmerica, SoutWestAsia=SouthWestAsia,SouthEastAsia=SouthEastAsia, MENA=MENA,Africa=Africa,SAsiaIO=SAsiaIO,EastAsia=EastAsia,CIS=CIS,SAmerica=SAmerica,Caribbean=Caribbean,China=China,Oceania=Oceania)
getname <- function (variab, name=deparse(substitute(variab))) { 
   name 
} #not used

makeGroups <- function(lpdf=JHH,varname="Region",Regiolist="") {  
  #if(!varname %in% names(lpdf)) lpdf[,varname]<-""
  if(!varname %in% names(lpdf)) lpdf[,varname]<-""
  for (Regio in Regiolist) {
    region= Regio[1] %>% strsplit(" countries") %>% unlist %>% strsplit(" Provinces&States")%>% unlist
    lpdf[lpdf$Province.State %in% Regio,varname] <- region
    lpdf[lpdf$CRPS %in% Regio,varname]<- region
    }
  if (verbose>= 1){
    print("Regions added:"% % paste(unique(JHH$Region),collapse="/ "))
    print(paste("Not attributed regions: (add to their regions as needed)"))
    print(unique(lpdf[lpdf$Region=="",c('CRPS','Province.State','Country.Region')]))  
    print(unique(lpdf[is.na(lpdf$Region),c('CRPS','Province.State','Country.Region')]))
  }
  lpdf
}
#lpdf<- ecdcdata;gridsize=5*6
makeDynRegions <- function(lpdf=JHH,gridsize=7*6) {
  a<- lpdf%>% ungroup %>% 
    filter(!(CRPS %in% c("USA","US","Australia","China","Canada","South America","Asia","Africa","World","Europe")),
           Date==max(Date))%>% 
    select( CRPS, confirmed)%>% arrange(desc(confirmed)) #done when extravars were created, and at creation of JHH already. 
  gridsize = ceiling(sqrt(length(unique(lpdf$CRPS))/8+.25)-.5) 
  gridsize= gridsize*(gridsize-1)
       #this way 8 grids of gridsize*(gridsize+1) result. 
  World1<-c("World 1",as.character(a[1:gridsize,]$CRPS ))
  World2<-c("World 2",as.character(a[(gridsize+1 ) : (2*gridsize),]$CRPS))
  World3<-c("World 3",as.character(a[(2*gridsize+1): (3*gridsize),]$CRPS))
  World4<-c("World 4",as.character(a[(3*gridsize+1): (4*gridsize),]$CRPS))
  World5<-c("World 5",as.character(a[(5*gridsize+1): (6*gridsize),]$CRPS))
  World6<-c("World 6",as.character(a[(6*gridsize+1): (7*gridsize),]$CRPS))
  World7<-c("World 7",as.character(a[(7*gridsize+1): min(nrow(a),8*gridsize),]$CRPS))
  World8<-c("World 8",as.character(a[(min(nrow(a),8*gridsize)+1):nrow(a),]$CRPS))
  a<- lpdf%>% ungroup %>% 
    filter((CRPS %in% setdiff(Europe, "Europe")),
           Date==max(Date))%>% 
    select(CRPS, confirmed)%>% arrange(desc(confirmed))
  gridsize<- 4*4
  Europe1<- c("Europe 1",as.character(a[1:gridsize,]$CRPS ))
  Europe2<-  c("Europe 2",as.character(a[(gridsize+1):(2*gridsize),]$CRPS))
  Europe3<-  c("Europe 3",as.character(a[(2*gridsize+1):(3*gridsize),]$CRPS))
  Europe4<-  c("Europe 4",as.character(a[(3*gridsize+1):min(nrow(a),4*gridsize),]$CRPS))
  
  as.list(paste("World",c("1","2","3","4","5","6","7"),sep=""))
  list( World1=World1, World2=World2, World3=World3, World4=World4, World5=World5, 
        World6=World6, World7=World7, World8=World8, Europe1=Europe1, 
        Europe2=Europe2, Europe3=Europe3)
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

provincializeJHH<- function(lpdf=JHH){
  CanadaP<- provincialize("Canada",lpdf)
  USS<- provincialize(US,lpdf)
  NAmericaS<- provincialize((NAmerica),lpdf)
  ChinaP<- provincialize("China",lpdf)
  OceaniaP<- provincialize(Oceania,lpdf)
  list(CanadaP=CanadaP,USS=USS, NorthAmerica=NAmericaS, ChinaP=ChinaP, OceaniaP=OceaniaP)
}


addPopulation <- function(lpdf) {
  population<- read.csv('population.csv')[c(1,3)]
  names(population)[2]<- "population"
  rownames(population)<- population$Country.Name
  lpdf[,"population"]<- population[lpdf%>% pull(CRPS),"population"]
  #lpdf[,"population"]<- population[lpdf$CRPS,"population"]
  #lpdf[,"population"]<- population[as.character(lpdf[,"CRPS"]),"population"]
  
  #if (!("population"%in% names(lpdf)))  lpdf<- merge(lpdf,population,by.x="Country.Region",by.y="Country.Name",all.x=TRUE)
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

imputeRecovered<- function(lpdf=JHH,lagrc=22,lagrd=15,dothese=FALSE,redo=FALSE){
  #lpdf<- pdata.frame(lpdf,index=c("CRPS", "Date"),stringsAsFactors=FALSE)
  if(!('recovered' %in% names(lpdf))) lpdf$recovered<- as.numeric(NA)
  if(any(dothese)) lpdf$recovered_old<- lpdf$recovered
  if (!"imputed"%in% names(lpdf))lpdf$imputed<-FALSE
  rowstodo<- drop(is.na(lpdf$recovered)|dothese|(redo&lpdf$imputed))
  if (verbose>=2) print("Imputing recovered for:"%+%paste(unique(lpdf[rowstodo,"Country.Region"]),collapse=" / "))
  if (sum(rowstodo)==0)return(lpdf)
  #lpdf[rowstodo,"imputed"]<- TRUE
  attr(lpdf,"imputed")<- "lagrc=22, lagdc=15"
  lpdf<- lpdf%>% group_by(CRPS)%>% 
    mutate_cond( rowstodo,imputed=TRUE )%>%      
    mutate_cond( rowstodo,recovered <- #max(recovered,
                                           dplyr::lag(confirmed,{{lagrc}})-  dplyr::lag(deaths,{{lagrd}})) ##{{}} ipv !! ? 
                                           #,na.rm=TRUE)
  #lpdf[rowstodo,"recovered"]<-  dplyr::lag(lpdf[rowstodo,"confirmed"],lagrc)- 
   #                           dplyr::lag(lpdf[rowstodo,"deaths"],lagrd)
  #lpdf
}
imputeRecovered2<- function(lpdf=ecdcdata,varname="recovered", lagrc=22,lagrd=15,
                            dothese=FALSE,redo=FALSE){
  if(!('recovered' %in% names(lpdf))) lpdf$recovered<- as.numeric(NA)
  if(any(dothese)) lpdf[,varname%+% "_old"]<- lpdf[,varname]
  if (!"imputed"%in% names(lpdf))lpdf$imputed<-FALSE
  rowstodo<- drop(is.na(lpdf$recovered)|dothese|(redo & lpdf$imputed))
  
  if (verbose>=2) print(paste("imputing recovered for:",
                              paste(unique(lpdf[rowstodo,"CRPS"]),collapse=" / ")))
  if (sum(rowstodo)==0)return(lpdf)
  lpdf<- lpdf%>% group_by(CRPS) %>%
    mutate_cond(rowstodo,imputed=TRUE )%>% 
    mutate_cond(rowstodo, recovered= dplyr::lag(confirmed,lagrc)- dplyr::lag(deaths,lagrd) ) #bug: cannot find lagrc suddenly, as it is not part of the dataframe. only complains about it if i use the function on a data frame (JHH) not on ecdcdata. Why? 
}

ma.diff.lpdf<- function(lpdf,id="CRPS", varnames=c("confirmed","active","recovered", "deaths"),prefix="new_",n=3){
  ans<- ddply(lpdf, id, 
              function (lpdf){
                data.frame(llply(lpdf[,varnames], 
                                 function(a){c(NA,ma(base::diff(a),n))}
                ))
              }
  )
  ans[,id]<- NULL
  names(ans)<- paste(prefix,varnames,sep="")
  ans
}

extravars<- function(lpdf,lagrc=0,lagdc=0){ #note only use lags if lpdf is a real lpdf a la plm
  lpdf$active <- lpdf$confirmed - lpdf$deaths - lpdf$recovered
  lpdf<-lpdf[with(lpdf, order(CRPS, Date)), ]
  prefix = "new_"
  varnames=c("confirmed","active","recovered", "deaths")
  lpdf[,paste("new_",varnames,sep="")]<- NULL #just to make sure if we twice use this function we dont get into problems. 
  lpdf<- cbind(lpdf,ma.diff.lpdf(lpdf,prefix = prefix))
  lpdf$confirmed_pM <- 1000000*lpdf$confirmed/lpdf$population
  lpdf$active_pM    <- 1000000* lpdf$active  /lpdf$population
  lpdf$recovered_pM <- 1000000*lpdf$recovered/lpdf$population
  lpdf$deaths_pM    <- 1000000*lpdf$deaths   /lpdf$population
  
  lpdf$new_confirmed_pM <- 1000000*lpdf$new_confirmed/lpdf$population
  lpdf$new_active_pM    <- 1000000* lpdf$new_active  /lpdf$population
  lpdf$new_recovered_pM <- 1000000*lpdf$new_recovered/lpdf$population
  lpdf$new_deaths_pM    <- 1000000*lpdf$new_deaths  /lpdf$population
  
  lpdf$recovered_per_confirmed<- ifelse(plm::lag(lpdf$confirmed,lagrc)>0, 
                            lpdf$recovered/plm::lag(lpdf$confirmed,lagrc), NA)
  lpdf$deaths_per_confirmed<- ifelse(plm::lag(lpdf$confirmed,lagdc)>0, 
                             lpdf$deaths/plm::lag(lpdf$confirmed,lagdc), NA)
  lpdf$recovered_per_deaths<- ifelse(plm::lag(lpdf$deaths,lagrc-lagdc)>0,
                             lpdf$recovered/plm::lag(lpdf$deaths,lagrc-lagdc), NA)
  
  lpdf$Date<- as.Date(lpdf$Date,format="%Y-%m-%d") #otherwise we cannot calculate the first date that confirmed is over minval. 
  lpdf$CRPS<- sortCRPS(lpdf) #the CRPS is now a factor! sorted on confirmed and CRPS! 
  lpdf
}
extravars2<- function(lpdf,lagrc=0,lagdc=0){
  lpdf<- lpdf %>%  ungroup %>% 
    arrange(CRPS, Date) %>%
    group_by(CRPS) %>% 
    mutate(active = confirmed - deaths - recovered,
           new_confirmed=ma(diff.sl(confirmed)), 
           new_active=ma(diff.sl(active)),
           new_recovered=ma(diff.sl(recovered)), 
           new_deaths=ma(diff.sl(deaths)),
           confirmed_pM = 1000000*confirmed/population,
           active_pM    = 1000000* active  /population,
           recovered_pM = 1000000*recovered/population,
           deaths_pM    = 1000000*deaths   /population,
           new_confirmed_pM = 1000000*new_confirmed/population,
           new_active_pM    = 1000000*new_active  /population,
           new_recovered_pM = 1000000*new_recovered/population,
           new_deaths_pM    = 1000000*new_deaths  /population,
           recovered_per_confirmed= frac(recovered,dplyr::lag(confirmed,lagrc)),
           deaths_per_confirmed= frac(deaths,dplyr::lag(confirmed,lagdc)),
           recovered_per_deaths= frac(recovered,dplyr::lag(deaths,lagrc-lagdc))              )
}

############### used in graphit and for saving to csv
addcounterfrommin<-function(lpdf=JHH,minv=0,varname="confirmed",id="CRPS",counter="day"){
  lpdf[,counter]<-as.numeric(NA)
  lpdf<- lpdf%>% filter(!is.na(!!varname))   #[!is.na(lpdf[,varname]),] #should not have any! effect for "confirmed" 
  if(sum(lpdf[,varname]>=minv)>0)  
  #lpdf<- lpdf%>% mutate_cond((!!varname)>=minv) %>% group_by(id) %>%
   #       mutate(counter=seq_along(Date))  
    
   lpdf[lpdf[,varname]>=minv,]<- 
         ddply(lpdf[lpdf[,varname]>=minv,],id, 
            function(lpdf){
              lpdf[,counter]<- seq_along(lpdf$Date)
              lpdf}
            )
    lpdf
}

### make day vars for tableau & Excel
makecountname <- function(countname,minv){paste(countname,minv,sep="_")}

writewithcounters<- function(lpdf=JHH,varname="confirmed",id="CRPS",name="JHH"){
    lpdf<- as.data.frame(lpdf)
      lpdf<- lpdf[!is.na(lpdf[c(varname)]),]
    for (minv in c(1,20,100,400,1000,2000,5000,10000)){
      lpdf<- addcounterfrommin(lpdf=lpdf, minv=minv, 
                               varname=varname,id=id,
                               counter=makecountname("day",minv))
    }
    filename=paste(name,"days.csv",sep="_")
    write.csv(lpdf,file=filename, na="")
    if (verbose>0) print(paste("Written the current data with counters to disk as",filename,"for use in Tableau or Excel"))
}
#Next, prepare functions to select data we want to line graph, determined by the minimum value , date, and country/id

datasel.old<- function(countries=MSM, minval= 0, lpdf=JHH,  #deprecated, unused
                   varname="confirmed", id="CRPS"){
  return( lpdf[ (lpdf[,varname]>=minval)&(lpdf[,id] %in% countries) , ]) 
}
datasel<- function( lpdf=JHH, countries=MSM, minval= 0 ){ #unused
  return( filter(lpdf,(confirmed>=minval)&(CRPS %in% countries) ) )  
}
dataprep1<- function(lpdf=JHH,countries=NULL, minval=1, id="CRPS", 
                     xvar="day", yvars=c("confirmed", "recovered"), 
                     logx=FALSE, logy=TRUE, 
                     returnid="CRPS"){
  if (!(xvar %in% names(lpdf))) lpdf<- addcounterfrommin(lpdf,minval,varname="confirmed", id=returnid,counter=xvar)
  if (logy) for (varname in yvars)  {
    if (sum((!is.na(lpdf[,varname]))&lpdf[,varname]<= 0)>0)
      lpdf[(!is.na(lpdf[,varname]))&lpdf[,varname] <= 0,varname]<- NA 
  }
  if(logx) lpdf[lpdf[,xvar]<=0,xvar]<- 1 
  lpdf[,id]<- sortCRPS(lpdf=lpdf,varname=yvars[1]) 
  lpdf[,c(xvar, returnid,yvars)]
}


dataprep2<- function(lpdf,id, xvar, yvars, #not used, integrated into graphit again. 
                     variable.name="varname", value.name="count"){
  lpdf<- melt(lpdf ,id=c(id,xvar),measure.vars=yvars,
              variable.name=variable.name, value.name=value.name)
  lpdf[,variable.name]<- factor(lpdf[,variable.name], levels = yvars) #better for graphing and legends?
  lpdf$mygroup<- paste(lpdf[,id],lpdf[,variable.name],sep=", ")
  lpdf
}

graphit <- function(lpdf, countries, minval=1, id="CRPS", xvar="Date", 
                     yvars=c("active", "recovered","deaths","confirmed"), 
                     fuzzy=FALSE, logx=FALSE, logy=FALSE, 
                     myfolder="",savename="", putlegend=TRUE, size=2, returnid="CRPS", 
                     area=FALSE,position='stack',facet=FALSE, until=Sys.Date()){
  lpdf<- as.data.frame(lpdf)
  if (typeof(until)=="character") until=as.Date(until,format="%Y-%m-%d")
  lastdate<- min(max(lpdf$Date),until)
  #if (length(countries)==0) {countries<- unique(lpdf[,returnid])
      #} else{ 
    countries<- lpdf%>% findidnames(testidnames=countries,searchid=id,fuzzy=fuzzy,returnid=returnid) #}
  lpdf <- lpdf%>% filter((confirmed>=minval)&(CRPS %in% countries)&Date<=until) 
          
  if (verbose>=6) print(paste("in graphit, until is",until,"and last date in dataset is",max(lpdf$Date)))
  if (nrow(lpdf)==0) {warning("no data -> no plot");    return()  }
  lpdf<- lpdf%>% dataprep1(countries=countries,id=id,minval=minval, xvar=xvar,yvars=yvars,logx=logx,logy=logy, returnid=returnid)
  id<- returnid
  
  mytitle<- paste("Covid-19",format(lastdate,format="%Y%m%d"),
                  savename, paste(yvars,collapse=" & "),
                  "by",xvar,"for",paste(minval,"+",sep=""),"confirmed")
  if (nrow(lpdf)==0| all(is.na(lpdf[,xvar]))|all(is.na(lpdf[,yvars])))
    return(if (verbose>=5) print (paste(mytitle, "Too little data to graph. Maybe lower the mininum value, take more regions?")))
 
   lpdf<- lpdf%>% melt(lpdf ,id=c(id,xvar),measure.vars=yvars, #instead of dataprep2
                    variable.name="varname", value.name="count")%>%
       mutate (varname=factor(varname, levels = yvars), mygroup=CRPS %, % varname)
  if (verbose>=6) print(summary(lpdf))
  #lpdf<- dataprep2(lpdf=lpdf, id=id,  xvar=xvar, yvars=yvars, 
   #               variable.name="varname", value.name="count")
  if (facet=='varname') lpdf$mygroup<- lpdf[,id] 
  else if (facet==id) lpdf$mygroup <- lpdf$varname
  if (verbose>=4) print( parse(text=substitute(xvar))% %"from"% % 
                           min(lpdf[,xvar])% % "to"% %max(lpdf[,xvar]) %, % 
                           "group by "% % lpdf$mygroup[1]%, %
                           "facets" % % facet)
  myplot<- ggplot(lpdf, aes_string(y="count",x=xvar,group='mygroup',
                              color= ifelse(length(unique(lpdf[,id]))==1,  
                                            'varname' , 
                                            ifelse(facet==id,'mygroup',id))
                                   ),na.action=na.omit) 
  if(area){myplot<- myplot + geom_area(aes_string(color='mygroup',fill='mygroup'), position = position,alpha=.6)+scale_fill_manual(values = c("red", "green","black","orange"))
  }else {#lpdf$label <- ""
    #lpdf$label[lpdf$Date==max(lpdf$Date)] <- lpdf$mygroup
    myplot<- myplot+
      geom_line(alpha=0.3,size=size*0.7)+#,
                #aes_string(color= ifelse(length(unique(lpdf[,id]))==1,  
                                      #'varname' , 
                                      #ifelse(facet==id,'mygroup',id))))+
      geom_point(size=size, aes_string(#color= ifelse(length(unique(lpdf[,id]))==1,
                                        #'varname' , 
                                        #ifelse(facet==id,'mygroup',id)),
                          shape='varname'))+
    #geom_label_repel(aes_string(x=xvar,y="count",color=ifelse(facet==id,'mygroup',id),label='label')) #labels LAST DATE pointS!
    geom_dl(aes_string(x=xvar,y="count",#,color=ifelse(facet==id,'mygroup',id),
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
 

#1 like 11 a+r+d+c line log, all in one graph (the original)
graph1<- function(group,name="",minval=100, id="CRPS",lpdf=JHH,logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group, minval, id,xvar='day',  logy=logy,
          yvars=c( 'active','recovered','deaths',"confirmed"), 
          myfolder="a,r,d,c by day all-in-one", 
          savename= ifelse(name=="","",paste(name,"all-in-one")),putlegend=TRUE,until=until)
}
#11 a+r+d+c by day line, log, facets by id
graph11<- function(group,name="",minval=100, id="CRPS",lpdf=JHH
                   , until=Sys.Date(),logy=TRUE){
  lpdf%>%graphit(group, minval, id,xvar='day', 
                 yvars=c('active','recovered','deaths' ,"confirmed"), 
                 myfolder="a,r,d,c by day", logy=TRUE,
                 savename= name,facet=id,putlegend=TRUE,until=until)
}


#2 conf+active+recovered+deaths         areaplot          facet per id
graph2<- function(group,name="",minval=100, id="CRPS",lpdf=JHH,logy=FALSE
                  , until=Sys.Date()){
  lpdf%>%graphit(group,1,id, xvar="Date",yvars=c('active','recovered','deaths'), 
            myfolder="a,r,d areaplot", area=TRUE,facet=id,
          savename= ifelse(name=="","",paste(name,"areaplot")),until=until) 
}

##3 a+r+d+c  per Million line, log
graph3<- function(group,name="",minval=100, id="CRPS",lpdf=JHH,logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,id, xvar="Date", myfolder="a,r,d,c per Million", 
          yvars=c('active_pM','recovered_pM','deaths_pM','confirmed_pM'), 
          logy=logy,savename= name,facet="CRPS",until=until)
}

#4 new confirmed by date line, logs, all-in-one
graph4<- function(group,name="",minval=1, id="CRPS",lpdf=JHH,logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,id, xvar='Date',yvars=c('new_confirmed'),
                 myfolder="new c by Date", logy=logy,  logx=FALSE,
                 savename=name,until=until)
}


#5 new conf , new rec, new deaths line  by date, facets by id
graph5<- function(group,name="",minval=100, id="CRPS",lpdf=JHH,logy=TRUE,
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,id, xvar="Date",
          yvars=c('new_confirmed','new_recovered','new_deaths'),  
          myfolder="new c,r,d", logy=TRUE,savename= name,facet=id,until=until) 
}

#6a new active, recovered, deaths by day, area, facet by id
graph0<- function(group,name="",minval=100, id="CRPS",lpdf=JHH,logy=FALSE,
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,id, xvar="Date",
                 yvars=c('new_confirmed','new_recovered','new_deaths'), 
                 myfolder="new c,r,d areaplot", logy=FALSE,
                 savename= ifelse(name=="",name,paste(name,"areaplot")),
                 facet=id,area = TRUE,position='identity',until=until) 
}
#6 new active, recovered, deaths by day, area, facet by id
graph6<- function(group,name="",minval=100, id="CRPS",lpdf=JHH,logy=FALSE,
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,id, xvar="Date",
          yvars=c('new_active','new_recovered','new_deaths'), 
          myfolder="new a,r,d areaplot", logy=FALSE,
          savename= ifelse(name=="",name,paste(name,"areaplot")),
          facet=id,area = TRUE,until=until) 
}



#7 new c, r, d, per Million line plot by date, facets by id
graph7<- function(group,name="",minval=100, id="CRPS",lpdf=JHH,logy=TRUE,
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,id, xvar="Date",
          yvars=c('new_confirmed_pM','new_recovered_pM','new_deaths_pM'), 
          myfolder="new c,r,d per Million", logy=logy,
          savename= name,facet="CRPS",until=until)
}


#8 reco & deaths by conf nolog
graph8<- function(group,name="",minval=1, id="CRPS",lpdf=JHH,logy=FALSE, 
                  until=Sys.Date()){
  lpdf%>% graphit(group,minval,id, xvar='confirmed',yvars=c('recovered','deaths'),
          myfolder="r,d by c", logy=logy,  logx=FALSE,savename=name,until=until)
}

#9 r by d all in one
graph9<- function(group,name="",minval=1, id="CRPS",lpdf=JHH,logy=TRUE, 
                  until=Sys.Date()){
  lpdf%>%graphit(group,minval,id, xvar='deaths',yvars=c('recovered'),
                 myfolder="r by d",logy=logy,logx=TRUE,savename=name,until=until)
}

#10 reco over conf by date line graph per id
graph10<- function(group,name="",minval=100, id="CRPS",lpdf=JHH,logy=FALSE, 
                   until=Sys.Date()){
  lpdf%>%graphit(group,1,id, xvar="Date",yvars=c('recovered_per_confirmed'), 
          myfolder="r per c", logy=logy,savename= name,putlegend=FALSE ,
          until=until)
}


graphs<- function(group,name="",minval=100,nrs=0:10, id="CRPS",lpdf=JHH, until=Sys.Date()){
  for (j in nrs)
      do.call (paste("graph",j,sep=""),args=list(group,name,minval, id,lpdf,until=until))
  }
#this one does it by group of countries first. when all graphs are in one folder, this makes it not easy to compare between groups. 
#this one does the graphs per type, then per countrygroup
writeRegiograph<- function(regions,minval=100,nrs=0:10, id="CRPS",lpdf=JHH, until=Sys.Date()){
  if (typeof(regions)=="character") {
    name= getname(regions)  #bug. it just outputs "regions"
    regions=list(c(name,regions))
}
  for (j in nrs)
    {if(verbose>=2) {ti=Sys.time(); print(paste(ti,"Graph ",j))}
    for (i in 1:(length(regions))){
      ids<-findidnames(lpdf=lpdf, testidnames=regions[[i]],searchid=id, fuzzy=FALSE,returnid="CRPS")
      if(verbose>=3) print(paste(Sys.time(),paste(ids,collapse=", ")))
      do.call (paste("graph",j,sep=""),args=list(ids,regions[[i]][1],minval, id,lpdf,until=until))
    }
    if(verbose>=2) {print (paste("duration: ",Sys.time()-ti))}
  }
}

makeDate<- function(chardate="",format=myDateFormat){
  tryCatch(as.Date(chardate, format=format),
           error=function(e){print(paste("Either enter a date or a string (please use the following Date format for the string:",format ))})
}

makehistory<- function(lpdf=JHH, dates =as.Date(max(JHH$Date), format=myDateFormat),
                        regions=regioList,
                        nrs=0:11,
                        regioList=graphRegiolist
                        ){
  if (typeof(dates)=="character") {  makeDate(dates)}
  if(any(is.na(dates))) print(paste("Not all dates recognized: ",paste(dates,collapse=","),". Either enter an R date or a string (please use the following Date format for the string:",format ))
  options(warn=-1)
  for (until in dates ){
    if(verbose>=1) {ti=Sys.time(); print(paste(ti, "doing", as.Date(until,origin="1970-01-01")))}
    if(nrow(lpdf[lpdf$Date<=until,])>0)writeRegiograph(regions,nrs=nrs,until=until) else print("no data")
    if(verbose>=1) {print (paste("duration for ",Sys.Date(),": ",Sys.time()-ti))}
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
