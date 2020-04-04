# now do it: make the long panel data frame
covid19<- makeCovid19()
CRPSlist<- (covid19$CRPS) 
countrylist<- unique(covid19$Country.Region)
a<- max(datasel(Europe)$confirmed)
bigEurope<- c("big Europe",setdiff(unique(datasel(Europe,a/10 )$CRPS),"Europe"))
middleEurope<- c("middle Europe",setdiff(unique(datasel(Europe,a/50 )$CRPS),c("Europe",bigEurope)))                           
smallEurope<- c("small Europe",setdiff(unique(datasel(Europe,a/500 )$CRPS),c("Europe",bigEurope,middleEurope)))        
microEurope<- c("microEurope", setdiff(Europe,c(smallEurope,bigEurope,middleEurope,"Europe")))      
  
CanadaP<-provincialize('Canada')
USS<-provincialize(US)
NAmericaS<- provincialize(NAmerica)
OceaniaP<- provincialize(Oceania)
ChinaP<- provincialize(China)

#covid19<- makeGroups(covid19,list=list(Europe))
covid19<- covid19[covid19$CRPS!="Europe",]
Eurotots<- total(Europe,id="CRPS",lpdf=covid19,varnames= c("confirmed","active", "deaths","population","new_confirmed","new_deaths","new_recovered" ))
Eurotots$CRPS<- "Europe"
Eurotots<- extravars(Eurotots)
covid19<- rbind(covid19,Eurotots)
rm(Eurotots)
require(plm)
covid19<- pdata.frame(covid19,index=c("CRPS", "Date"),stringsAsFactors=FALSE)
covid19<- extravars(covid19)
covid19<- makeGroups(covid19)
#covid19$Date<-format(covid19$Date,format="%Y-%m-%d")
#covid19$Date<- as.Date(covid19$Date, format="%Y-%m-%d")
#View(covid19[covid19$Region==Europe,]$CRPS)
print(paste("Not attributed regions:"))
print(unique(covid19[covid19$Region=="",c('CRPS','Province.State','Country.Region')]))

sortCountries<- function(lpdf){
  ordre<- lpdf[lpdf$Date== max(lpdf$Date),c("confirmed","CRPS")]
  ordre<- ordre[order(ordre$confirmed,ordre$CRPS,decreasing=c(TRUE,FALSE)),]
  lpdf$CRPS<- factor(lpdf$CRPS,levels=ordre$CRPS)
}
covid19$CRPS<- sortCountries(covid19)

writewithcounters(covid19,name="Covid19") #this deletes the order!


