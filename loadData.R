source ("requirements.R")# now do it: make the long panel data frame
source("definitions2.R")
covid19<- makeCovid19(force=TRUE)

#CRPSlist<- unique(covid19$CRPS)  #seems not used. 
WorldCRPSList<- unique(covid19$CRPS)
a<- sort(datasel(WorldCRPSList,id="CRPS",lpdf=covid19[covid19$Date==max(covid19$Date), ])$confirmed,decreasing= TRUE)
bigEarth<- c("big Earth",as.character(unique(datasel(WorldCRPSList,a[36] )$CRPS)))
middleEarth<- setdiff(c("middle Earth",as.character(unique(datasel(WorldCRPSList,a[72] )$CRPS))), bigEarth)
smallEarth<- setdiff(c("small Earth",as.character(unique(datasel(WorldCRPSList,a[108] )$CRPS))), c(middleEarth, bigEarth))
microEarth<- setdiff(c("micro Earth",as.character(unique(datasel(WorldCRPSList,a[144])$CRPS))),c(bigEarth,middleEarth,smallEarth))
a<- sort(datasel(Europe,id="CRPS",lpdf=covid19[covid19$Date==max(covid19$Date), ])$confirmed,decreasing= TRUE)
bigEurope<- c("big Europe",setdiff(unique(datasel(Europe,a[16] )$CRPS),"Europe"))
middleEurope<- c("middle Europe",setdiff(unique(datasel(Europe,a[32] )$CRPS),c("Europe",bigEurope)))                           
smallEurope<- c("small Europe",setdiff(unique(datasel(Europe,a[48] )$CRPS),c("Europe",bigEurope,middleEurope)))        
microEurope<- c("microEurope", setdiff(Europe,c(smallEurope,bigEurope,middleEurope,"Europe")))      

CanadaP<- provincialize("Canada")
USS<- provincialize(US)
NAmericaS<- provincialize((NAmerica))
ChinaP<- provincialize("China")
OceaniaP<- provincialize(Oceania)
covid19<- makeGroups(covid19, Regiolist =  list(Europe,EU,EFTA,NAmericaS,NAmerica, SouthWestAsia,SouthEastAsia, MENA,Africa,SAsiaIO,EastAsia,CIS,SAmerica,Caribbean,OceaniaP,ChinaP,China,Oceania))
print(paste("Not attributed regions: (add to their regions as needed)"))
print(unique(covid19[covid19$Region=="",c('CRPS','Province.State','Country.Region')]))  
#print(unique(covid19[is.na(covid19$Region),c('CRPS','Province.State','Country.Region')]))  
covid19<- addPopulation(covid19)

print("population unknown: ")
print(unique(covid19[is.na(covid19$population),"CRPS"]))

#View(covid19[grepl(", US", covid19$CRPS),])

covid19<- extravars(covid19) #no totals, adds active, new_... , population,  pM
#delete USA tots, China tots, AUstraliatots , Canadatots first
tail(covid19[covid19$CRPS %in% c("Australia", "Canada","China","USA","US"),])
tail(covid19[covid19$CRPS %in% c("Australia", "Canada","China"),])
WorldCRPSList<- setdiff(unique(covid19[,"CRPS"]),c("USA","Australia","China","Canada"))# we have USA twice: the world data has country USA, the US data had county which we totalled for each state)
Worldtots<- total(WorldCRPSList,id="CRPS",newrow="World",lpdf=covid19,
                  varnames= c("confirmed","recovered", "deaths","population","active","new_confirmed","new_deaths","new_recovered","Long","Lat" ))
Worldtots<- extravars(Worldtots)

writewithcounters(covid19,name="Covid19") #this does not save the factor order tho! Best to save the RData file and reload that. 

#covid19$new_CRPS<-NULL
#"active","new_confirmed","new_deaths","new_recovered"
regtots<- rbind(extravars(totals("US","Country.Region",varnames= c("confirmed","recovered", "deaths","population","Long","Lat" ),lpdf=covid19)),
                extravars(total(Europe,id="CRPS",newrow="Europe",lpdf=covid19,
                                varnames= c("confirmed","recovered", "deaths","population","Long","Lat" ))),
                extravars(totals("China",varnames= c("confirmed","recovered", "deaths","population","active","Long","Lat" ))),
                extravars(totals("Australia",varnames= c("confirmed","recovered", "deaths","population","Long","Lat" ))),
                extravars(totals("Canada",varnames= c("confirmed","recovered", "deaths","population","Long","Lat" ))))

# if you want to include the whole of Australia, Canada, Europe and USA also, do this: 
covid19<- covid19[!covid19$CRPS %in% c("Europe","China","Australia","Canada","US","World"),] #just to be sure, that if i do it twice i dont get double counts. 

covid19<- rbind(covid19,regtots,Worldtots)
rm(Worldtots,regtots,popUS,population,lpdf)
# end now run Graphs.R