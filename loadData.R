source ("requirements.R")# now do it: make the long panel data frame
covid19<- makeCovid19(force=TRUE)
CRPSlist<- unique(covid19$CRPS)  #seems not used. 
Worldcountrylist<- unique(covid19$Country.Region)
a<- max(datasel(Europe)$confirmed)
bigEurope<- c("big Europe",setdiff(unique(datasel(Europe,a/10 )$CRPS),"Europe"))
middleEurope<- c("middle Europe",setdiff(unique(datasel(Europe,a/50 )$CRPS),c("Europe",bigEurope)))                           
smallEurope<- c("small Europe",setdiff(unique(datasel(Europe,a/500 )$CRPS),c("Europe",bigEurope,middleEurope)))        
microEurope<- c("microEurope", setdiff(Europe,c(smallEurope,bigEurope,middleEurope,"Europe")))      
  

covid19<- covid19[covid19$CRPS!="Europe",] #just to be sure, that if i do it twice i dont get double counts. 
#require(plm)
#covid19<- pdata.frame(covid19,index=c("CRPS", "Date"),stringsAsFactors=FALSE)

#covid19$confirmed[base::diff(covid19$confirmed)!=plyr::diff(covid19$confirmed)]
covid19<- extravars(covid19) 
#this needs to be done after makign it a panel data frame as in plm, but that did not work. 
#so i programmed my own pdf diff function. 
#
#delete USA tots, China tots, AUstraliatots , Canadatots first
tail(covid19[covid19$CRPS %in% c("Australia", "Canada","China","USA","US"),])
tail(covid19[covid19$CRPS %in% c("Australia", "Canada","China"),])
WorldCRPSList<- setdiff(unique(covid19[,"CRPS"]),c("USA","Australia","China","Canada"))# we have USA twice: the world data has country USA, the US data had county which we totalled for each state)
Worldtots<- total(WorldCRPSList,id="CRPS",newrow="World",lpdf=covid19,
                  varnames= c("confirmed","recovered", "deaths","population","active","new_confirmed","new_deaths","new_recovered","Long","Lat" ))
Worldtots<- extravars(Worldtots)
Regiolist<- list(Europe,EU,EFTA,NAmericaS,NAmerica, SouthWestAsia,SouthEastAsia, MENA,Africa,SAsiaIO,EastAsia,CIS,SAmerica,Caribbean,OceaniaP,ChinaP,China,Oceania)
covid19<- makeGroups(covid19)
print(paste("Not attributed regions: (add to their regions as needed)"))
print(unique(covid19[covid19$Region=="",c('CRPS','Province.State','Country.Region')]))
writewithcounters(covid19,name="Covid19") #this does not save the factor order tho! Best to save the RData file and reload that. 


UStots<- extravars(totals("US","Country.Region",varnames= c("confirmed","recovered", "deaths","population","Long","Lat" ),lpdf=covid19))
#"active","new_confirmed","new_deaths","new_recovered"
Eurotots<- extravars(total(Europe,id="CRPS",newrow="Europe",lpdf=covid19,
                 varnames= c("confirmed","recovered", "deaths","population","Long","Lat" )))
regtots<- rbind(UStots,Eurotots,
                extravars(totals("China",varnames= c("confirmed","recovered", "deaths","population","active","Long","Lat" ))),
                extravars(totals("Australia",varnames= c("confirmed","recovered", "deaths","population","Long","Lat" ))),
                extravars(totals("Canada",varnames= c("confirmed","recovered", "deaths","population","Long","Lat" ))))
# end now run Graphs.R
