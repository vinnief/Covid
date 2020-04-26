source ("requirements.R")#load libraries
source("definitions2.R")# make sure we have our definitions. 
#now do it: make the long panel data frame, with id=CRPS, time= Date
covid19<- makeCovid19(force=TRUE)
covid19 <-  covid19 %>%
          makeGroups( Regiolist=as.list(c(
              provincializeJHH(covid19), 
              list(Europe=Europe,EU=EU,EFTA=EFTA,NAmerica=NAmerica, SoutWestAsia=SouthWestAsia,SouthEastAsia=SouthEastAsia, MENA=MENA,Africa=Africa,SAsiaIO=SAsiaIO,EastAsia=EastAsia,CIS=CIS,SAmerica=SAmerica,Caribbean=Caribbean,China=China,Oceania=Oceania)))) %>%
          addPopulation() %>%
##lpdf<- lpdf[!(grepl("Princess" , lpdf$CRPS)),]
          imputeRecovered(redo=TRUE) %>%
          extravars() #adds active, new_....=ma(diff,3),  _pM
#bug: some NA rows added. 
writewithcounters(covid19,name="Covid19") #no factors!

print( "partial totals exist for")
print(unique(covid19[covid19$CRPS %in% c("Australia", "Canada","China","USA","US","Asia","Africa","Europe"),c("CRPS")]))

WorldCRPSList<- setdiff(unique(covid19[,"CRPS"]),c("USA","US","Australia","China","Canada","Asia","Africa"))# we have USA twice: the world data has country USA, the US data had county which we totalled for each state)

covid19<- covid19 %>% filter(!CRPS %in% c("South America", "Asia", "Africa", "Europe","China","Australia","Canada","US","World")) #just to be sure, that if i do it twice i dont get double counts. 
regtots<- rbind(extravars(total(WorldCRPSList,id="CRPS",newrow="World",lpdf=covid19,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars(totals("US","Country.Region",lpdf=covid19,
                                 varnames= c("confirmed","recovered", "deaths","population"))),
                extravars(total(Europe,id="CRPS",newrow="Europe",lpdf=covid19,
                                varnames= c("confirmed","recovered", "deaths","population" ))),
                extravars(total(Africa,id="CRPS",newrow="Africa",lpdf=covid19,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars(total(Asia,id="CRPS",newrow="Asia",lpdf=covid19,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars(total(SAmerica,id="CRPS",newrow="South America",lpdf=covid19,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars(totals("China", 
                                 varnames= c("confirmed","recovered", "deaths","population"))),
                extravars(totals("Australia",
                                 varnames= c("confirmed","recovered", "deaths","population"))),
                extravars(totals("Canada",
                                 varnames= c("confirmed","recovered", "deaths","population"))))


covid19<- rbind(covid19,regtots)
rm(regtots)
# end now run Graphs.R
unique(covid19$Region)
