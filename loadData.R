source ("requirements.R")#load libraries
source("definitions2.R")# make sure we have our definitions. 
#now do it: make the long panel data frame, with id=CRPS, time= Date
JHH<- makeJHH(force=TRUE)
#lpdf$CRPS<- sortCRPS(lpdf)
Asia<- c(Asia, setdiff(provincialize(China),"China Provinces&States"))
JHH <-  JHH %>%
          makeGroups( Regiolist=as.list(c(
              provincializeJHH(JHH), 
              list(Europe=Europe,EU=EU,EFTA=EFTA,NAmerica=NAmerica, SoutWestAsia=SouthWestAsia,SouthEastAsia=SouthEastAsia, MENA=MENA,Africa=Africa,SAsiaIO=SAsiaIO,EastAsia=EastAsia,CIS=CIS,SAmerica=SAmerica,Caribbean=Caribbean,China=China,Oceania=Oceania))))%>%
          addPopulation() %>%
        imputeRecovered(redo=TRUE) #%>%
JHH<- JHH%>%         extravars2() #adds active, new_....=ma(diff,3),  _pM
#bug: some NA rows added. 
#bug: impute now crashes
#bug: makeJHH cannot rbind two datasets in R4. 
##lpdf<- lpdf[!(grepl("Princess" , lpdf$CRPS)),]
          
writewithcounters(JHH,name="Covid19JHH") #no factors!
print( "partial totals exist for")
print(unique(JHH[JHH$CRPS %in% c("Australia", "Canada","China","USA","US","Asia","Africa","Europe"),c("CRPS")]))

addtotals<- function(lpdf){
  lpdf<- as.data.frame(lpdf)
  World<- setdiff(unique(lpdf$CRPS),c("USA","US","Australia","China","Canada","Asia","Africa"))# we have USA twice: the world data has country USA, the US data had county which we totalled for each state)
  lpdf<- lpdf %>% filter(!CRPS %in% c("South America", "Asia", "Africa", "Europe","China","Australia","Canada","US","World")) %>%  #just to be sure, that if i do it twice i dont get double counts. 
    as.data.frame()
  
  rbind(lpdf, extravars2(total(World
                                ,id="CRPS",newrow="World",lpdf=lpdf,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(totals("US","Country.Region",lpdf=lpdf,
                                 varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(total(Europe,id="CRPS",newrow="Europe",lpdf=lpdf,
                                varnames= c("confirmed","recovered", "deaths","population" ))),
                extravars2(total(Africa,id="CRPS",newrow="Africa",lpdf=lpdf,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(total(Asia,id="CRPS",newrow="Asia",lpdf=lpdf,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(total(SAmerica,id="CRPS",newrow="South America",lpdf=lpdf,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(totals("China", 
                                 varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(totals("Australia",
                                 varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(totals("Canada",
                                 varnames= c("confirmed","recovered", "deaths","population"))))
}
JHH<- JHH %>%   addtotals  ## bug: 
#Error in Ops.Date(1e+06, lpdf$new_recovered) : 
 # * not defined for "Date" objects
#In addition: Warning messages:
#  1: Factor `CRPS` contains implicit NA, consider using `forcats::fct_explicit_na` 
#2: Factor `CRPS` contains implicit NA, consider using `forcats::fct_explicit_na` 
#3: In Ops.factor(1e+06, lpdf$new_confirmed) :
#  ‘*’ not meaningful for factors
#4: In Ops.factor(1e+06, lpdf$new_active) :
#  Show Traceback

# end now run Graphs.R

