source("requirements.R")#load libraries
source("definitions.R")# make sure we have our definitions. 
#execute the data loading, make the long panel data frame, 
#
#with id=PSCR, time= Date, totals for continents and world, 
#and (in future) sorted by confirmed (desc)

JHH <- makeJHH(force = TRUE) #load data
JHH0 <- JHH
regios <- c(list(World = c('World', unique(JHH[['PSCR']]))), provincializeJHH(), regios) #regiosP?
#verbose=1
JHH <- JHH0 %>%
  addPopulation() %>% addTotals2 %>% addRegions( Regiolist = regios) %>% 
  imputeRecovered() %>% extravars()#
JHH <- JHH %>%  addDoublingDaysPerCountry(variable = 'confirmed') %>% 
  addDoublingDaysPerCountry(variable = 'active_imputed') 

#JHH<- JHH %>% addSimVars(minVal = 100) #%>% 
  JHH <- JHH %>%  addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")
#names(JHH)
writeWithCounters(JHH,name = "Covid19JHH") #no factors!

#same with ECDC
ECDC0 <- makeECDC()

ECDC <- ECDC0 %>% addTotals3 %>% imputeRecovered %>%  extravars %>%
#ECDC<- ECDC  %>% 
          addDoublingDaysPerCountry(variable = 'active_imputed') %>% #addSimVars(minVal=100)%>%
  addDoublingDaysPerCountry(variable = 'confirmed') %>%
      addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")# %>% addSimVars(minVal = 100)

writeWithCounters(ECDC,name = "Covid19ECDC")
# end now run Graphs.R
