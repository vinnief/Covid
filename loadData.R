#Loads latest data, first JHH then ECDC then testing

source("definitions.R")# make sure we have our definitions. 

ti <-  Sys.time()
JHH <- makeJHH(force = TRUE) 
reportDiffTime('loading and melting JHH:',ti,'secs')
JHH0 <- JHH
regios <- c(list(World = c('World', unique(JHH[['PSCR']]))), provincializeJHH(), regios) 
ti <-  Sys.time()
JHH <- JHH0 %>%
  addPopulation() %>% addCountryTotals() %>% addRegionTotals() %>%
  addRegions( Regiolist = regios) %>% arrange(PSCR,Date) %>%
  imputeRecovered() %>% extravars()#
reportDiffTime('adding population, totals, imputations, and daily vars in JHH:',ti,'secs')
ti = Sys.time()
JHH <- JHH %>%  addDoublingDaysPerCountry(variable = 'confirmed') %>% 
  addDoublingDaysPerCountry(variable = 'active_imputed') 
reportDiffTime('adding the doubling days (twice) in JHH:',ti,'mins')

#ti = Sys.time()
#JHH <- addSimVars(JHH, minVal = 100) %>% 
#  addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")
#reportDiffTime('adding the simulated values in JHH:',ti,'mins')
JHHRegios <- makeRegioList(JHH)
#writeWithCounters(JHH,name = "Covid19JHH") 

#same with ECDC
tim = Sys.time()
ECDC0 <- makeECDC()
reportDiffTime('load ECDC:',tim,'mins')
tim = Sys.time()
ECDC  <- ECDC0 %>% correctMissingLastDay() %>% 
  addTotals3 %>% imputeRecovered %>%  extravars %>%
  mutate(Country.Region = PSCR) %>%
  addDoublingDaysPerCountry(variable = 'active_imputed') %>%
  addDoublingDaysPerCountry(variable = 'confirmed') 
reportDiffTime('correct, add totals, imputations, vars, doubling days in ECDC:',tim,'mins')

#tim = Sys.time()
#ECDC <- ECDC %>%  
  #addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") #%>% #, maxDate = Sys.Date() - 1
  # Because of missing Spain data, growth in Europe on last day is negative. hence sim does not work
#ECDC <- ECDC %>% addSimVars(minVal = 100)  #gives errors. cayman islands follows conveyance_Japan
#reportDiffTime('adding the simulated values in ECDC:',tim,'secs')
ECDCRegios <- makeDynRegions( ECDC, piecename = 'ECDC World')
#writeWithCounters(ECDC,name = "Covid19ECDC")

testing <- readTesting()
write.csv(testing,myPlotPath %//% "data" %//% 'testing.csv' )


# end now run Graphs.R or output.md

