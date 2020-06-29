source("requirements.R")#load libraries
source("definitions.R")# make sure we have our definitions. 
#execute the data loading, first JHH then ECDC then testing
JHH <- makeJHH(force = TRUE) 
JHH0 <- JHH
regios <- c(list(World = c('World', unique(JHH[['PSCR']]))), provincializeJHH(), regios) 
ti <-  Sys.time()
JHH <- JHH0 %>%
  addPopulation() %>% addCountryTotals() %>% addRegionTotals() %>%
  addRegions( Regiolist = regios) %>% arrange(PSCR,Date) %>%
  imputeRecovered() %>% extravars()#
reportDiffTime('loading and adding totals, imputations, and daily vars in JHH:',ti,'secs')
ti = Sys.time()
JHH <- JHH %>%  addDoublingDaysPerCountry(variable = 'confirmed') %>% 
  addDoublingDaysPerCountry(variable = 'active_imputed') 
reportDiffTime('adding the doubling days in JHH:',ti,'secs')

ti = Sys.time()
JHH <- JHH %>% addSimVars(minVal = 100) #%>% 
JHH <- JHH %>%  addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")
reportDiffTime('adding the simulated values in JHH:',ti,'mins')

writeWithCounters(JHH,name = "Covid19JHH") 

#same with ECDC
tim = Sys.time()
ECDC0 <- makeECDC()
ECDC  <- ECDC0 %>% addTotals3 %>% imputeRecovered %>%  extravars %>%
  mutate(Country.Region = PSCR, Province.State = "") %>%
  addDoublingDaysPerCountry(variable = 'active_imputed') %>%
  addDoublingDaysPerCountry(variable = 'confirmed') 
reportDiffTime('loadn an add the doubling days in ECDC:',tim,'secs')
tim = Sys.time()
#ECDC <- ECDC %>% 
 # addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") %>% 
#  addSimVars(minVal = 100) 
#  reportDiffTime('adding the simulated values in ECDC:',tim,'mins')
writeWithCounters(ECDC,name = "Covid19ECDC")

testing <- readTesting()
write.csv(testing,myPlotPath %//% "data" %//% 'testing.csv' )
# end now run Graphs.R
