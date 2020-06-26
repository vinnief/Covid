source("requirements.R")#load libraries
source("definitions.R")# make sure we have our definitions. 
#execute the data loading, 
JHH <- makeJHH(force = TRUE) #load data
JHH0 <- JHH
regios <- c(list(World = c('World', unique(JHH[['PSCR']]))), provincializeJHH(), regios) #regiosP?
#verbose=1
#
ti = Sys.Date()
JHH <- JHH0 %>%
  addPopulation() %>% addTotals2 %>% addRegions( Regiolist = regios) %>% 
  imputeRecovered() %>% extravars()#
reportDiffTime('loading and adding totals, imputations, and daily vars in JHH:',ti,'auto')

ti = Sys.Date()
JHH <- JHH %>%  addDoublingDaysPerCountry(variable = 'confirmed') %>% 
  addDoublingDaysPerCountry(variable = 'active_imputed') 
reportDiffTime('adding the doubling days in JHH:',ti,'auto')

ti = Sys.Date()
JHH <- JHH %>% addSimVars(minVal = 100) #%>% 
JHH <- JHH %>%  addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")
reportDiffTime('adding the simulated values in JHH:',ti,'auto')
writeWithCounters(JHH,name = "Covid19JHH") #no factors!
#names(JHH)

#same with ECDC
ECDC0 <- makeECDC()
ECDC <- ECDC0 %>% addTotals3 %>% imputeRecovered %>%  extravars %>%
#ECDC<- ECDC  %>% 
          addDoublingDaysPerCountry(variable = 'active_imputed') %>% #addSimVars(minVal=100)%>%
  addDoublingDaysPerCountry(variable = 'confirmed') 
#%>% addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")# %>% addSimVars(minVal = 100)

writeWithCounters(ECDC,name = "Covid19ECDC")

testing <- readTesting()
write.csv(testing,myPlotPath %//% "data" %//% 'testing.csv' )
# end now run Graphs.R
