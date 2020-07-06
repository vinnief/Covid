source("requirements.R")#load libraries
source("definitions.R")# make sure we have our definitions. 

#execute the data loading, first JHH then ECDC then testing
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

ti = Sys.time()
JHH <- JHH %>% addSimVars(minVal = 100) #%>% 
JHH <- JHH %>%  addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")
reportDiffTime('adding the simulated values in JHH:',ti,'mins')
JHHRegios <- makeRegioList(JHH)
writeWithCounters(JHH,name = "Covid19JHH") 



#same with ECDC
tim = Sys.time()
ECDC0 <- makeECDC()
reportDiffTime('load ECDC:',tim,'mins')
tim = Sys.time()
ECDC  <- ECDC0 %>% correctMissingLastDay() %>% addTotals3 %>% imputeRecovered %>%  extravars %>%
  mutate(Country.Region = PSCR) %>%
  addDoublingDaysPerCountry(variable = 'active_imputed') %>%
  addDoublingDaysPerCountry(variable = 'confirmed') 
reportDiffTime('correct, add totals, imputations, vars, doubling days in ECDC:',tim,'mins')

tim = Sys.time()
ECDC <- ECDC %>%  
  addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") #%>% #, maxDate = Sys.Date() - 1
  # Because of missing Spain data, growth in Europe on last day is negative. hence sim does not work
#ECDC <- ECDC %>% addSimVars(minVal = 100)  #gives errors. cayman islands follows conveyance_Japan
 #reportDiffTime('adding the simulated values in ECDC:',tim,'mins')
ECDCRegios <- makeDynRegions( ECDC, piecename = 'ECDC world')
writeWithCounters(ECDC,name = "Covid19ECDC")

testing <- readTesting()
write.csv(testing,myPlotPath %//% "data" %//% 'testing.csv' )

#show some data table output
#latest numbers
JHH[JHH$Date == max(JHH$Date),'Date'][1,1]
JHH %>% ungroup %>% filter(Date == max(Date)) %>% filter(!is.nan(new_active_rate)) %>%
  select(PSCR,active_imputed, new_confirmed, active_imputed_growthRate, new_active_rate, 
         Date) %>% arrange(new_active_rate) %>% tail(20)
JHH[JHH$Date == max(JHH$Date) & JHH$PSCR %in% 
      c('EU','World','New York,US',"Kazakhstan",'Belgium','Spain','US','Netherlands','Europe',
        'Germany','France','Africa','Iran','Russia','Brazil'),
    c('confirmed','new_confirmed','active_imputed','deaths','PSCR',
      'new_active_rate', 'active_imputed_growthRate','confirmed_p_M') ]  %>%
  arrange(new_active_rate)


#overtaking

map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'theyme',lastDays = 4))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'theyme',lastDays = 1))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'theyme',lastDays = 1))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'theyme',lastDays = 4))

map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'Ithem',lastDays = 4))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'Ithem',lastDays = 4))

map_dfc(c('Germany','France','United Kingdom'), function(x) overtakeDays_df(JHH,x,who = "theyme", lastDays = 4))

map_dfc(c('Indonesia','Peru','India'),function(x) overtakeDays_df(JHH,x,who = 'Ithem'))
#map(c('Indonesia','Peru','India'),function(x) overtakeDays_v(JHH,x,who = 'Ithem'))
map_dfc(c('Indonesia','Peru','India'),function(x) overtakeDays_df(JHH,x,who = 'theyme'))


# end now run Graphs.R

