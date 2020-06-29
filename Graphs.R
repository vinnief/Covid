#for publication 
rm(list = setdiff(ls(), c('ECDC0', 'JHH0', 'JHH', 'ECDC', 'JHHRegios', 'ECDCRegios','testing')))
options(warn = 0)
source("loadData.R")  #also loads the requirements and the definitions 

#makeDyn Regions sorts by confirmed and countries get added sometimes. 
# so the next 4 lines need to be done on the latest data!

#latest numbers
JHH[JHH$Date == max(JHH$Date),'Date'][1,1]
JHH %>% ungroup %>% filter(Date == max(Date)) %>% filter(!is.nan(new_active_rate)) %>%
  select(PSCR,active_imputed, new_confirmed, active_imputed_growthRate, new_active_rate, 
         Date) %>% arrange(new_active_rate) %>% tail(20)
JHH[JHH$Date == max(JHH$Date) & JHH$PSCR %in% 
    c('Malta','World','New York,US',"Kazakhstan",'Belgium','Peru','Spain','US','Netherlands','Europe',
        'Germany','France','Africa','Iran','Russia','Brazil'),
    c('confirmed','new_confirmed','active_imputed','deaths','PSCR',
      'new_active_rate', 'active_imputed_growthRate','confirmed_p_M') ]  %>%
  arrange(new_active_rate)

#overtaking

map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'Ithem',lastDays = 2))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'Ithem',lastDays = 1))

map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'theyme',lastDays = 2))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'theyme',lastDays = 1))

map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'theyme',lastDays = 2))
map_dfc(c('Germany','France','Spain',"Italy",'United Kingdom'), function(x) overtakeDays_df(JHH,x,who = "Ithem"))

map_dfc(c('Indonesia','Peru','India'),function(x) overtakeDays_df(JHH,x,who = 'Ithem'))
#map(c('Indonesia','Peru','India'),function(x) overtakeDays_v(JHH,x,who = 'Ithem'))
map_dfc(c('Indonesia','Peru','India'),function(x) overtakeDays_df(JHH,x,who = 'theyme'))
#compare my countries
graph3Dard_fia(JHH,c("Spain","Belgium","Netherlands",'Illinois,US','Ontario,Canada',"France"))
#make all graphs
ECDCRegios <- makeDynRegions( ECDC, piecename = 'ECDC world')
tim = Sys.time()
curGraph('GR', lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs)
reportDiffTime('ECDC graphs',tim)

JHHRegios <- makeRegioList(JHH)
tim = Sys.time()
curGraph('GR', lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs)
reportDiffTime('JHH graphs',tim)

graphCodes()
print(myGraphNrs)
print(myGraphListbyDate)
print(myGraphListbyDay)
myGraphNrs
myGraphList

verbose = 2
#look at growth rates and growth paths in first days (Synchronized)
ECDC %>% byRegionthenGraph(regions = ECDCRegios,graphlist = c("graph1dnr_iyl","graph2dac_iyl"))

#simulate deaths and confirmed   
ECDC %>% byRegionthenGraph(ECDCRegios,ext = '_sim', graphlist = c('graphDccprr_fiyl','graphDddp_fyl'))  #sims
ECDC %>% byRegionthenGraph(ECDCRegios,ext = '_endsim',graphlist = c('graphDccprr_fiyl','graphDddp_fyl')) #simulations 


if ( weekdays( Sys.Date() , abbreviate = FALSE) == "Friday") ECDC %>% 
  byRegionthenGraph(ECDCRegios, graphlist = myGraphListbyDate)

JHH %>% byRegionthenGraph(regions = JHHRegios,graphlist = 'graphDccprr_fyl')
JHH %>% byRegionthenGraph(regions = JHHRegios[1:2],graphlist = 'graphDccprr_fyl')
  #byRegionthenGraph(regions=JHHRegios[1:2],graphlist = 'graphDccprr_fyl')


verbose = 3

#do standard graphs for all past dates
JHH %>% makehistory(regions = JHHRegios, dates = seq(Sys.Date() - 0,Sys.Date() - 250,-10))  
ECDC %>% makehistory(regions = ECDCRegios, dates = seq(Sys.Date() - 250,Sys.Date() - 0,30))  

