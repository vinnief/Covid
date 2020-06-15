#for publication #MSM,Vincent,
rm(list = setdiff(ls(), c('ECDC0', 'JHH0', 'JHH', 'ECDC', 'JHHRegios', 'ECDCRegios')))
source("loadData.R")  #also loads the requirements and the definitions 

#makeDyn Regions sorts by confirmed and countries get added regularly. 
# so the next 4 lines need to be done on the latest data!

#latest numbers
#JHH[JHH$Date==max(JHH$Date)&JHH$PSCR %in% c('Malta','World','New York,US',"Kazakhstan",'Belgium','US','Netherlands','Europe','Germany','France','Africa','Iran','Russia','Brazil'),c('confirmed','new_confirmed','active_imputed','deaths','PSCR')]%>% mutate(newrate=round(new_confirmed/active_imputed*100,2))%>% arrange(newrate)

JHH[JHH$Date == max(JHH$Date) & JHH$PSCR %in% 
    c('Malta','World','New York,US',"Kazakhstan",'Belgium','Spain','US','Netherlands','Europe',
        'Germany','France','Africa','Iran','Russia','Brazil'),
    c('confirmed','new_confirmed','active_imputed','deaths','PSCR',
      'new_active_rate', 'active_imputed_growthRate') ]  %>%
  arrange(new_active_rate)

#overtaking
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'Ithem'))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'theyme'))
map(c('Germany','France','Spain',"Italy",'United Kingdom'),          function(x) overtakeDays_v(JHH,x,who = "Ithem"))


map(c('Indonesia','Peru','India'),function(x) overtakeDays_v(JHH,x,who = 'Ithem'))
map(c('Indonesia','Peru','India'),function(x) overtakeDays_v(JHH,x,who = 'theyme'))


#compare my countries
graph3Dard_fia(ECDC,c("Spain","Belgium","Netherlands","France"))

graph3Dard_fina(JHH,c("Kazakhstan","Belgium","Netherlands","France"),from = "2020-06-10")
graph3Dard_fina(ECDC,regios$MSM,from = Sys.Date()-7) 
graph6Dardcra_finyl(ECDC,regios$MSM,from = Sys.Date()-7)
graph6Dardcra_finyl(JHH, c("Kazakhstan","Belgium","Netherlands","France"),from = Sys.Date()-10)
#make all graphs
ECDCRegios <- makeDynRegions( ECDC, piecename = 'ECDC world')
curGraph('GR', lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs)
JHHRegios <- makeRegioList(JHH)
curGraph('GR', lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs)

graphCodes()
myGraphNrs
myGraphList
myGraphListbyDate
verbose = 2
#look at growth rates and growth paths in first days (Synchronized)
ECDC %>% byRegionthenGraph(regions = ECDCRegios,graphlist = c("graph1dnr_iyl","graph2dac_iyl"))

#simulate deaths and confirmed   
ECDC %>% byRegionthenGraph(ECDCRegios,ext='_sim', graphlist=c('graphDccprr_fiyl','graphDddp_fyl'))  #sims
ECDC %>% byRegionthenGraph(ECDCRegios,ext='_endsim',graphlist=c('graphDccprr_fiyl','graphDddp_fyl')) #simulations 


if ( weekdays( Sys.Date() , abbreviate=FALSE) == "Friday") ECDC %>% 
  byRegionthenGraph(ECDCRegios, graphlist= myGraphListbyDate)

JHH %>% makeHistoryGraph(regions=JHHRegios,graphlist = 'graphDccprr_fyl')
JHH %>% byRegionthenGraph(regions=JHHRegios[1:2],graphlist = 'graphDccprr_fyl')
  #makeHistoryGraphsRG(regions=JHHRegios[1:2],graphlist = 'graphDccprr_fyl')


verbose=3

#do standard graphs for all past dates
JHH%>% makehistory(regions=JHHRegios, dates=seq(Sys.Date()-0,Sys.Date()-200,-10))  
ECDC%>% makehistory(regions=ECDCRegios, dates=seq(Sys.Date()-210,Sys.Date()-0,30))  

####corrections to earlier graphs
verbose=1

#check imputations
#JHHdifimp<- unique(JHH%>% filter(active != active_imputed)%>% .$PSCR )
JHH%>% makehistory(regions=
                     unique(JHH%>% filter(active != active_imputed)%>% .$PSCR ), 
                   graphlist = c('graphDrr_fia','graphDaa_fia','graphDaa_yfil')  ) 
JHH%>% filter(PSCR %in% "Wyoming,US") %>% select(PSCR,confirmed,active_imputed,recovered_imputed,deaths)%>%View

### find mac ccf per country. should be around 21 or at least 15. it is much less!
#are diffs made properly? 
all(is.na(JHH[JHH$Date=="2020-01-22", "new_confirmed"]))  # should be true
findMaxCCF(myPSCR="Italy")
findMaxCCF(myPSCR="Hubei,China")
findMaxCCFs(myPSCR="Hubei,China")
findMaxCCFs(myPSCR="Taiwan*")
rclags<-findMaxCCFs("new_recovered","new_confirmed")
rclags<- rclags[!is.nan(rclags$cor),]
hist(rclags$lag, plot=TRUE,breaks=20)
rclags[rclags$lag>=0,"lag"] %>% median
rclags[order(rclags$lag,decreasing = TRUE),][1:20,]

rdlags<-findMaxCCFs("new_recovered","new_deaths")
rdlags<- rdlags[!is.nan(rdlags$cor),]
hist(rdlags$lag, plot=TRUE,breaks = 20)
#rdlags[rdlags$lag<=5&rdlags$lag>=0,"PSCR"]
rdlags[rdlags$lag>=0,"lag"] %>% median
rdlags[rdlags$lag>5,"PSCR"]
rdlags[rdlags$lag>10,"PSCR"]
rdlags[order(rdlags$lag,decreasing = TRUE),][1:20,]

dclags<-findMaxCCFs("new_deaths","new_confirmed")
dclags<- dclags[!is.nan(dclags$cor),]
hist(dclags$lag, plot=TRUE,breaks=20)
dclags[dclags$lag>=0,"lag"]%>% median
dclags[order(dclags$lag,decreasing = TRUE),][1:20,]

#bug: instead of cor=NA, and lag=NA, we obtain cor =NAN, lag =-28 when var1 is missing. 
#it seems 21 is the time to recover since being reported for Taiwan*
# Singapore tested the first 100 patients, and all recover by the 31st day. Some the first day! 
# https://towardsdatascience.com/visual-notes-from-singapores-first-100-fully-recovered-covid-19-patients-aad7f2e1d0a0
# majority of patients among the first 100 fully recovered cases were confirmed within 14 days of the reported onset of symptom

#sweden claims patients all recover by the 6th week, 42 days. 
#So I suggest taking lags 42 and 36 (i.e. assume people die within 5 days if they die, which is the median of positive rd lags. Note we do not take the median positive RC lag, which equals 14 (JHH data until 2020-05-18), 
# if we take a smaller lag between r and d, then d might start rising faster than c, which leads to r decreasing over time and that is not possible.the New York,US data we have imputed suffers from this idiosyncracy.  )