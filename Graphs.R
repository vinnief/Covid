#for publication #MSM,Vincent,
rm(list=ls())

while((Sys.time()>Sys.Date()% % "22:00:00")| max(JHH$Date)<Sys.Date()-1 ) {
  source("loadData.R")  #also loads the requirements and the definitions 
    if (max(JHH$Date)< Sys.Date()-1)     {
      print( "failed to get yesterday's values ") 
      for (i in 1:12){
        print(Sys.time())
        Sys.sleep(600)
  }   } }

#makeDyn Regions sorts by confirmed and countries get added regularly. 
# so the next 4 lines need to be done on the latest data!
ECDCRegios <- ECDC %>% makeDynRegions(piecename='ECDC world')
verbose=2
profvis(
  makeHistoryGraphs(ECDC,regions=ECDCRegios)#
)
# 
writeRegioGraph(ECDC,ECDCRegios)
profvis(JHHRegios <- makeRegioList(JHH))
makeHistoryGraphs(JHH,regions=JHHRegios)#
JHH%>%writeRegiograph(JHHRegios)

ECDC %>% writeRegiograph(ECDCRegios)
graphcodes()
mygraphlist
verbose=2



#check colors
graph3Dard_fia (JHH,JHHRegios$continents)


JHH%>% makehistory(regions=JHHRegios, dates=seq(Sys.Date()-0,Sys.Date()-200,-10))  
ECDC%>% makehistory(regions=ECDCRegios, dates=seq(Sys.Date()-210,Sys.Date()-0,30))  
JHH %>% makehistory(regions=JHHRegios,graphlist = 'graphDccp_yfl')



####corrections to earlier graphs

verbose=1
JHH%>% makehistory(regions=JHHRegios, dates="2020-05-06") #[c('Africa','SAmerica','Caribbean','OceaniaP')])
ECDC%>% makehistory(regions=ECDCRegios, dates="2020-05-06") 


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
# if we take a smaller lag between r and d, then d might start rising faster than c, which leads to r decreasing over time and that is not possible. New York suffers from this idiosyncracy.  )