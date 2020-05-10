#for publication #MSM,Vincent,
source ("requirements.R")#load libraries
source("definitions2.R")# make sure we have our definitions. 

while((Sys.time()>Sys.Date()% % "22:00:00")| max(JHH$Date)<Sys.Date()-1 ) {
  source("loadData.R")  #this loads the requirements and the definitions also. 
    if (max(JHH$Date)< Sys.Date()-1)     {
    print( "failed to get yesterday's values ") 
    for (i in 1:12){
      print(Sys.time())
      Sys.sleep(600)
    }
  }
}

#makeDyn Regions sorts by confirmed! so needs to be done on the latest data!
JHHRegios <- JHH%>% makeregios()
ecdcregios <- ecdc %>% makeDynRegions2(piecename='ecdc world')
verbose=2

JHH%>% makehistory(regions=JHHRegios)#,graphnrs)

verbose=2
JHH%>% makehistory(regions=JHHRegios,dates="2020-05-07")
#crashes on graph00l
ecdc%>% makehistory(regions=ecdcregios, dates=seq(Sys.Date()-11,Sys.Date()-100,-10)) # bug crashes 2020-04-09 on graph00l a+r+c+d allinone log line on the first region ecdcworld1. 

####corrections
####
verbose=1
JHH%>% makehistory(regions=JHHRegios, dates="2020-04-25")
ecdc%>% makehistory(regions=ecdcRegios,, dates="2020-05-06")

makehistory(dates=seq(Sys.Date()-11,Sys.Date()-100,-10))  

makehistory(regions=JHHregios, seq(Sys.Date()-11,Sys.Date()-100,-10),nrs=c(0,2,4,5,6:9))  

### find mac ccf per country. should be around 21 or at least 15. it is much less!
#are diffs made properly? 
all(is.na(JHH[JHH$Date=="2020-01-22", "new_confirmed"]))  # should be true
findMaxCCF(myCRPS="Italy")
findMaxCCF(myCRPS="Hubei, China")
findMaxCCFs(myCRPS="Hubei, China")
findMaxCCFs(myCRPS="Taiwan*")
rclags<-findMaxCCFs("new_recovered","new_confirmed")
rclags<- rclags[!is.nan(rclags$cor),]
hist(rclags$lag, plot=TRUE,breaks=20)
rclags[rclags$lag<=10&rclags$lag>=0,"CRPS"]
rclags[order(rclags$lag,decreasing = TRUE),][1:20,]
dothese<- JHH$Country.Region=="Netherlands"
rdlags<-findMaxCCFs("new_recovered","new_deaths")
rdlags<- rdlags[!is.nan(rdlags$cor),]
hist(rdlags$lag, plot=TRUE,breaks = 20)
rdlags[rdlags$lag<=5&rdlags$lag>=0,"CRPS"]
rdlags[rdlags$lag<0,"CRPS"]
rdlags[rdlags$lag>5,"CRPS"]
rdlags[rdlags$lag>10,"CRPS"]
rdlags[order(rdlags$lag,decreasing = TRUE),][1:20,]

dclags<-findMaxCCFs("new_deaths","new_confirmed")
dclags<- dclags[!is.nan(dclags$cor),]
hist(dclags$lag, plot=TRUE,breaks=20)
dclags[dclags$lag>=10,"CRPS"]
rdlags[order(rdlags$lag,decreasing = TRUE),][1:20,]

#bug: instead of cor=NA, and lag=NA, we obtain cor =NAN, lag =-28 when var1 is missing. 
#it seems 21 is the time to recover since being reported. for Taiwan*
# Singapore tested the first 100 patients, and all recover by the 31st day. Some the first day! 
# https://towardsdatascience.com/visual-notes-from-singapores-first-100-fully-recovered-covid-19-patients-aad7f2e1d0a0
# majority of patients among the first 100 fully recovered cases were confirmed within 14 days of the reported onset of symptom
