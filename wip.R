source("requirements.R")
min(covid19[covid19$Date>="2020-01-23",]$new_confirmed) #check the diffs are made well. 
scnc<- covid19[covid19$Date>="2020-01-23",]$new_confirmed

covid19[covid19$new_confirmed==min(covid19$new_confirmed),]$Date

countNA<- function(varname="recovered",lpdf=covid19){
  cna<- ddply(lpdf, "CRPS", 
        function (lpdf){ sum(is.na(lpdf[,varname]))  }
        )
  names(cna)[names(cna)=="V1"]<- "NA.count"
  cna
}

countsr<- countNA("recovered")
(countsr[countsr$NA.count!=0,"CRPS"])

noNA<- function(varname="recovered",lpdf=covid19){
  counts<- countNA(varname)
  covid19[covid19$CRPS %in% counts[counts$NA.count==0,"CRPS"],]
}
noNA()$CRPS  # the ones who have no missing recovery cases. 
ccf(x, y) #cross correlation. gives a graph showing the major lagged correlations
all(noNA("new_recovered")$CRPS==noNA()$CRPS)
length(unique(noNA("new_recovered")$CRPS))
length(unique(noNA("new_confirmed")$CRPS))
length(unique(noNA("new_deaths")$CRPS))
noNAnr<- noNA("new_recovered")
#it needs a full df, no na!
ccf(noNAnr$new_confirmed, noNAnr$new_recovered,lag_max=10)
ccf(noNAnr[noNAnr$Date>"2020-01-22",]$new_confirmed, noNAnr[noNAnr$Date>"2020-01-22",]$new_deaths,lag_max=20)
min(extravars(covid19)$new_confirmed)
?pacf

?ccf
?plot
ccf.vf()
ccf.vf(var1="new_deaths")
ccf.vf(var2="new_deaths")
ccf.vf(var2="new_deaths",CRPS="Italy",saveit=TRUE)
dev.off()
ccf.vf(var1="new_deaths",CRPS="Spain",saveit=TRUE)
ccf.vf(var1="new_deaths",CRPS="France",saveit=TRUE)
ccf.vf(var1="new_deaths",CRPS="Germany",saveit=TRUE)
ccf.vf(var1="new_deaths",CRPS="Spain",saveit=TRUE)
ccf.vf(var1="new_deaths",CRPS="Korea, South",saveit=TRUE)
pacf(x, lag.max, plot, na.action, ...)
a<- c(1:10,10:1,1:5,5:1)
b<- c(1,2,1,lag(a,3))[1:30]
b
print(ccf(b,a))

##############
`%+%`<- function(x,y){paste(x,y,sep="")}

imputeRecovered<- function(lpdf=covid19,lagc=0,lagd=0,lagr=0){
  if(!all(is.na(lpdf$recovered))) lpdf$recovered_old<- lpdf$recovered
  lagcS<- ifelse(lagc>=0,as.character(lagc),"_"%+%as.character(-lagc))
  lagrS<- ifelse(lagr>=0,as.character(lagr),"_"%+%as.character(-lagr))
  lpdf$recovered<- lpdf[,"recovered" %+% lagcS %+% "." %+% lagrS]<- 
    lag(lpdf$confirmed,lagc)- lag(lpdf$deaths,lagr)
  if(!all(is.na(lpdf$active))) lpdf$active_old<- lpdf$active
  lpdf$active<- lpdf[,"active" %+% lagcS %+% "." %+% lagrS]<- 
    lpdf$confirmed - 
    lpdf[,"recovered" %+% lagcS %+% "." %+% lagrS]
  lpdf
}
USSData<- datasel("US",0,id="Country.Region")

USSData<- imputeRecovered(USData, 21,7)

graph3(US,"US",1,id="CRPS",lpdf=USData)
################pdataframe
require(plm)
covid19<- pdata.frame(covid19,index=c("CRPS", "Date"),stringsAsFactors=FALSE)

