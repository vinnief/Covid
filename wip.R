
#covid19$o<-reorder(covid19$CRPS,(ordre))

################pdataframe
require(plm)
covid19<- pdata.frame(covid19,index=c("CRPS", "Date"),stringsAsFactors=FALSE)

