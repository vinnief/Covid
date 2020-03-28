alldata<- makeGroups( mklpdf())


### in case you want to make the size of the line depend on the name of the data var
sizes=data.frame(confirmed=3, deaths=1, recovered=2,recoveredOverConfirmed=3,deathsOverConfirmed=4,recoveredOverDeaths=2)


####### test what goes wrong

#### test
##### unitTESTS
## colors and plots
emf("simple_graphic.emf", width=10, height=8)
plot(c(1.1,2,2,3,3));#plot(2.2);#plot("hello")
dev.off()
svg(filename="simple_graphic.svg", width=10, height=8)
plot(c(1.1,2,2,3,3));plot(2.2);#plot("hello")
dev.off()

display.brewer.all(colorblindFriendly = TRUE)
display.brewer.all(colorblindFriendly = FALSE)
display.brewer.pal(1,palette)
display.brewer.pal(12,"Paired")


## automate country abbrevs  
Cabbrevs<- data.frame(abr=abbreviate(unique(alldata$Country.Region),minlength=1,method="left.kept"),
                      country=unique(alldata$Country.Region))
Pabbrevs<- data.frame(abr=abbreviate(unique(alldata$Province.State),minlength=1,method="left.kept"),
                      country=unique(alldata$Province.State))
#while at it, maybe also make population a variable, and then cases/pop

######################### ################  wip make totals. actually useless: Tableau does it better. 
######################### 

aggreg<- function(kol,sep="_"){
  endval<- ifelse (is.na(kol[NROW(kol)]),"NA",kol[NROW(kol)])
    if (kol[1]==endval)  {kol[1]}
  else {paste(kol[1],length(unique(kol))-2,endval,sep=sep)}
}
lpdf<- datasel(c("Alabama, US","California, US"),id="CRPS",lpdf=lpdf)
NROW(lpdf)
View(lpdf)
aggreg(datasel("US",id="Country.Region",lpdf=lpdf)$CRPS)
totals<- function(rows=c("US","China"), id="Country.Region", 
                  varnames=c("confirmed","deaths","recovered","new_confirmed","new_deaths","new_recovered"),lpdf=alldata){
  ddply(lpdf[lpdf[,id] %in% rows,],c("Date", id),
        function(a) {c(Country.Region=aggreg(a$Country.Region),CRPS=aggreg(a$CRPS),State=aggreg(a$State),County=aggreg(a$County),colSums(a[,varnames]))})
}
NROW(lpdf)

head(totals())
head(totals("Netherlands",id="Country.Region"))
head(totals(c("Germany","France"),id="Country.Region"))
totals(c("Germany","France, France"))#,varnames="confirmed")
totals("US",id="Country.Region")
(alldata[alldata$Country.Region=="France","State"])

lpdf=totals(rows="France", id="Country.Region")
names(lpdf)
lpdf <- addcounterfrommin(10,
                          datasel("France",minval=0,id=, lpdf=lpdf,varname=varnames[1],fuzzy=FALSE)
                          ,id=ID,counter=countname)
graphit4("California", ID="State", minval=1, lpdf=totals(rows="California", id="State"), )
graphit2(c("California","New York"),lpdf=totals(rows="US", id="CRPS"),ID="State")
graphit2("US",ID="Country.Region",lpdf=totals(rows="US", id="Country.Region"))



graphit2("France",ID="Country.Region",lpdf=totals(rows="France", id="Country.Region"))






################pdataframe

#alldata$new_confirmed<- c(0,diff(alldata$confirmed)) # without Pdf, the first data point disappears. 
#  alldata$new_d<- c(0,diff(alldata$deaths))
#  alldata$new_r<- c(0,diff(alldata$recovered))
require(plm)
lpdf<- datasel(c("Italy","France"),id="CRPS",lpdf=alldata)
typeof(lpdf$Date)
lpdf$fDate<-lpdf$Date
typeof(lpdf$fDate)
lpdf<- pdata.frame(alldata,index=c("CRPS", "fDate"),stringsAsFactors=FALSE)
lpdf$oDate<- as.ordered(lpdf$Date)

lpdf$new_confirmed<- diff(lpdf$confirmed)
lpdf$new_deaths<- diff(lpdf$deaths)
lpdf$new_recovered<- diff(lpdf$recovered)

names(lpdf)
lpdf$rod<- lag(lpdf$recovered,-10)/lpdf$deaths
lpdf$roc<- lpdf$recovered/lag(lpdf$confirmed,21)
lag.plot(lpdf[lpdf$Country.Region=="Italy",c("new_confirmed","new_recovered","new_deaths")],4,na.omit=TRUE)
graphit2("Italy",10,lpdf=lpdf,varnames=c("roc","rod"),legend=FALSE)
                
