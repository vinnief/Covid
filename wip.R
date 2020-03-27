alldata<- makeGroups( mklpdf())
sum(is.na(deaths))
unique(confirmed$CRPS)
unique(recovered$CRPS)
names(confirmed)
names(deaths)
names(recovered)
sum(is.na(alldata))
View(alldata[is.na(alldata$confirmed),])
View(alldata[is.na(alldata$recovered),])
View(confirmed[is.na(alldata$recovered),])
  
  
Cabbrevs<- data.frame(abr=abbreviate(unique(alldata$Country.Region),minlength=1,method="left.kept"),
                      country=unique(alldata$Country.Region))
Pabbrevs<- data.frame(abr=abbreviate(unique(alldata$Province.State),minlength=1,method="left.kept"),
                      country=unique(alldata$Province.State))

  coltype="date"; values.name="count"
wpdf=wc
wc$CRPS <- (ifelse(""==wpdf$Province.State, 
                     as.character(wpdf$Country.Region),
                     paste(wpdf$Province.State,wpdf$Country.Region,sep=', ')))


lpdf <- addcounterfrommin(minval,
                          View(
                            datasel(countries,minval=20,id=ID, lpdf=lpdf,varname=varnames[1],fuzzy=FALSE)
                          )
                          ,id=ID,counter=countname)


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

################pdataframe
require(plm)
#lpdf<- pdata.frame(alldata,index=c("CRPS", "Date"))
lpdf$currentc<- diff(lpdf$confirmed)

alldata$newc<- c(0,diff(alldata$confirmed))
  alldata$newd<- c(0,diff(alldata$deaths))
  alldata$newr<- c(0,diff(alldata$recovered))
graphit2(WestvsEast,varnames=c("confirmed","newc"),size=2)
graphit2(smallEurope,varnames=c("confirmed","newc"),size=2)
lpdf$recoveredOverDeaths <-with ( lpdf, ifelse(deaths>0,recovered/lag(deaths,10), NA))
graphit2("Germany", 500, lpdf = lpdf,varnames="recoveredOverDeaths") 
graphit2("Germany", 500,            ,varnames="recoveredOverDeaths")
