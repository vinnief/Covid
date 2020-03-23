######################### ################  wip make totals. actually useless: Tableau does it better. 
######################### 
aggreg<- function(kol,sep="-"){
  if (kol[1]==kol[length(kol)])  {kol[1]}
  else {paste(kol[1],length(unique(kol))-2,kol[length(kol)],sep=sep)}
}
aggreg(paste(1:5,"ne"))

all.equal(rep("ne",5),lag(rep("ne",5)))
totals<- function(rows=c("Italy","Belgium"), id="CRPS", 
                  varnames=c("confirmed","deaths","recovered"),lpdf=alldata){
  ddply(lpdf[lpdf[,id,] %in% rows,],c("Date", id),
        function(a) {c(Country.Region=aggreg(a$Country.Region),CRPS=aggreg(a$CRPS),State=aggreg(a$State),"County"=aggreg(a$County),apply(a[varnames],2,sum))})
}
head(totals())
head(totals("Netherlands",id="Country.Region"))
head(totals(c("Germany","France"),id="Country.Region"))
totals(c("Germany","France, France"))#,varnames="confirmed")
totals("US",id="Country.Region")
alldata[alldata$Country.Region=="France","State"]



