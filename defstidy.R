source("requirements.R")

if(!require(tidyverse)) {install.packages("tidyverse");require(tidyverse)}
#if(!require(dplyr)){ installed.packages("dplyr"); require(dplyr)} #note dplyr has sometimes conflicting packages. so be careful not to confuse them.
### data from ECDC - World bank. 
### https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
### my first try at the tidyverse! 
library(utils)
makeecdc<- function (){
  ecdcdata <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na = "" ) %>%  #fileEncoding = "UTF-8-BOM" doesn use bom in readr tidyverse. 
    mutate( CRPS=countriesAndTerritories, 
            confirmed_today=cases, 
            deaths_today=deaths,
            Date= as.Date(dateRep, format="%d/%m/%Y"),
            population= popData2018, popData2018=NULL) %>%
    select(-geoId, -day, -month, -year, -cases ,-countriesAndTerritories,-dateRep,
              -countryterritoryCode) %>%  
    arrange(CRPS,Date) %>% group_by(CRPS) %>% 
    mutate(confirmed = cumsum(confirmed_today),
           deaths = cumsum(deaths_today))%>%
    mutate(recovered=as.numeric(NA))
}
ecdcdata <- makeecdc()
ecdcdata
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}

impute<- function(lpdf=ecdcdata,varname="recovered", lagrc=22,lagrd=15,
                  dothese=FALSE,redo=FALSE){
  if(!(varname%in% names(lpdf))) lpdf$recovered<- as.numeric(NA)
  if(any(dothese)) lpdf[,varname%+% "_old"]<- lpdf[,varname]
  if (!"imputed"%in% names(lpdf))lpdf$imputed<-FALSE
  rowstodo<- drop(is.na(lpdf$recovered)|dothese|(redo & lpdf$imputed))
  if (verbose>=2) print(paste("imputing recovered for:",paste(unique(lpdf[rowstodo,"CRPS"]),collapse=" / ")))
  if (sum(rowstodo)==0)return(lpdf)
  lpdf<- lpdf%>% group_by(CRPS) %>%
            mutate_cond(rowstodo,imputed=TRUE )%>% 
            mutate_cond(rowstodo, recovered= dplyr::lag(confirmed,lagrc)- dplyr::lag(deaths,lagrd) )
}
verbose=1
ecdcdata<- ecdcdata %>% impute() #the lag=22 becomes lag =0 if i use plm::lag. strange. soit. 
ecdcdata

frac<- function (n,d){ifelse(n>0, n/d, NA)}
diff.sl<- function(avector,n=1){c(rep(NA,n),diff(avector,n))}

extravars2<- function(lpdf,lagrc=0,lagdc=0){
  lpdf<- lpdf    %>%  #ungroup()%>% #mutate(CRPS= sortCountries(lpdf))%>% #the CRPS is now a factor! sorted on decreasing confirmed and CRPS! 
      arrange(CRPS, Date) %>%
      group_by(CRPS) %>% 
      mutate(active = confirmed - deaths - recovered,
                 new_confirmed=ma(diff.sl(confirmed)), new_active=ma(diff.sl(active)),
                new_recovered=ma(diff.sl(recovered)), new_deaths=ma(diff.sl(deaths)),
                confirmed_pM = 1000000*confirmed/population,
                active_pM    = 1000000* active  /population,
                recovered_pM = 1000000*recovered/population,
                deaths_pM    = 1000000*deaths   /population,
                new_confirmed_pM = 1000000*new_confirmed/population,
                new_active_pM    = 1000000*new_active  /population,
                new_recovered_pM = 1000000*new_recovered/population,
                new_deaths_pM    = 1000000*new_deaths  /population,
                recovered_per_confirmed= frac(recovered,dplyr::lag(confirmed,lagrc)),
                deaths_per_confirmed= frac(deaths,dplyr::lag(confirmed,lagdc)),
                recovered_per_deaths= frac(recovered,dplyr::lag(deaths,lagrc-lagdc))#,
                
             #Date= as.Date(lpdf$Date,format="%Y-%m-%d")
                #should not be needed as tibbles have no factors.  #otherwise we cannot calculate the first date that confirmed is over minval. 
              )
}
ecdcdata <- makeecdc()
ecdcdata1<- ecdcdata %>%  extravars2()  #note : check that new_confirmed = conf today. approximately (we did running average after all! )
rm(ecdcdata2)
eWorldCRPSList<- unique(ecdcdata$CRPS)
eWorldCRPSList  # from here bugs exist
# dynamic varnames in select: ## !!varname := 
total<- function(rows="", #rewrite this tibblewise with group_by! then totals and total will collapse into one function. 
                 id="CRPS", newrow="" ,lpdf=covid19,
                 varnames=c("confirmed","deaths","recovered")  
) {
  ans<- ddply(lpdf[lpdf[,id] %in% rows,],c("Date"),
              function(a) {
                Country.Region=ifelse(newrow!="", newrow,
                                      aggreg(as.character(a$Country.Region)))
                Province.State=ifelse(newrow!="",newrow,aggreg(a$Province.State))
                CRPS=ifelse(newrow!="",
                            newrow,
                            ifelse(id=="CRPS",
                                   aggreg(a$CRPS) ,
                                   ifelse(Province.State=="",
                                          Country.Region,
                                          paste(Province.State,Country.Region,sep=', ')
                                   )
                            )
                )
                b1<- colSums(a[,varnames],na.rm=TRUE)
                nam<- names(b1)
                dim(b1)<- c(1,length(b1))
                b2<-data.frame(b1)
                colnames(b2)<- nam
                ans<- cbind(Country.Region,
                            CRPS,
                            Province.State,
                            Lat=mean(a$Lat) ,Long=mean(a$Long),
                            b2
                )
                if("imputed"%in% names(lpdf)) ans$imputed<- any(lpdf$imputed)
                ans
              }) #,County=aggreg(a$County)#Region=aggreg(a$Region)
  ans[,setdiff(names(lpdf),names(ans))]<-NA
  ans
}
regtots<- rbind(extravars2(total(eWorldCRPSList,id="CRPS",newrow="World",lpdf=ecdcdata1,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(total(Europe,id="CRPS",newrow="Europe",lpdf=ecdcdata1,
                                varnames= c("confirmed","recovered", "deaths","population" ))),
                extravars2(total(Africa,id="CRPS",newrow="Africa",lpdf=ecdcdata1,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(total(Asia,id="CRPS",newrow="Asia",lpdf=ecdcdata1,
                                varnames= c("confirmed","recovered", "deaths","population"))),
                extravars2(total(SAmerica,id="CRPS",newrow="South America",lpdf=ecdcdata1,
                                varnames= c("confirmed","recovered", "deaths","population")))
                )


ecdcdata1<- rbind(ecdcdata1,regtots)
