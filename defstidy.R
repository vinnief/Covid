source("requirements.R")
source("definitions2.R")
#if(!require(tidyverse)) {install.packages("tidyverse");require(tidyverse)}
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



verbose=2
ecdcdata<- ecdcdata %>% imputeRecovered2() %>% ungroup()

frac<- function (n,d){ifelse(n>0, n/d, NA)}

diff.sl<- function(avector,n=1){c(rep(NA,n),diff(avector,n))}
#ecdcdata$CRPS<- ecdcdata$CRPS2
ecdcdata$CRPS2<- sortCRPS(as.data.frame(ecdcdata) )
ecdcdata$CRPS1<- sortCRPS1(ecdcdata)
ecdcdata[is.na(ecdcdata$CRPS),]
ecdcdata[ecdcdata$CRPS2!=ecdcdata$CRPS1,c("CRPS",'CRPS1','CRPS2','confirmed' )]
ecdcdata  %<>% 
  filter( is.na(CRPS2) | (CRPS1!= CRPS2)) %>% 
  select(CRPS, CRPS1,CRPS2, Date,confirmed) # %>% 
ecdcdata%>%  summary("CRPS")



#ecdcdata <- makeecdc()%>% imputeRecovered2()
ecdcdata<- ecdcdata %>%  extravars2()

ecdcCRPSlist<- makeDynRegions(ecdcdata)
writeRegiograph(ecdcCRPSlist,lpdf=ecdcdata)

eWorldCRPSList<- unique(ecdcdata$CRPS)
eWorldCRPSList  # from here bugs exist
graph0("France", lpdf=as.data.frame(ecdcdata))

devtools::install_github("dgrtwo/gganimate")# and install image_magick. 
# see http://www.ggtern.com/2017/07/23/version-2-2-1-released/
# add  frame = Date  to aes of ggplot, then
# gganimate(myplot, "filename.gif" )  # or mp4, html, swf. 
# p3 <- ggplot(gapminder, aes(gdpPercap, lifeExp, frame = year)) +
#geom_path(aes(cumulative = TRUE, group = country)) +
#  scale_x_log10() +
#  facet_wrap(~continent)
#12 ggplot extensions: ggcorplot, 
#https://mode.com/blog/r-ggplot-extension-packages
#
# dynamic varnames in select: ## !!varname := 
# !!varname does not work the same as confirmed in arrange(desc(!!varname))
# arrange( desc(eval(parse(text=substiture(!!varname)))))  # works. 
# CRAZY!!
# 
# 
# 
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
