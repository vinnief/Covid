# Daily and Monthly Output
author: "Vincent Feltkamp"
output: html_document


```R name="setup" tags=["remove_cell"]
#knitr::opts_chunk$set(echo = TRUE)
R.Version()$version.string
```

Resulting graphs and cleaned and prepared data can be found at 
https://drive.google.com/drive/folders/1Yo0IW4awCIvWMP6jYVKpn3kLgEolse0e?usp=sharing
for publication 
```R name="Init & load latest graph definitions and data" tags=["remove_input"]
#rm(list = setdiff(ls(), c('ECDC0', 'JHH0', 'JHH', 'ECDC', 'JHHRegios', 'ECDCRegios','testing')))
options(warn = 0)
if (!exists('JHH')) source('loadData.R') else 
  if (max(JHH$Date) < Sys.Date() - 1) source('loadData.R')  else source('definitions.R')
save.image(".RData") #D:/gits/Covid19/ #save immediately so that another RMD or Jupyter notebook does not need to redownload. 
tibble(maxECDCdate = max(ECDC$Date), maxJHHdate = max(JHH$Date))# % % "is the last Date for ECDC data, and " % % "is max date for JHH data"
```
Do it separately for ECDC and JHH
```R
dataPath <- './data'
if (!dir.exists(dataPath)) dir.create(dataPath, recursive = TRUE)
if (!exists('ECDC') | (max(ECDC$Date) < Sys.Date() ) ){
  source('loadData.R')  
  ECDC <- loadECDC()  
  writeWithCounters(ECDC,name = "Covid19ECDC")
  } else source('definitions.R') #make sure we have latest definitions. 
if (!exists('JHH') | (max(JHH$Date) < Sys.Date() - 1)) {
  source('loadData.R')  
  JHH <- loadJHH()
  writeWithCounters(JHH,name = "Covid19JHH") 
  }else source('definitions.R') #make sure we have latest definitions. 
  save.image(".RData") #D:/gits/Covid19/ #save immediately so that another RMD or Jupyter notebook does not need to redownload. 
tibble(maxECDCdate = max(ECDC$Date), maxJHHdate = max(JHH$Date))# % % "is the last Date for ECDC data, and " % % "is max date for JHH data"
```
```R
options( repr.plot.width = 6,repr.plot.res = 300)# repr.plot.height = 3)#, repr.plot.res = 200)
```

Graph active_imputed, recovered, deaths, and confirmed for some selected territories, 
based on JHH data:
```R name="demo graph"
graphit(JHH, regios$Vincent, xvar = 'Date', 
        yvars = c('net_active_imputed','new_recovered','new_deaths','new_confirmed'),
        facet = "PSCR", logy = TRUE,from = "2020-09-01")
```
```R
options( repr.plot.width = 5,repr.plot.res = 200)# repr.plot.height = 3)#, repr.plot.res = 200)
```

# test the new labeling of endpoints

```R
source("graphit.R") 
graphit(JHH, regios$Vincent, xvar = 'day', logx =  FALSE , logy = TRUE, from = "2020-08-01",
          yvars = c('new_confirmed_p_M'), labmeth = 'repel_label', putlegend = FALSE)
graphit(JHH, regios$Vincent, xvar = 'day', logx =    FALSE, logy = TRUE, from = "2020-08-01",
          yvars = c('new_confirmed_p_M'), labmeth = 'repel_text')
graphit(JHH, regios$Vincent, xvar = 'day', logx =    FALSE, logy = TRUE, from = "2020-08-01",
          yvars = c('new_confirmed_p_M'), labmeth = 'dl_polygon')
graphit(JHH, regios$Vincent, xvar = 'day', logx =    FALSE, logy = TRUE, from = "2020-08-01",
          yvars = c('new_confirmed_p_M'), labmeth = 'dl_bumpup', putlegend = TRUE)
```

Show table outputs: 
```R name="latest numbers with most growth"
#show some data table output

#JHH[JHH$Date == max(JHH$Date),'Date'][1,1]
JHH %>% filter(Date == max(Date)) %>% filter(!is.nan(new_active_rate)) %>%
  select(PSCR,confirmed, active_imputed, deaths, new_confirmed, new_active_rate, active_imputed_growthRate, 
         active_imputed_p_M) %>% arrange(new_active_rate) %>% tail(10)
```
and Countries we're interested in latest numbers: 
```R
JHH[JHH$Date == max(JHH$Date) & JHH$PSCR %in% 
      c('EU','World',"Kazakhstan",'Belgium','Spain','US','Netherlands','Europe',
        'Germany','France','Africa','Russia','Brazil'),
    c('PSCR','active_imputed','active_imputed_p_M','new_deaths','new_confirmed',
      'new_active_rate', 'active_imputed_growthRate','confirmed') ]  %>%
  arrange(new_active_rate)

```
Who overtakes us based the JHH data set on latest day data? 
```R name="overtaking 1" tags=["remove_cell"] eval=false
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),
        function(x) overtakeDays_df(JHH,x,who = 'theyme',lastDays = 1))
```
And on 7 day averages in JHH? 
```R name="overtaking week based"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),
        function(x) overtakeDays_df(JHH,x,who = 'theyme',lastDays = 7))
```

And according to ECDC? 
```R name="overtaking based on ECDC"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'), 
        function(x) overtakeDays_df(ECDC,x,who = 'theyme',lastDays = 7))
```
Who is more affected? 
```R name="graph it"
graph3Dard_fia(ECDC, c('Kazakhstan','Belgium','Netherlands','Sweden'))
graph3Dard_fina(ECDC, c('Kazakhstan','Belgium','Netherlands','Sweden'))
```
Who do we overtake?
```R name="we overtake"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'Ithem',lastDays = 7))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'Ithem',lastDays = 7))
```
Lets see who is going to overtake the UK, France, or GErmany soon: 
```R
options( repr.plot.width = 5, repr.plot.height = 3, repr.plot.res = 200)# repr.plot.height = 3)#, repr.plot.res = 200)
```

```R name="UK France Germany"
graph3Dard_fia(JHH, c('Germany','France','United Kingdom'))
graph3Dard_fina(JHH, c('Germany','France','United Kingdom'))
map_dfc(c('Germany','France','United Kingdom'), function(x) overtakeDays_df(JHH,x,who = "theyme", lastDays = 7))
map_dfc(c('Germany','France','United Kingdom'), function(x) overtakeDays_df(JHH,x,who = "Ithem", lastDays = 7))
```
Not overtaking anyone does not mean the epidemic is under control. It just means your epidemic is alsready larger than all states that grow slower. It also means you have better control than more severely touched territories. 
For New York state, Indonesia, Peru, India: 
```R name="NY Id In Pe"
graph3Dard_fina(JHH, c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"))
map_dfc(c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"),
        function(x) overtakeDays_df(JHH,x,who = 'Ithem'))
map_dfc(c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"),
        function(x) overtakeDays_df(JHH,x,who = 'theyme'))
```
See how the most affected regions in the world are developing: (pop the graph out to a new window in Rstudio and enlarge to see more detail)
```R name="most affected graph"
graph6Dardcra_finyl(JHH, JHHRegios$`JHH World1`)
```
```R name="deaths and recovered by confirmed"
graph2crd_il(JHH,JHHRegios$`JHH Europe3`)
graph2crd_il(JHH,JHHRegios$`JHH Europe2`)
graph1dnar_iyl(JHH, JHHRegios$`JHH Europe2`)
 graphit(JHH, JHHRegios$`JHH Europe2`, minVal=1, xvar = 'day', 
          yvars = c('new_active_rate'), logy = TRUE, intercept = stableRate) 
```
The motor in all this is the graphit function. It is quite powerful but has a lot of parameters. Several functions preset some parameters, Below a list of Graphs defined. 
```R name="show the system"
setdiff(myGraphList, c(myGraphListbyDate, myGraphListbyDay, myGraphNrs))
myGraphListbyDate #that don't get a number. This because then they do not get executed every day
myGraphListbyDay 
myGraphNrs
```
There is a system in the naming: 
```R
graphCodes()
```
