# Daily and Monthly Output
author: "Vincent Feltkamp"
output: html_document

```R name="setup" tags=["remove_cell"]
#knitr::opts_chunk$set(echo = TRUE)
R.Version()$version.string
```
Resulting graphs and cleaned and prepared data can be found at 
https://drive.google.com/drive/folders/1Yo0IW4awCIvWMP6jYVKpn3kLgEolse0e?usp=sharing


## load the data
```R name="initialize" tags=["remove_cell"]
verbose = 2
source("loadData.R")
theLog = file("output.log", open = "wt")
sink(theLog, type = "message")

```
###  Plot options for Jupyter notebooks
```R name="extra wide" tags=["remove_cell"] eval=false
options( repr.plot.width = 6,repr.plot.res = 300, repr.plot.height = 3)#, repr.plot.res = 200)
```
```R name="middle wide" eval=false
options( repr.plot.width = 5,repr.plot.res = 200, repr.plot.height = 3)
```
## a Danny Dorling plot: val by Delta val, for active_imputed. It shows vertical zig zags
```R
graph1cd_finl(JHH, JHH.Regios$`JHH Europe2`, size =1, scales = "fixed", logy = T, logx = T, slope = deathRate) # savename = "Europe2",
graph1cd_finl(JHH, JHH.Regios$`JHH Europe1`, size =1, scales = "fixed", slope = deathRate)
graph1cd_finl(JHH, JHH.Regios$`JHH World1`, size = 1, scales = "fixed" , slope = deathRate)#, savename = "World1")

```
```R
lifecycle::last_warnings()


```

## Graph active_imputed, recovered, deaths, and confirmed for some selected territories, 
based on JHH data:
```R name="demo graph"
graphit(JHH, regios$Vincent, xvar = 'theDate', 
        yvars = c('net_active_imputed','new_recovered_imputed','new_deaths','new_confirmed'), smoothn = 7,
        facet = "PSCR", logy = TRUE,from = "2020-09-01")

```
## Descriptive stats
### fastest growth
```R name="latest numbers with most growth"
JHH %>% filter(theDate == max(theDate)) %>% filter(!is.nan(new_active_rate)) %>%
  select(PSCR,confirmed, active_imputed, deaths, new_confirmed, new_active_rate, active_imputed_growthRate, 
         active_imputed_p_M) %>% arrange(new_active_rate) %>% tail(10)
```
```R name="overtaking week based"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),
        function(x) overtakeDays_df(JHH,x,who = 'theyme',lastDays = 7))
```

### Who overtakes, in 7 day averages according to the ECDC? 
```R name="overtaking based on ECDC"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'), 
        function(x) overtakeDays_df(ECDC,x,who = 'theyme',lastDays = 7))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'), 
        function(x) overtakeDays_df(ECDC %>% smoothem.lpti(n=7),x,who = 'theyme',lastDays = 1))

```
### Who is more affected among Kz, S, Nl, Be? 
```R name="graph it"
graph3Dard_fia(JHH, c('Kazakhstan','Belgium','Netherlands','Sweden'), scales = "fixed")
graph3Dard_fia(ECDC, c('Kazakhstan','Belgium','Netherlands','Sweden'), scales = "fixed")
graph3Dard_fina(ECDC, c('Kazakhstan','Belgium','Netherlands','Sweden'), scales = "fixed")
```
```R
graph3Dard_fina(ECDC, c('Kazakhstan','Belgium','Netherlands','Sweden'), scales = "fixed")
```

### Who do Kz, Nl, S, Be overtake?
```R name="we overtake"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'Ithem',lastDays = 7))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'Ithem',lastDays = 7))
```
```R
options( repr.plot.width = 5, repr.plot.height = 3, repr.plot.res = 200)# repr.plot.height = 3)#, repr.plot.res = 200)
```

### How are UK, France & Germany doing? 
```R
graph3Dard_fia(JHH, c('Germany','France','United Kingdom'))
graph3Dard_fina(JHH, c('Germany','France','United Kingdom'))
map_dfc(c('Germany','France','United Kingdom'), function(x) overtakeDays_df(JHH,x,who = "theyme", lastDays = 7))
map_dfc(c('Germany','France','United Kingdom'), function(x) overtakeDays_df(JHH,x,who = "Ithem", lastDays = 7))
```
Note: Not overtaking anyone does not mean the epidemic is under control. It just means your epidemic is alsready larger than all states that grow slower. It also means you have better control than more severely touched territories. 
### State of New York state, Indonesia, Peru, India: 
```R name="NY Id In Pe"
graph3Dard_fina(JHH %>% smoothem.lpti(n = 7), c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"))
map_dfc(c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"),
        function(x) overtakeDays_df(JHH,x,who = 'Ithem'))
map_dfc(c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"),
        function(x) overtakeDays_df(JHH,x,who = 'theyme'))
```
### The most affected regions in the world 
(pop the graph out to a new window in Rstudio and enlarge to see more detail)
```R name="most affected graph"
graph6Dardcra_finyl(JHH , JHH.Regios$`JHH World1`)
graph6Dardcra_finyl(JHH, JHH.Regios$`JHH World1`)
```
```R name="deaths and recovered by confirmed"
graph2crd_il(JHH,JHH.Regios$`JHH Europe3`)
graph2crd_il(JHH,JHH.Regios$`JHH Europe2`)
graph1dnar_iyl(JHH, JHH.Regios$`JHH Europe2`)
graphit(JHH, JHH.Regios$`JHH Europe2`, minVal=1, xvar = 'day', 
          yvars = c('new_active_rate'), logy = TRUE, smoothn = 7, intercept = stableRate) 
graphit(JHH, JHH.Regios$`JHH Europe2`, minVal=1, xvar = 'day', 
          yvars = c('new_active_rate'), logy = TRUE, intercept = stableRate) 

```
Note: the motor in all this is the graphit function. It is quite powerful but has a lot of parameters. Several functions preset some parameters, Below a list of Graphs defined. 
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
