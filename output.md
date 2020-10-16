# Daily and Monthly Output
author: "Vincent Feltkamp"
output: html_document

```R name="setup" tags=["remove_cell"]
#knitr::opts_chunk$set(echo = TRUE)
R.Version()$version.string
```
Resulting graphs and cleaned and prepared data can be found at 
https://drive.google.com/drive/folders/1Yo0IW4awCIvWMP6jYVKpn3kLgEolse0e?usp=sharing

te))#

## load the data
```R name="initialize"
verbose = 2
source("loadData.R")
```
###  Plot options for Jupyter notebooks
```R eval=false tags=["remove_cell"]
options( repr.plot.width )
```
```R name="extra wide" eval=false tags=["remove_cell"]
options( repr.plot.width = 6,repr.plot.res = 300)# repr.plot.height = 3)#, repr.plot.res = 200)
```
```R name="middle wide" eval=false
options( repr.plot.width = 5,repr.plot.res = 200)# repr.plot.height = 3)#, repr.plot.res = 200)
```
## a Danny Dorling plot: val by Delta val, for active_imputed. It shows vertical zig zags
```R
graph1aa_inl(JHH, c("Belgium","Netherlands","Sweden"), smoothvars = "net_active_imputed", smoothn=7)
graphdanny(JHH , c("France", "Germany"), smoothn  = 7)
graphdanny(JHH , c("France", "Germany"))
graphdanny(JHH , c( "Germany"), smoothn  = 7)
```
## Graph active_imputed, recovered, deaths, and confirmed for some selected territories, 
based on JHH data:
```R name="demo graph"
graphit(JHH, regios$Vincent, xvar = 'theDate', 
        yvars = c('net_active_imputed','new_recovered','new_deaths','new_confirmed'),
        facet = "PSCR", logy = TRUE,from = "2020-09-01")
```
## test the new labeling of endpoints

## Descriptive stats
### fastest growth
```R name="latest numbers with most growth"
JHH %>% filter(theDate == max(theDate)) %>% filter(!is.nan(new_active_rate)) %>%
  select(PSCR,confirmed, active_imputed, deaths, new_confirmed, new_active_rate, active_imputed_growthRate, 
         active_imputed_p_M) %>% arrange(new_active_rate) %>% tail(10)
```
### Interesting Countries latest numbers: 
```R
Sys.Date() %>% print
JHH[JHH$theDate == max(JHH$theDate) & JHH$PSCR %in% 
      c('EU','World',"Kazakhstan",'Belgium','Spain','US','Netherlands','Europe',
        'Germany','France','Africa','Russia','Brazil'),
    c('PSCR','active_imputed','active_imputed_p_M','new_deaths','new_confirmed',
      'new_active_rate', 'active_imputed_growthRate','confirmed') ]  %>%
  arrange(new_active_rate)

```
### Who overtakes us based on the JHH data set on latest day? 
```R name="overtaking 1" tags=["remove_cell"] eval=false
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),
        function(x) overtakeDays_df(JHH,x,who = 'theyme',lastDays = 1))
```
### Who overtakes, in 7 day averages averages according to the JHH? 
```R name="overtaking week based"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),
        function(x) overtakeDays_df(JHH,x,who = 'theyme',lastDays = 7))
```

### Who overtakes, in 7 day averages according to the ECDC? 
```R name="overtaking based on ECDC"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'), 
        function(x) overtakeDays_df(ECDC,x,who = 'theyme',lastDays = 7))
```
### Who is more affected among Kz, S, Nl, Be? 
```R name="graph it"
graph3Dard_fia(ECDC, c('Kazakhstan','Belgium','Netherlands','Sweden'))
graph3Dard_fina(ECDC, c('Kazakhstan','Belgium','Netherlands','Sweden'))
```
### Who do Kz, Nl, S, Be overtake?
```R name="we overtake"
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(JHH,x,who = 'Ithem',lastDays = 7))
map_dfc(c('Kazakhstan','Belgium','Netherlands','Sweden'),function(x) overtakeDays_df(ECDC,x,who = 'Ithem',lastDays = 7))
```
### Who overtakes the UK, France, or Germany soon: 
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
graph3Dard_fina(JHH, c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"))
map_dfc(c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"),
        function(x) overtakeDays_df(JHH,x,who = 'Ithem'))
map_dfc(c('New York,US','California,US','Florida,US','Texas,US','Peru',"Arizona,US"),
        function(x) overtakeDays_df(JHH,x,who = 'theyme'))
```
### The most affected regions in the world 
(pop the graph out to a new window in Rstudio and enlarge to see more detail)
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
