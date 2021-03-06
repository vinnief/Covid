---
title: "__Covid Trendlines__"
author: "VF"
date: "March- July 2020"
output: 
  html_document: 
    fig_caption: yes
    number_sections: yes
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
WIP branch: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/vinnief/Covid/wip)
 
 The notebook: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/vinnief/Covid/wip?filepath=output.ipynb)

 The RMD: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/vinnief/Covid/wip?filepath=output.Rmd)
 
# __Covid19 Trendlines__ using R

The presentation of the Covid19 situation in the news often consists of just three numbers for your own country: total confirmed, new confirmed, and deaths, 
or a world map with bubbles, the size of which symbolizes the number of confirmed. 

This does not tell much about the general trends nor the severity of the situation. 
Here an attempt at making trendlines for all countries in the world, plus some states and territories that make their info available and which is collected by either ECDC.europa.eu or the John Hopkins Hospital and shared by their 
Computer Center on GitHub.com. 

The resulting graphs and cleaned and extended data can be found at 
https://drive.google.com/drive/folders/1Yo0IW4awCIvWMP6jYVKpn3kLgEolse0e?usp=sharing. 
The data is also in this repo. 

This is a descriptive analysis of the timeline and area graphs in R. 
An attempt at simulating what would have happened in Excel is available, and I initially tried to do this with Julia, but had trouble getting plotting to work. It would be interesting to see how much faster Julia can do these things. 

##_Approach_: 
the JHH panel data is in csv format, row=country, column= date,
and we melt it into a long version, with on column per count vatiable, and PSCR + Date as row indices. The ECDC data is already in this form. 

The PSCR key is constructed out of pasting Province.State and Country.Region together. 

Data for the US, China, Australia and Canada is available at the JHHCC github per PS. I summed them up to get country data, then added Continent totals. 


##_Imputations_: A conservative estimate of recovery within 42 days was used, whereever recoveries were missing. 
This includes Canada states data, US States Data, Syria, Mozambique in the JHH dataset, and all of the 
ECDC country data. The severity situation in a country depends on the actual active sick people, as 
only those are contagious, and only a fraction (+/-10%?) of those will need treatment, or die (+/-5%?). 

##Lags
The lag of 42 days was not warranted by this data: most countries report the majority of recoveries within 15-22 days. However, we want a very conservative (upper) estimate of active patients, and want to be sure to only consided those non-contagious people who had the disease as "recovered_imputed"

##Counting
Counting processes in different countries vary wildly: comparing excess 
deaths with Covid deaths over the spring of 2020 yields e.g. that 

Covid deaths are 53% of excess deaths in the Netherlands (undercounting!), 
Covid deaths are 102% of excess deaths in Belgium. (overcounting!)
Sweden has a low difference between Covid and excess deaths. 
Belarus reports no Covid deaths, and there are claims China undercounted deaths. 

cf. the Economist: 
https://www.economist.com/graphic-detail/2020/04/16/tracking-covid-19-excess-deaths-across-countries
https://medium.economist.com/measuring-the-true-toll-of-the-pandemic-fa7e003b3ff4
https://github.com/TheEconomist/covid-19-excess-deaths-tracker
It follows that comparing between countries is not very trustable. 
##definitions
To load the definitions and see what graphs are defined, use 
```{r define}
source('definitions.R')
```

to load the data:
``` {r }
source('loaddata.R') #do this once, before the chunks below!
```
To do one page with (a)ctive, (r)ecovered, (d)eaths, 
(f)acet per PSCR, using (i)mputed recoveries, (a)rea graphs, 
not saving to disk but showing on screen:
``` {r}
graphOnRegion(lpdf=JHH, myRegion = JHHRegios$`JHH World1`, myGraph='graph3Dard_fia', saveit = FALSE) 
``` 
or equivalently 
``` {r}
graph3Dard_fia(JHH, countries = JHHRegios$ `JHH World2`)
```
see other graphs you can make with:
```{r}
myGraphList #will give you all graphs defined
graphCodes() # shows the naming system
```
In order to make and save all current graphs to disk: 
edit the save paths below to what you need, 
then run the lines in output.Rmd or Graphs.R that you need. E.g.: 
```{r message = FALSE, eval = FALSE}
if (!exists('JHH')) source('loadData.R') else 
  if (max(JHH$Date)< Sys.Date()-1) source('loadData.R')

switch(get_os(), 
       windows = {print("I run MS Windows.");myPlotPath <- "G:/My Drive/Covid19_plots"},
       linux   = {print("I'm a penguin."); myPlotPath <- "~/Covid19_plots"},
       osx     = {print("I'm a Mac.");myPlotPath <- "~/Covid19_plots"},
       ...     = {print('not recognized OS');myPlotPath <- "~/Covid19_plots"})

if (!dir.exists(myPlotPath %//% 'data')) dir.create(myPlotPath %//% 'data', recursive = TRUE)

walkThrough(lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'RG') 
walkThrough( lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'RG')
```
Note: a bit of patience: it takes one hour to make all graphs on all territories (both ECDC and JHH) on my HP Elitebook laptop.
