__Covid19 Trendlines__

The presentation of the Covid19 situation in the news often is just three numbers for your own country, 
or a world map with bubbles. This does not tell much about the general trends. 
Here My attempt at making trendlines for all countries in the world, plus some states and territories 
that make their info available and which is collected by the John Hopkins Hospital and shared by their 
Computer Center on GitHub.com. 

My resulting graphs and cleaned and prepared data can be found at 
https://drive.google.com/drive/folders/1Yo0IW4awCIvWMP6jYVKpn3kLgEolse0e?usp=sharing

This is the analysis part of the timeline and area graphs in R. 
An attempt at simulating what whould have happened in Excel is available, and I initially tried to do 
this with Julia, but had trouble getting plotting to work. 

Approach: 
the confirmed panel data is in csv format, row=country, column= date,
and we melt it into a long version, with on column per count vatiable, and PSCR + Date as row indices.
The PSCR key is constructed out of pasting Province.State and Country.Region together. 

Data for the US, China, Australia and Canada is available at the JHHCC github per PS. I summed them 
up to get country data. Then added Continent totals. 
As a check, we also use ECDC data (Country data only).  

Imputations: A conservative estimate of recovery within 42 days was used, whereever recoveries were missing. 
This includes Canada states data, US States Data, Syria, Mozambique in the JHH dataset, and all of the 
ECDC country data. The severity situation in a country depends on the actual active sick people, as 
only those are contagious, and only a fraction (10%?) of those will need treatment, or die (5%?). 

Note that the lag of 42 days was not warranted by my data: most countries report the majority of 
recoveries within 15-22 days. However, we want a very conservative estimate of active patients,
and want to be sure to only call 'recovered' the non-contagious people who had the disease, 

Note also that counting processes in different countries vary wildly: comparing excess 
deaths with Covid deaths over the spring of 2020 yields e.g. that 

Covid deaths are 53% of excess deaths in the Netherlands (undercounting!), 
Covid deaths are 102% of excess deaths in Belgium. (overcounting!)
Sweden has a low difference between Covid and excess deaths. 

cf the Economist: 
https://www.economist.com/graphic-detail/2020/04/16/tracking-covid-19-excess-deaths-across-countries
https://medium.economist.com/measuring-the-true-toll-of-the-pandemic-fa7e003b3ff4
https://github.com/TheEconomist/covid-19-excess-deaths-tracker
It follows that comparing between countries is not very trustable. 

use 
```{r}
source('definitions.R')
graphList() #will give you all graphs defined
```

to load the data:
``` {r }
source('loaddata.R') #do this once, before the chunks below!
```
To do one page with (a)ctive, (r)ecovered, (d)eaths, 
(f)acet per PSCR, using (i)mputed recoveries, (a)rea graphs, 
not saving to disk but showing on screen:
``` {r}
graphOnRegion(lpdf=JHH, myRegion=JHH$`JHH World 1`, myGraph='graph3Dard_fia', saveit = FALSE, ...) 
```
In order to make and save all current graphs to disk: 
edit the save paths at the start of 'definitions.R' to what you need. 

then run
```{r message = FALSE}
#source('Loaddata.R') #if you ran this chunck or the chunck above no need to redo this line
walkThrough(lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'RG') 
walkThrough( lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'RG')
```
It takes one hour to make all graphs on all territories on my HP Elitebook laptop!