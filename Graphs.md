---
title: "Graphs"
author: "VF"
date: "9 July 2020"
output: html_document
---
##Graphs

If needed, run loadData.R first to load the latest Data and the definitions. 
Please make sure myPlotPath is correct!

```R name="init" tags=["remove_cell"]
knitr::opts_chunk$set(echo = TRUE)

if (!exists('JHH')) source('loadData.R') else 
  if (max(JHH$Date) < Sys.Date() - 1) {source('loadData.R')  
   } else source('definitions.R')
save.image(".RData") #D:/gits/Covid19/
tibble( maxECDCdate = max(ECDC$Date), maxJHHdate = max(JHH$Date))
if (!dir.exists(myPlotPath %//% 'data')) dir.create(myPlotPath %//% 'data', recursive = TRUE)
writeWithCounters(ECDC,name = "Covid19ECDC")
writeWithCounters(JHH,name = "Covid19JHH") 
```

Graph and save lots of graphs on all regions present, paginated by size of the Covid impact (total confirmed cases), and on each page, territory graphs are sorted by decreasing value of the first graphed variable on the latest date in that territory.


```R name="graphs walkthrough" tags=["remove_input"]
print("results of" % % Sys.Date())
if (!exists("walk")) source("definitions.R") #should not be necessary after running previous cell?????
verbose = 0
tim = Sys.time()
walkThrough(lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'GR')
reportDiffTime('ECDC graphs',tim, units = 'mins')

tim = Sys.time()
walkThrough( lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'GR')
reportDiffTime('JHH graphs',tim,units = 'mins')

```

Development one month at a time
```R name="one month" tags=["remove_cell"] eval=false
if ( mday(Sys.Date() ) <= 03) {#uses lubridate
  walkThrough(lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, 
              from = floor_date(Sys.Date() %m-% months(1), 'month'), 
              to = ceiling_date(Sys.Date() %m-% months(1), 'month')-1, 
              myFolderDate = year(Sys.Date() %m-% months(1)) %-% sprintf("%02d", month(Sys.Date() %m-% months(1))), 
              ordre = 'RG')
  }
```

Do all months up to today
```R name="all months" tags=["remove_cell"] eval=false
verbose = 1
makeHistoryGraphs(JHH, regions = JHHRegios, fromDates = seq(as.Date('2020-01-01'),Sys.Date(),  by = '1 month'), ordre = 'GR')  
makeHistoryGraphs(ECDC, regions = ECDCRegios, fromDates = seq(as.Date('2020-01-01'),Sys.Date(),  by = '1 month'))  

```
A more convoluted way, just for the record: 
```R name="all months convoluted" tags=["remove_cell"] eval=false
startDate = "2020-01-01"
fromDates <- as.character(seq(as.Date(startDate), length = 12, by = "1 month"))
toDates <- as.character(seq(as.Date(fromDates[2]), length = 12, by = "1 month") - 1)

makeHistoryGraphs(ECDC,ECDCRegios, graphlist = myGraphNrs, 
                  fromDates = fromDates, toDates = toDates)

makeHistoryGraphs(JHH,JHHRegios, graphlist = myGraphNrs, 
                  fromDates = fromDates, toDates = toDates)

```
This do once in a while, to do all the non-numbererd graphs. 

```R name="byDate once a week"
if ( weekdays( Sys.Date() , abbreviate = FALSE) == "Friday")  
  walkThrough(JHH, JHHRegios[1:10], graphlist = myGraphListbyDate,
              myFolderDate = 'weekly')
```

Simulate how the non-social distancing situation would have turned out: deaths, recovered, and confirmed   
```R name="sims" tags=["remove_cell"] eval=false
graphDddp_fyl(JHH,regios$Vincent,savename  = "deaths missed") 
walkThrough(JHH, regions = JHHRegios,graphlist = 'graphDccprr_fyl')

walkThrough(ECDC, ECDCRegios,ext = '_sim', graphlist = c('graphDccprr_fiyl','graphDddp_fyl')) 
walkThrough(ECDC , ECDCRegios,ext = '_endsim',graphlist = c('graphDccprr_fiyl', 'graphDddp_fyl')) 


```
## R Markdown and Jupyter Notebook: sync with Jupytext. 

This is an R Markdown document, linked to the same document as Jupyter notebook, using Jupytext. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

Note that the `{r, echo = FALSE}` parameter can be added to the code chunk to prevent printing of the R code that generated the plot. And Include = false prevents automatic execution of the block.

Second, note that for git, the ipynb files containing output are much heavier than the Rmd files which contain the inputs only, according to Jupytext faq (https://jupytext.readthedocs.io/en/latest/faq.html). I commit both just to be sure at this moment, for Binder. However, i clear output of most graph making cells. Here this file in [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/vinnief/Covid/wip?filepath=graphs.ipynb)
