# Loads latest data, first JHH then ECDC then testing

(source("definitions.R"))# make sure we have our definitions. 
#theLog = file("loadData.log", open = "wt")
#sink(theLog, type = "message")# , append = T)

if (verbose >= 1) message(Sys.time() % % "loading the data from the web and transforming into useful data format. Please wait a few minutes. ")

#load ECDC if the data is stale
#if (!exists('ECDC') || (max(ECDC$theDate) < Sys.Date() ) ) {
 # ECDC <- loadECDC(7)  
 # writeWithCounters(ECDC, name = "Covid19ECDC")
#}
#if (verbose >= 1) message("ECDC data loaded, latest date:" % % max(ECDC$theDate))

JHH<- reNewJHH()
#vax<- readOwidvax()

#load Belgian data per municipality
#Belgium <- readBelgium() %>% imputeRecovered() %>% extravars()
#writeWithCounters(Belgium, "Belgium", na=)
#
OwidTesting=readTesting()
write_csv(OwidTesting, "Data/Owidtesting.csv",na="")
save.image(".RData") #D:/gits/Covid19/ #save immediately so that another RMD or Jupyter notebook does not need to redownload. 
sink.number( type = c("output","message"))
sink( type = "message"); 
#if (isOpen(theLog)) close("the.log") # not string expected. 
if (isOpen(theLog)) close(theLog)

# end now run Graphs.Rmd or output.Rmd
