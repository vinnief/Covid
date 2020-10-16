# Loads latest data, first JHH then ECDC then testing

source("definitions.R")# make sure we have our definitions. 
if (verbose >= 1) print("loading the data from the web and transforming into useful data format. Please wait a few minutes. ")
dataPath <- './data'
if (!dir.exists(dataPath)) dir.create(dataPath, recursive = TRUE)

ls()

#load ECDC if the data is stale
if (!exists('ECDC') || (max(ECDC$theDate) < Sys.Date() ) ) {
  ECDC <- loadECDC()  
  writeWithCounters(ECDC, name = "Covid19ECDC")
}
if (verbose >= 2) print("ECDC data loaded, latest date:" % % max(ECDC$theDate))

#load JHH if data is stale
if (!exists('JHH') || (max(JHH$theDate) < Sys.Date() - 1)) {
  JHH <- loadJHH()
  writeWithCounters(JHH,name = "Covid19JHH") 
}
if (verbose >= 2) print("JHH data loaded, latest date:" % % max(JHH$theDate))
save.image(".RData") #D:/gits/Covid19/ #save immediately so that another RMD or Jupyter notebook does not need to redownload. 

# end now run Graphs.Rmd or output.Rmd
