source("requirements.R")
#Global assumptions
LAGRC <- 42
LAGRD <- 36
LAGDC <- LAGRC - LAGRD
deathRate = .05
stableRate = 1/LAGRC
if (!exists("verbose")) verbose <- 1
myDateFormat <- "%Y-%m-%d"
dataPath = './data'
if (!dir.exists(dataPath)) dir.create(dataPath, recursive = TRUE)

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

switch(get_os(), 
       windows = {message <- "I run MS Windows"; myPlotPath <- "G:/My Drive/Covid19_plots"},
       linux   = {message <- "I run Linux"; myPlotPath <- "~/Covid19_plots"},
       osx     = {message <- "I run OSX" ; myPlotPath <- "~/Covid19_plots"},
       {message <- 'OS not recognized' ; myPlotPath <- "~/Covid19_plots"})
switch(.Platform$GUI, 
       RTerm = {message <- message % % "and Jupyter Notebook/Lab."; myPlotPath <- "G:/My Drive/Covid19_plots"},
       Rgui   = {message <- message % % "and just RGui."; myPlotPath <- "~/Covid19_plots"},
       RStudio     = {message <- message % % "and RStudio." ; myPlotPath <- "~/Covid19_plots"},
       {message <- message % % "and the GUI is" % % .Platform$GUI ; myPlotPath <- "~/Covid19_plots"})
if (verbose >= 2) print(message)
.Platform$OS.type
Sys.info()[['sysname']]
R.version$os # mingw32 for win!

if (!dir.exists(myPlotPath %//% 'data')) dir.create(myPlotPath %//% 'data', recursive = TRUE)

# define data loading
# *Note*: the data of John hopkins, 
# git@github.com:CSSEGISandData/COVID-19.git
# before 20200325:#c <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
# after 20200326:
# c <- read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_global.csv'

#for the USA: 
 #https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_US.csv
#RAW: 
#https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_US.csv
#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
#US headers: UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key, 1/22/20, 
readUSdata <- function(dataversion = "confirmed"){#deaths and recovered are the other options. 
 filename <- paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_', dataversion, "_US.csv", sep = "")
 tryCatch( wpdf <- read.csv(filename) , 
      error = function(e) print( " The data was not found: Are you sure this file exists? " % % filename % % e)
          )
 return(wpdf)
}

readTesting <- function(){
  testing <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv', 
                      col_types = cols(
                        "Entity" = col_character(),
                        "ISO code" = col_character(),
                        "Date" = col_date(format = ""),
                        "Source URL" = col_character(),
                        "Source label" = col_character(),
                        "Notes" = col_character(),
                        "Cumulative total" = col_double(),
                        "Daily change in cumulative total" = col_double(),
                        "Cumulative total per thousand" = col_double(),
                        "Daily change in cumulative total per thousand" = col_double(),
                        "7-day smoothed daily change" = col_double(),
                        "7-day smoothed daily change per thousand" = col_double()
                      ))  %>%  
    select(c('Entity', 'ISO code','Date', 'Cumulative total', 'Daily change in cumulative total'))  %>%  
    mutate(tests  = "Cumulative total", 
           new_tests = "Daily change in cumulative total", 
           ISOcode  = "ISO code")  %>%   
    select(-'Cumulative total', -'Daily change in cumulative total', -'ISO code')
  coco <- as.data.frame(str_split_fixed(testing$Entity, ' - ', n = 2))
  testing$PSCR <- coco[, 1]
  
  testing[testing$PSCR == 'United States','PSCR'] <- 'United States of America'
  testing[testing$PSCR == 'Czech Republic','PSCR'] <- 'Czechia'
  testing$PSCR <- testing$PSCR  %>%  str_replace_all(' ','_') #str_split(' ')  %>%  modify(function(l) paste(l, collapse = '_'))  %>%  unlist() 
  
  testing$comment <- coco[,2]
  testing
}


readTesting_notypes <- function(){
 testing <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv')  %>%  
  select(c('Entity', 'ISO code','Date', 'Cumulative total', 'Daily change in cumulative total'))  %>%  
  mutate(tests  = "Cumulative total", 
      new_tests = "Daily change in cumulative total", 
      ISOcode  = "ISO code")  %>%   
  select(-'Cumulative total', -'Daily change in cumulative total', -'ISO code')
 coco <- as.data.frame(str_split_fixed(testing$Entity, ' - ', n = 2))
 testing$PSCR <- coco[, 1]

 testing[testing$PSCR == 'United States','PSCR'] <- 'United States of America'
 testing[testing$PSCR == 'Czech Republic','PSCR'] <- 'Czechia'
 testing$PSCR <- testing$PSCR  %>%  str_replace_all(' ','_') #str_split(' ')  %>%  modify(function(l) paste(l, collapse = '_'))  %>%  unlist() 

 testing$comment <- coco[, 2]
 testing
}

readdata <- function(dataversion = "confirmed"){#deaths and recovered are the other options. 
 filename <- paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_', 
          dataversion, "_global.csv", sep = "")
 tryCatch( wpdf <- read.csv(filename) , 
      error = function(e) print(paste(e, " The data was not found: Are you sure this file exists? ", filename))
 )
 return(wpdf)
}
makePSCR <- function(PS, CR){PS %,% CR}

convertdata <- function(wpdf, coltype = "date", values.name = "count", US = FALSE){ 
 ID <- c("Country.Region", "PSCR", "Province.State", "Lat", "Long")
 if (US) { ID <- c(ID, "Combined_Key")  }
 wpdf$PSCR <- (ifelse(""  == wpdf$Province.State, 
           as.character(wpdf$Country.Region), 
           makePSCR(wpdf$Province.State , wpdf$Country.Region) ))
 lpdf <- reshape2::melt(wpdf, id = ID, 
            variable.name = coltype, value.name = values.name) 
 #lpdf <- wpdf  %>%  pivot_longer(????)
 lpdf$Date <- as.Date(paste(lpdf[, coltype], "20", sep = ""), format = "X%m.%d.%Y") #add century, get rid of X
 lpdf$date <- NULL
 return(lpdf)
} 

findIDnames <- function(lpdf = JHH, testIDnames = c("Neth", "India"), searchID = "PSCR", 
             fuzzy = TRUE, returnID){
 lpdf <- as.data.frame(lpdf)
 allIDs <- (unique(lpdf[, searchID]))  #error maybe? [ for dataframe 
 if (!fuzzy) {a1 <- intersect(testIDnames, allIDs)
 } else a1 <- allIDs[unlist(llply(testIDnames, function(a) grep(a, allIDs, ignore.case = TRUE)))] # dplyr try: #testIDnames %>% grep( allIDs, ignore.case = TRUE)
 if (missing(returnID)) return ( a1) #returnID = searchID
 else if (searchID  == returnID) {
  if (verbose >= 10) print('no need for returnID if same as searchID')
  return ( a1)} #returnID = searchID
 unique(lpdf[lpdf[, searchID] %in% a1, returnID])
} 

aggreg <- function(avector){
 len <- length(unique(avector))
 if (len  == 1){avector[1]
 }else #if (len  == 2) {paste(avector, collapse = "_") }else
  paste(avector[1], len-2, avector[length(avector)], sep = "_")
}


total_plyr <- function(lpdf = JHH, rows = "", 
         ID = "PSCR" , 
         varnames = c("confirmed", "deaths", "recovered") , 
         newrow = ""
         ) {
 if (rows[1]  == "") rows = unique(lpdf[[ID]])  
 ans <- ddply(lpdf[lpdf[[ID]] %in% rows, ], c("Date"), 
  function(df) {
   Country.Region = ifelse(newrow !=  "", newrow, 
              aggreg(as.character(df$Country.Region)))
   Province.State = ifelse(newrow !=  "", newrow, aggreg(df$Province.State))
   PSCR = ifelse(newrow !=  "", 
         newrow, 
         ifelse(ID  == "PSCR", 
             aggreg(df$PSCR) , 
             ifelse(Province.State  == "", 
                Country.Region, 
                makePSCR(Province.State, Country.Region)
                )
             )
         )
   b1 <- colSums(df[, varnames], na.rm = TRUE)
   nam <- names(b1)
   dim(b1) <- c(1, length(b1))
   b2 <- data.frame(b1)#as_tibble_row()
   colnames(b2) <- nam
   ans <- cbind(Country.Region, 
         PSCR, 
         Province.State, 
         Lat = mean(df$Lat) , Long = mean(df$Long), 
         b2
         )
   if ("population" %in% names(df)) ans$population <- sum(df$population, na.rm = TRUE)
   if ("imputed" %in% names(df)) ans$imputed <- any(df$imputed)
   if ("Region" %in% names(lpdf)) 
    ans$Region <- ifelse(newrow !=  "", newrow, aggreg(df$Region))
   if ("County" %in% names(df)) 
    County = ifelse(newrow !=  "", newrow,  as.character(aggreg(df$County)))
   ans
   }) 
 ans[, setdiff(names(lpdf), names(ans))] <- NA
 ans
}


total <- function(lpdf = JHH, rows = "", ID = "PSCR" , 
                  varnames = c("confirmed", "deaths", "recovered") , 
                  newrow = "") {
  if (rows[1]  == "") rows = unique(lpdf[[ID]])  
  ans <- lpdf[lpdf[[ID]] %in% rows, ] %>% group_by(Date) %>% 
    summarize(
      Country.Region = 
        ifelse(newrow !=  "", newrow, 
               aggreg(as.character(Country.Region))), 
      Province.State = ifelse(newrow !=  "", newrow, aggreg(Province.State)),
      PSCR = ifelse(newrow !=  "", newrow, 
                    ifelse(ID  == "PSCR", aggreg(PSCR) , 
                           ifelse(Province.State  == "", 
                                  Country.Region, 
                                  makePSCR(Province.State, Country.Region)
                           ))),
      confirmed = sum(confirmed, na.rm = TRUE),
      recovered = ifelse(!'recovered' %in% varnames, NA, 
                         sum(recovered, na.rm = TRUE)),
      deaths = sum(deaths, na.rm = TRUE),
      Lat = ifelse(!"Lat" %in% names(lpdf), NA, mean(Lat)) , 
      Long = ifelse(!"Long" %in% names(lpdf), NA, mean(Long)),
      population = ifelse(!"population" %in% names(lpdf), NA,
                         sum(population, na.rm = TRUE)),
      imputed = ifelse(!"imputed" %in% names(lpdf), NA, any(imputed)),
      Region = ifelse(!"Region" %in% names(lpdf) , NA, #try NULL 
                      ifelse(newrow != "", newrow, aggreg(Region))),
      County = ifelse(!"County" %in% names(df) , NA,
                      ifelse(newrow !=  "", newrow, as.character(aggreg(County))))
    )
  ans[, setdiff(names(lpdf), names(ans))] <- NA
  ans[, setdiff(names(ans), names(lpdf))] <- NULL
  ans
}

totals <- function(lpdf = JHH, rows = "", ID = "Country.Region", 
         varnames = c("confirmed", "deaths", "recovered"), needAggreg=TRUE ){
 if (rows[1]  == "") rows = as.list(unique(lpdf[[ID]]))
 if (verbose >= 5) print(paste("Making the total for ", paste(rows, collapse = "/ "), "in", ID))
 ans <- ldply(rows, function(a) lpdf  %>%  total(a, ID, varnames, ifelse(needAggreg,  "", a)))
}
totals_dplyr <- function(lpdf = JHH, rows = "", ID = "Country.Region",  # dplyr try. 
                   varnames = c("confirmed", "deaths", "recovered"), needAggreg=TRUE ){
  if (rows[1]  == "") rows = (unique(lpdf[[ID]]))
  if (verbose >= 5) print(paste("Making the total for ", paste(rows, collapse = "/ "), "in", ID))
  ans <- lpdf %>% filter(!!ID %in% rows) %>% group_by(!!ID) %>% 
    group_modify( .f = (function(a) {total( first(a[[ID]]), ID, varnames, ifelse(needAggreg,  "", first(a[[ID]])))}) )
}

correctnames <- function(df){
 names(df)[match("Long_", names(df))] <- "Long"
 names(df)[match("Province_State", names(df))] <- "Province.State"
 names(df)[match("Country_Region", names(df))] <- "Country.Region"
 df[, !names(df) %in% c("Admin2" , "UID", "iso2", "iso3", "code3", "FIPS")]
}
makelpdfUSStates <- function(){
 wc <- readUSdata('confirmed') 
 #geo.location <- wc[, c("Combined_Key", "Country_Region", "Province_State", "Admin2", "UID", "Lat", "Long_")]
 wc <- correctnames(wc)
 #write.csv( geo.location, file = dataPath %#% "/" %#% "geo.location.US.csv", na = "")
 #rm(geo.location)
 # wc <- wc[, !names(wc) %in% c("Admin2" , "UID", "iso2", "iso3", "code3", "FIPS")]
 confirmed <- convertdata(wc, values.name = "confirmed", US = TRUE)
 wd <- readUSdata("deaths")
 wd <- correctnames(wd)
 #wd <- wd[, !names(wd) %in% c("Admin2" , "UID", "iso2", "iso3", "code3", "FIPS")]
 deaths <- convertdata(wd, values.name = "deaths", US = TRUE)
 lpdf <- merge(confirmed, deaths, all.x = TRUE, 
        by = c("Country.Region", "Province.State", "PSCR", "Combined_Key", "Lat", "Long", "Date"), sort  = FALSE)
 #tryCatch( wr <- readUSdata("recovered"), error = function(e){print(e)})
 #if ("wr" %in% ls()){
 # wr <- correctnames(wr)
 # recovered <- convertdata(wr, values.name = "recovered")
 # lpdf <- merge(lpdf, recovered, all = TRUE, 
 #  by = c("Country.Region", "Province.State", "PSCR", "Combined_Key", "Lat", "Long", "Date"))
 #}else 
 lpdf$recovered <- as.numeric(NA ) #just in case NA totalled into 0. 
 lpdf
}

makelpdfUS <- function() {
 lpdf <- makelpdfUSStates()
 lpdf <- lpdf  %>%  totals(rows = "",  ID = "Province.State", varnames = c('confirmed', 'deaths'), needAggreg = TRUE) 
    # depends on all provinces being chosen to sum! 
 lpdf$Combined_Key <- NULL
 if (!all(is.na(lpdf$recovered)))
  if ( max(lpdf$recovered, na.rm = TRUE) <= 0) lpdf$recovered <- as.numeric(NA ) #just in case NA totalled into 0. 
 lpdf 
}

makelpdf <- function() {
 wc <- readdata('confirmed') #"Confirmed")
 geo.location <- wc[c("Country.Region", "Province.State", "Lat", "Long")]
 #write.csv( geo.location, file = dataPath %#% "/" %#% "geo.location.csv", na = "")
 confirmed <- convertdata(wc, values.name = "confirmed")
 wd <- readdata("deaths")
 deaths <- convertdata(wd, values.name = "deaths")
 wr <- readdata("recovered")
 names(wr)[1] <- names(wc)[1] #"Province.State without strange characters BOM?
 recovered <- convertdata(wr, values.name = "recovered")
 lpdf <- merge(confirmed, recovered, all.x = TRUE, #, sort = FALSE, 
         by = c("Country.Region", "PSCR", "Province.State", "Date", "Lat", "Long"), sort  = FALSE)
 lpdf <- merge(lpdf, deaths, all.x = TRUE, 
         by = c("Country.Region", "PSCR", "Province.State", "Date", "Lat", "Long"), sort  = FALSE)
 lpdf[lpdf$PSCR  == "US", ]$PSCR <- as.character("USA") #to distinguish from the detailed data
 #levels(lpdf$Country.Region) <- c(levels(lpdf$Country.Region), "USA")
 lpdf[lpdf$PSCR  == "USA", "Country.Region"] <- "USA" 
 lpdf
}

updateJHHFromWeb <- function(nameUS = "JHH_US.csv", namenonUS = "JHH_non_US.csv") {
 CUS0 <- makelpdfUS()
 CUS <- as_tibble( CUS0) 
 Cworld0 <- makelpdf() 
 Cworld <-  as_tibble(Cworld0)
 rbind.data.frame(Cworld, CUS)#, StringsAsFactors = FALSE)
 }

readLocalData <- function(nameUS = "JHH_US.csv", namenonUS = "JHH_non_US.csv"){
 #CUS <- read.csv(dataPath %#% "/" %#% nameUS, stringsAsFactors = FALSE)#colClasses = ("Date" = "character"))
 CUS2 <- read_csv(nameUS)
 #Cworld <- read.csv(dataPath %#% "/" %#% namenonUS, stringsAsFactors = FALSE)
 Cworld2 <- read_csv(namenonUS)
 #lpdf <- rbind(Cworld, CUS)
 lpdf2 <- rbind(Cworld2, CUS2)
 #as_tibble(lpdf)
}

# lpdf = JHH

sortIDlevels <- function(lpdf, varname  = "active_imputed", ID  = "PSCR", ondate){
 if (missing(ondate)) { 
  theDateData <- lpdf[, c(varname, ID, 'Date')]  %>%  group_by(PSCR)  %>%  
   filter(Date  ==  max(Date))  %>%  ungroup
 } else {
  theDateData <- lpdf[lpdf$Date  ==  ondate, c(varname, ID, 'Date')]
  if (nrow(theDateData)  ==  0)
   stop("Cannot sort if the date is not present in the Data")
 }
 if (nrow(theDateData)  !=  NROW(unique(lpdf$PSCR)) & (verbose >=  3)) {
  print( ' sorting just lost you these countries'  % %  
       paste(setdiff(unique(lpdf$PSCR), theDateData$PSCR), collapse  = ', ')  % % 
       'as they has no data on '  % %  format(ondate, "%Y-%m-%d"))}
 ordre <- theDateData[, c(varname, ID)]
 levels <- ordre[order(-ordre[[varname]], ordre[[ID]] ), ][[ID]]
 factor(lpdf[[ID]], levels  = levels) #and what if there are too little levels?
}


sortbyvar <- function(lpti, varname = 'active_imputed', ID = 'PSCR', ondate = ""){
lpti[[ID]] <- lpti %>%  sortIDlevels(varname = varname, ID = ID, ondate = ondate) 
lpti <- lpti[order(lpti[[ID]], lpti[[varname]]), ]  
}

makeJHH <- function(name = "JHH", force = FALSE) {
 nameUS <- paste( paste(name, "US", sep = "_"),      "csv", sep = ".")
 namenonUS <- paste( paste(name, "non", "US", sep = "_"),  "csv", sep = ".")
 namedays <- paste(name, "_days.csv", sep = "")
 if (force || (difftime(Sys.time(), file.info(namedays)[, "mtime"], units  = "hours") > 6)) {
  lpdf <- updateJHHFromWeb(nameUS, namenonUS)
  if (verbose >= 1) print("updating JHH from Github")
 } else {
  if (verbose >= 1) print(paste("loading local", namedays))
  lpdf <- read.csv(dataPath %#% "/" %#% namedays, stringsAsFactors  = FALSE)
 }
 if (typeof(lpdf$Date)  == "character") 
  lpdf$Date <- as.Date(lpdf$Date, "%Y-%m-%d") #strptime gives timezones! no need for timezones
 if (verbose > 0) {a = as.numeric(max(lpdf$Date) - min(lpdf$Date) + 1)
  print('JHH' %: % a % % "dates" %, % (nrow(lpdf)/a) % % "regions, last date:" % %  
      max(lpdf$Date) %, % "with"  % % 
      sum(is.na(lpdf[lpdf$Date >= "2020-02-02", ]))  % % 
      "missing values after 2020-02-01")}
 lpdf
}

# ####### make state groups, also useful in tableau

regios <- list(EFTA = c("EFTA", "Iceland", "Liechtenstein", "Switzerland", "Norway"), 
    Benelux = c("Benelux", "Belgium", "Netherlands", "Luxembourg"), 
    US = c("USA" ,"US"), 
    SouthWestAsia = c("South West Asia", "Afganistan", "Iran", "Irak", "Syria", "Lebanon", "Turkey", "Israel", "West Bank and Gaza", "Palestine"), 
    SouthEastAsia = c("South East Asia", "Indonesia", "Thailand", "Vietnam", "Laos", "Malaysia", "Cambodia", "Papua New Guinea", "Myanmar", "Burma", "Brunei", "Philippines", "Timor-Leste"), 
    SAsiaIO = c("South Asia & Indian Ocean", "India", "Pakistan", "Bangladesh", "Sri Lanka", "Comoros", "Maldives", "Madagascar", "Mauritius", "Seychelles", "Bhutan", "Nepal", "Mayotte", "Reunion"), 
     EastAsia = c("East Asia", "Japan", "Korea, South", "Korea, North", "Taiwan*", "Hong Kong", "Singapore", "Mongolia"), 
     CIS = c("CIS", "Russia", "Belarus", "Armenia", "Azerbaijan", "Kazakhstan", "Kyrgyzstan", "Turkmenistan", "Tajikistan", "Uzbekistan", "Moldova"), 
     China = c("China"))
regios <- c(list(EU6 = c("EU6", regios$Benelux[2:4], "Germany", "France", "Italy"), 
     Asia = setdiff(c("Asia", regios$SAsiaIO, regios$SouthEastAsia, 
              regios$SouthWestAsia, regios$EastAsia, regios$China, 
              regios$CIS), 
             c("Madagascar", "East Asia", "South Asia & Indian Ocean", 
              "South East Asia", "South West Asia", "CIS", "Moldova", 'Russia', 
              "Belarus", "Georgia", "Azerbaijan", "Armenia"))), 
     regios)
regios <- c(list(EU = c("EU", regios$EU6[2:7], "Ireland", "Denmark", "Greece", "Spain", "Portugal", "Austria", "Sweden", "Finland", "Poland", "Hungary", "Slovakia", "Slovenia", "Czechia", "Estonia", "Lithuania", "Latvia", "Malta", "Cyprus", "Romania", "Bulgaria", "Croatia"), 
     Caribbean = c("Caribbean", 'Anguilla', "Antigua and Barbuda", "Bahamas" ,  "Barbados", "Bermuda", "Cayman Islands", "Cuba", "Dominica" , "Dominican Republic", "Grenada", "Haiti" , "Jamaica", "Saint Kitts and Nevis" , "Saint Vincent and the Grenadines", "Saint Lucia" , "Trinidad and Tobago", 'Aruba', 'Curacao', "Bonaire, Sint Eustatius and Saba", "British Virgin Islands", 'Guadeloupe', 'Martinique', 'Sint Maarten','St Martin', 'Saint Barthelemy', 'Turks and Caicos Islands', 'Montserrat')), 
     regios)

regios = c(list(
  other = c("Other", 'Diamond Princess', 'MS Zaandam', 'World'), 
  MSM  = c("MSM", "Netherlands", "Belgium", "United Kingdom", "Germany", "Malta", "Egypt", "Suriname", "China", "Vietnam", "Hungary", "Romania", "Kuwait", "Italy", "Ireland", "Iran", "Kazakstan", "Liberia", "Indonesia", "Ethiopia", "Nigeria", "Ghana", "Uganda", "South Africa", "Canada", "Spain", "France"), 
  Vincent = c("Some Selected Regions", "Belgium", "Germany", "Italy", "France", "Kazakhstan", "Indonesia", "Spain", "Netherlands", "Japan", "New York,US"), 
  continents = c("Continents", "Europe", 'North America', "Africa", "South America", "Asia"), #"USA", "US", 
  WestvsEast = c("WestvsEast", "USA", "United Kingdom", "Italy", "Iran", "Korea, South", "Germany", "France", "Spain", "Sweden", "Norway", "Belgium", "Netherlands", "Singapore", "Japan", "Taiwan*", "Denmark", "Hubei, China", "Hongkong, China", "Jiangsu, China", 'Indonesia'), 
  MENA = c("MENA", "Marocco", "Algeria", "Tunesia", "Libia", "Egypt", "West Bank and Gaza", "Palestine", "Lebanon", "Syria", "Turkey", "Iraq", "Iran", "Afghanistan", "Jordan", "Saudi Arabia", "Kuwait", "Oman", "United Arab Emirates", "UAE", "Yemen", "Bahrain", "Qatar"), 
  South_America = c("South America", "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Suriname", "French Guiana", "Venezuela", "Paraguay", "Peru" , "Uruguay", "Falkland Islands (Malvinas)"), 
  Europe = c("Europe", regios$EU[2:28], regios$EFTA[2:5], "United Kingdom", "Russia", "Ukraine", "Belarus", "Moldova", "Georgia", "Armenia", "Azerbaijan", "Andorra", "Monaco", "San Marino", "Vatican", "Holy See", "Albania", "North Macedonia", "Kosovo", "Montenegro", "Bosnia and Herzegovina", "Serbia", "Gibraltar", "Faroe Islands", "Isle of Man", "Channel Islands", "Greenland"), 
  North_America = c("North America", "USA", "Canada", "Mexico", "Saint Pierre and Miquelon", "Antilles", "Belize", "Guatemala", "Nicaragua", "Costa Rica", "Honduras", "El Salvador", "Panama", regios$Caribbean), 
  Africa = c("Africa", "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo (Kinshasa)", "Congo (Brazzaville)", "Cote d'Ivoire", "Djibouti", "Egypt",  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara", "Zambia", "Zimbabwe"), 
   Oceania = c("Oceania", "Australia", "New Zealand", "Vanuatu", "Tuvalu", "Fiji", "Guam", "French Polynesia", "New Caledonia" )

      ), 
   regios)

### data from ECDC - World bank. 
### https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
makeECDC <- function(){
 lpti <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na  = "" ,
                  col_types = cols(
   dateRep = col_character(),
   day = col_double(),
   month = col_double(),
   year = col_double(),
   cases = col_double(),
   deaths = col_double(),
   countriesAndTerritories = col_character(),
   geoId = col_character(),
   countryterritoryCode = col_character(),
   popData2019 = col_double(),
   continentExp = col_character()
 ))  %>%  #fileEncoding  = "UTF-8-BOM" doesn use bom in readr tidyverse. 
  mutate( PSCR = countriesAndTerritories, 
      ISOcode  = countryterritoryCode, 
      confirmed_today = cases, 
      deaths_today = deaths, 
      Date  = as.Date(dateRep, format = "%d/%m/%Y"), 
      population  = popData2019, 
      Region = continentExp)  %>% 
  select(-popData2019, geoId, -day, -month, -year, -cases , -countriesAndTerritories, 
      -dateRep, -continentExp, -countryterritoryCode)  %>%  
  arrange(PSCR, Date)  %>%  group_by(PSCR)  %>%  
  mutate(confirmed  = cumsum(confirmed_today), 
      deaths  = cumsum(deaths_today), 
      recovered = as.numeric(NA))  %>% 
  select(-confirmed_today, -deaths_today)
 if (verbose > 0) {a = as.numeric(max(lpti$Date) - min(lpti$Date) + 1)
  print('ECDC' %: % a % % "dates" %, % length(unique(lpti$PSCR)) % % "regions, last date:" % %  
     max(lpti$Date) %, % "with"  % % 
     sum(is.na(lpti[lpti$Date >= "2020-02-02", ]))  % % 
     "missing values after 2020-02-01")}
 lpti
}

correctMissingLastDay <- function(lpti = ECDC0){
  maxDate <- max(lpti$Date)
  lpti <- lpti %>% group_by(PSCR) 
  missingPSCR <- setdiff( unique(lpti$PSCR) ,
                          lpti %>% filter(Date == maxDate) %>% pull(PSCR) )
  for (myPSCR in missingPSCR) {
    countryData <- filter(lpti, PSCR == myPSCR )
    lastDate <- max(countryData$Date)
    missingRows <- countryData %>% filter( Date == lastDate)
    lastDate <- as.Date(lastDate, format = '%Y-%m-%d')
    missingRows <- missingRows[rep(1, as.Date(maxDate, format = '%Y-%m-%d') - lastDate) ,]
    missingRows <- missingRows %>% mutate(Date = as.Date(lastDate + row_number(),  origin = '1970-01-01'))
      if (verbose >= 2) print(missingRows)
      lpti <- rbind(lpti,missingRows)
    }
  lpti
}

addRegions <- function(lpdf = JHH, varname = "Region", Regiolist = "") { 
 if (!varname %in% names(lpdf)) lpdf[[varname]] <- as.character(NA)
 for (Regio in Regiolist) {
  region = Regio[1]  %>%  strsplit(" countries")  %>%  unlist  %>%  strsplit(" Provinces&States") %>%  unlist
  lpdf[lpdf$Province.State %in% Regio, varname] <- region
  lpdf[lpdf$PSCR %in% Regio, varname] <- region
 }
 lpdf[lpdf$Country.Region == 'US', varname] <- 'North America'
 if (verbose >=  1) {
  print("Regions added:" % %  length(unique(lpdf[[varname]])) )
  if (sum(is.na(lpdf[[varname]])) > 0) {
   print(paste("Not attributed regions:")  % % 
   paste(unique(lpdf[is.na(lpdf[[varname]]), ]$PSCR), collapse = "; "))
  }
 }
 lpdf
}

makeDynRegions <- function(lpti = JHH, byVar = "active_imputed", gridsize = 5*6, piecename = 'World', ratio = 5) {
 lpti <- lpti %>%  group_by(PSCR)  %>%  
  filter( Date  == max(Date)) %>%  ungroup  #%>% 
  #select( PSCR, confirmed, active_imputed) %>%  
  #arrange(desc(active_imputed)) 
  #   arrange(desc(!!enquo(byVar)) ) !! does not work. {{}} neither.
  lpti <- lpti[order(-lpti[[byVar]]), , drop = FALSE]
 nr = 1
 mylist = vector(mode  = "list", length  = 0)
 while (nrow(lpti) > gridsize) {
   if (verbose >= 2) print(piecename %#% nr % % lpti$PSCR[1:3])
  minneeded <- lpti[[byVar]][1]/ratio
  piece <- c(piecename %#% nr, as.character(head(lpti[lpti[[byVar]] >= minneeded, ]$PSCR , gridsize) ))
  mylist[[piece[1]]] <- piece
  nextone = length(piece) - 1
  if (nextone <= 1) nextone = 2
  lpti <- lpti %>%  filter(row_number() >= nextone) 
  nr <- nr + 1
  }
 piece <- c(piecename %#% nr, as.character(lpti[1:nrow(lpti), ]$PSCR))
 mylist[[piece[1]]] = piece
 mylist
}

provincialize <- function(lpdf = JHH, countries){
 cl1 <- unique(lpdf[lpdf$Country.Region %in% countries, ]$PSCR)
 c(countries[1] % % "Provinces&States", cl1)#, setdiff(cl1, countries))
}

provincializeJHH <- function(){
 lpdf = JHH
 ChinaP <- provincialize(lpdf, regios$China)
 list(CanadaP = provincialize(lpdf, "Canada"), 
    USS = provincialize(lpdf, regios$US), 
    NorthAmericaS = setdiff(provincialize( lpdf, c(regios$North_America, "US")), "US"), 
    OceaniaP = (c(provincialize(lpdf, regios$Oceania), regios$Oceania)), #,             "Australia"), 
    AsiaP = c(regios$Asia, (setdiff(ChinaP, "China Provinces&States"))), 
    ChinaP = ChinaP
 )
}

makeRegioList <- function(lpti = JHH, piecename = "JHH"){
 #Oceania = piecename  % %  'Oceania'
 #South_America  = piecename  % %  'South America'
 regios  = c(  
  lpti  %>%  makeDynRegions(piecename = piecename % % 'World'), 
  lpti  %>%  filter(PSCR %in% regios$Europe )  %>%  
   makeDynRegions(gridsize = 20, piecename = piecename % % 'Europe'), 
  lpti  %>%  filter(PSCR %in% c(regios$AsiaP))  %>%  makeDynRegions(piecename = piecename % % 'Asia'), 
  lpti  %>%  filter(PSCR %in% regios$NorthAmericaS)  %>%  makeDynRegions(piecename = piecename  % %  'North America'), 
  lpti  %>%  filter(PSCR %in% regios$Africa )    %>%  makeDynRegions(piecename  = piecename  % %  'Africa') , 
  list(South_America  = regios$South_America, 
    Oceania  = regios$OceaniaP
  ) 
 ) 
}

addPopulation <- function(lpdf) {
 population <- read.csv(dataPath %#% "/" %#% 'population.csv')[c(1, 3)]
 names(population)[2] <- "population"
 rownames(population) <- population$Country.Name
 lpdf[, "population"] <- population[lpdf  %>%  pull(PSCR), "population"]
 popUS <- read.csv(dataPath %#% "/" %#% 'USstatespop2019.csv')[c('State', 'p2019')]
 names(popUS)[2] <- "population"
 rownames(popUS) <- popUS$State
 lpdf[grepl(",US", lpdf$PSCR), "population"] <- 
  popUS[lpdf[grep(",US", lpdf$PSCR), ]$Province.State, "population"]
 popunknown <- unique(lpdf[is.na(lpdf$population), ]$PSCR)

 if (verbose >= 2 || length(popunknown) > 0) print("population unknown:"  % %  

      ifelse(length(popunknown)  ==  0, 0, paste(popunknown, collapse = "; ")))
 lpdf
}


imputeRecovered <- function(lpdf = ECDCdata, lagrc = LAGRC, lagrd = LAGRD, # was 22, 16
              dothese = FALSE, correct = FALSE){
 varname = "recovered"
 if (!('recovered' %in% names(lpdf))) lpdf$recovered <- as.numeric(NA)
 #if (any(dothese)) lpdf[, varname %#% "_old"] <- lpdf[, varname]
 if (!"imputed" %in% names(lpdf))lpdf$imputed <- FALSE
 lpdf <- lpdf %>%  group_by(PSCR)  %>% 
  mutate(recovered_imputed  = 
       pmin(confirmed - deaths, 
          pmax(0, recovered, 
            lag(confirmed, lagrc) - lag(deaths, lagrd), 
            na.rm = TRUE), 
          na.rm = FALSE)
           ) 
 rowstobecorrected = correct & ( lpdf$recovered < lpdf$recovered_imputed )
 rowstodo <- is.na(lpdf$recovered) | dothese | rowstobecorrected
 #  %>%  drop() #no need: is vector already
 if (verbose >= 5)print("imputing recovered for:" % % 
            length(unique(lpdf[rowstodo, ][["PSCR"]]))
             % %  'Regions.')
 if (verbose >= 7) {print('imputing territories:' );print(paste(unique(lpdf[rowstodo, ][["PSCR"]]), collapse = "; "))}
 if (sum(rowstodo)  == 0) return(lpdf)
 lpdf <- lpdf %>%  group_by(PSCR)  %>% 
  mutate_cond(rowstodo, imputed = TRUE )# %>%    mutate_cond(rowstodo, recovered = recovered_imputed)
}

frac <- function(n, d){ifelse(d !=  0, n/d, NA)}

diff.sl <- function(avector, n = 1){c(rep(NA, n), diff(avector, n))}
p_M <- function(a, b) 1e6*a/b

extravars <- function(lpdf, lagrc = 0, lagdc = 0){
 tempwarn <- getOption("warn")
 options(warn = -1)
 on.exit(options(warn = tempwarn))
 lpdf <- lpdf  %>%  ungroup  %>%  
  arrange(PSCR, Date)  %>% 
  group_by(PSCR)  %>%  
  mutate( active           =  confirmed - deaths - recovered, 
      active_imputed       =  confirmed - deaths - recovered_imputed, 
      new_confirmed        =  mac(diff.sl(confirmed)), 
      net_active           =  mac(diff.sl(active)), 
      net_active_imputed   =  mac(diff.sl(active_imputed)), 
      new_recovered        =  mac(diff.sl(recovered)), 
      new_recovered_imputed=  mac(diff.sl(recovered_imputed)), 
      new_deaths           =  mac(diff.sl(deaths)), 
      confirmed_p_M        = p_M(confirmed, population), 
      active_p_M           = p_M(active , population), 
      active_imputed_p_M   = p_M(active_imputed , population), 
      recovered_p_M        = p_M(recovered, population), 
      recovered_imputed_p_M= p_M(recovered_imputed, population), 
      deaths_p_M           = p_M(deaths  , population), 
      new_confirmed_p_M    = p_M(new_confirmed, population), 
      net_active_p_M       = p_M(net_active  , population), 
      new_recovered_p_M    = p_M(new_recovered, population), 
      net_active_imputed_p_M     = p_M(net_active_imputed  , population), 
      new_recovered_imputed_p_M  = p_M(new_recovered_imputed, population), 
      new_deaths_p_M             = p_M(new_deaths , population), 
      recovered_per_confirmed    = frac(recovered, dplyr::lag(confirmed, lagrc)), 
      recovered_imputed_per_confirmed = 
       frac(recovered_imputed, dplyr::lag(confirmed, lagrc)), 
      deaths_per_confirmed = frac(deaths, dplyr::lag(confirmed, lagdc)), 
      recovered_per_deaths = frac(recovered, dplyr::lag(deaths, lagrc - lagdc)), 
      recovered_imputed_per_deaths  
                           = frac(recovered_imputed, dplyr::lag(deaths, lagrc - lagdc)), 
      new_active_rate      =  round(new_confirmed/dplyr::lag(active_imputed, 1), 3) )
}

doublingLine <- function(lpti  = JHH, country, minVal, growthRate, 
            doublingDays  = 5, nrRows  = 100, pop, 
            myDeathRate  = deathRate, lagrc  = LAGRC, lagdc  = LAGDC ){
 if (!missing(country)) {
  if (missing(minVal)) minVal <- lpti$confirmed[1]
  lpti <- lpti  %>%  filter(PSCR %in% country, confirmed >=  minVal)
  minVal <- lpti$confirmed[1]
  if (missing(pop)) pop <- lpti$population[1]
  if (missing(doublingDays)) 
    if (missing(growthRate)) {
      doublingDays  = lpti$confirmed_doublingDays[1]
      if (verbose >=  5) print('doublingline:1 doublindDays = '  % %  doublingDays)
      growthRate  = 2^(1/doublingDays)}
    else doublingDays <- 1/log2(growthRate)
  if (NROW(lpti)  ==  0) stop("No country '" %#% country %#% "'in the data")
 }
 if (!missing(nrRows)) maxDate <- max(lpti$Date) + nrRows
 else {maxDate <- max(lpti$Date)}
 if (verbose >=  5) print('doublingline: doublindDays = '  % %  doublingDays)
 if (missing(growthRate)) {growthRate  = 2^(1/doublingDays)}
 nrRows <- maxDate - min(lpti$Date) + 1
 if (maxDate  ==  -Inf) stop("Max Date equals -inf. Probably we have an empty data set. Did you choose the right country? ")
 if (verbose >=  3) print('doublingLine:'  % %  "R0 = "  % %  round(2^(lagrc/doublingDays) - 1, 2) %, % 'Simulated'  % %  nrRows % % 'days until'  % %  maxDate)
 out = tibble(Date = seq(from = min(lpti$Date), to = maxDate, by = 1))
 doubling <- round(minVal * 2^((0:(nrow(out) - 1))/(doublingDays) ) )
 out <- out  %>%  mutate( growthRate, doublingDays, 
            confirmed  = pmin(doubling, pop), 
            deaths   = round(myDeathRate*lag(confirmed, lagdc, default = 0)), 
            recovered  = lag(confirmed, lagrc, default = 0), 
            active   = confirmed - deaths - recovered, 
            population  = pop - deaths
 ) 
 out
}
growOnce <- function(lpti, rownr, minVal, doublingDays, growthRate, nrRows, myDeathRate = deathRate, pop, lagrc  = LAGRC, lagdc  = LAGDC){#no need for minVal, doublingDays, nrRows but makes passing arguments easier
 #prevrow <- lpti[rownr - 1, ]
 currow <- lpti[rownr - 1, ]  %>%  #we randomize because this overcomes low rates& small minVals
  mutate(Date  = Date + 1, 
      confirmed  = confirmed +
       round(rpois(1, pmax(0.00000001, active*(growthRate - 1) *
                 (population - recovered - active) / population))))
 if (rownr <=  lagdc) currow$deaths <- 0 
  else currow$deaths <- round(myDeathRate*lpti[rownr - lagdc, ]$confirmed) 
 currow$recovered <- 
     ifelse(rownr <= lagrc, 0, round(lpti[rownr - lagrc, ]$confirmed*(1 - myDeathRate)))
 currow <-  currow %>%  mutate(population = pop - deaths, 
               active = confirmed - deaths - recovered
               )
 #currow[setdiff(names(lpti), names(currow))] <- NA #no need slows down.? 
 currow
}

simulGrow <- function(lpti, country, ...){
 out <- doublingLine(lpti = lpti, country = country, ...) 
 if (verbose >=  4) {print('doublingline made' );print(out)} #to compare with growonce results. 
 nrRows <- NROW(out)
 if (nrRows < 2) return(out)
 
 pop  = out$population[1]
 growthRate <- out$growthRate[1] #confirmed_
 for (rownr in 2:nrRows) { # one could try pmap here!
  out[rownr, ] <- growOnce(out, rownr = rownr, growthRate = growthRate, pop = pop, ...) 
 }
 out
}

addSimCountry <- function(lpti, country, ...){ 
 out <- simulGrow(lpti = lpti, country = country, ...)  %>% 
     imputeRecovered  %>%  extravars
 if (missing(country)) {
  out <- out  %>%  mutate(
   Country.Region = doublingDays  %_% 'days', 
   PSCR = doublingDays  % %  'days', 
   Province.State  =  doublingDays  % %  'days', 
   Region  ==  doublingDays  % %  'days', 
   Lat = 90, Long = 90)
 } else {
  doublingDays = out$doublingDays[1] #confirmed_ #bug. what are the names of vars in out? 
  out <- out  %>%  mutate(
   Country.Region = country  % % doublingDays %_% 'days', 
   PSCR = country  % % doublingDays  % %  'days', 
   Province.State  = country  % %  doublingDays  % %  'days', 
   Region  ==  lpti2$Region[1]  % % doublingDays  % %  'days', 
   Lat = lpti2$Lat[1], 
   Long = lpti2$Long[1] )
 }
 missingCols <- setdiff(names(lpti), names(out))
 if (verbose >= 7 & length(missingCols > 0)) 
  print('addsimcountry:'  % % paste(missingCols, collapse = ',') % %  'filled with NAs')
 out[missingCols] <- NA
 lpti <- rbind(lpti, out ) # bug: need to adjust the names of simulated variables in out.
 lpti
}

# from R0 to doubling days: #2^(LAGRC/dd) = (R0+1)   1 should yield LAGRC. 
R02doublingDays <- function(R0 = 1){
 LAGRC/(log2(R0 + 1))
}
doublingDays2R0 <- function(doublingDays = 3) {2^(LAGRC/doublingDays) - 1}


estimateDoublingDaysOneCountry <- function(lpti, variable = 'confirmed', nrDays = 9, minDate = "2019-12-31", maxDate = '2020-12-31'){
 getGR <- function(rowNr){
  lptiSel <- lpti[(rowNr - nrDays + 1):rowNr, ] #potential Bug: assumes data sorted by increasing Date! is it? should be!
  if (sum(!is.na(lptiSel[[variable]])) >= 3 ) {
   #https://win-vector.com/2018/09/01/r-tip-how-to-pass-a-formula-to-lm/
   f <- as.formula('log2(' %#% variable %#% ')~Date')
   growthRate <-  lm( f, data  = lptiSel, na.action  = na.exclude )$coefficients[['Date']] 
          }
  else {growthRate <- NA
  if (verbose >=  3) print('addDoublingDays'  % %  lpti$PSCR[1]  % % 
              'row'  % %  rowNr  % %  
              ' <= 3 days of data for estimating growthrate, filling NA')}
  return(tibble(doublingDays  = 1/growthRate, growthRate))
 }
 rbind(tibble(doublingDays  = rep(NA, nrDays - 1), growthRate  = doublingDays), 
    map_dfr(nrDays:nrow(lpti), getGR))
}


addDoublingDaysPerCountry <- function(lpti, variable = 'confirmed', ...){
 if (!(variable  %_%  'doublingDays' %in% names(lpti))) 
      lpti[[variable  %_%  'doublingDays']] <- as.numeric(NA)
 if (!(variable  %_%  'growthRate' %in% names(lpti))) 
      lpti[[variable  %_%  'growthRate']] <- as.numeric(NA)
 lpti  %>%  group_by(PSCR)  %>%  
   (function(lpti){
     lpti[lpti[[variable]] > 0 , c(variable %_% 'doublingDays', variable  %_%  'growthRate')] <- 
          estimateDoublingDaysOneCountry(lpti[ lpti[[variable]] > 0, ], 
                    variable = variable, ...) 
   lpti
  })
 #lpti
}

addSimVars <- function(lpti, countries, minVal  = 100, ext  = '_sim', minDate  = "2019-12-31", 
            maxDate  = Sys.Date(),  ...){ # doublingDays = -1, pop = 0, ...){
 if (!missing(countries)) countries  = findIDnames(lpti, countries, searchID  = 'PSCR', fuzzy  = FALSE)
 else {
   countries  = unique(lpti$PSCR)
   if (verbose >=  3) {
   print( "AddSimVars: no country given, simulating:"  % %  length(countries)  % %  'countries' )
   if (verbose >=  4) print(paste(countries, collapse  = "/"))}
 }
 if (!('confirmed' %#% ext %in% names(lpti))) {
   lpti[, c('confirmed', 'active', 'recovered', 'deaths') %#% ext ] <- NA}
 lptivalid <- lpti[ lpti$confirmed >=  minVal & lpti$Date >=  minDate & lpti$Date <=  maxDate, ]
 for (country in countries) {
   nrRows <- NROW(lptivalid[lptivalid$PSCR  ==  country, ])
   if ( nrRows  <=  0) { 
     if (verbose >=  4 )
       {print('addsimvarsCountry not simulated'  % %  country  % %  'minVal = '  % %  minVal )
       }
   }
  else {
    details <- lptivalid[lptivalid$PSCR  ==  country, ][1, ]
    temp <- simulGrow(lptivalid[lptivalid$PSCR  ==  country, ], country, ...) 
    names(temp)[names(temp) %in% c('confirmed', 'deaths', 'recovered', 'active')] <- 
      names(temp)[names(temp) %in% c('confirmed', 'deaths', 'recovered', 'active')] %#% ext
    #c('confirmed', 'deaths', 'recovered', 'active') %#% ext
    newnrRows <- nrow(temp)
    if (any(is.na(lpti[lpti$PSCR == country ,c('confirmed','Date')]))) { 
      print(country % % 'nrows' % % nrRows % % newnrRows) 
      print(which((is.na(lpti[lpti$PSCR == country ,c('confirmed','Date')]))))}
    lpti[lpti$PSCR == country & lpti$confirmed >= minVal & lpti$Date >= minDate & 
         lpti$Date <=  maxDate, c('confirmed', 'deaths', 'recovered', 'active') %#% ext] <- 
                 temp[1:nrRows, c('confirmed', 'deaths', 'recovered', 'active') %#% ext]
    if (newnrRows > nrRows) { # we have simulated extra! 
    temp <- temp  %>%  mutate(
     Country.Region = details$Country.Region, 
     PSCR = details$PSCR, 
     Province.State  = details$Province.State, 
     Region  = details$Region, 
     Lat  = details$Lat, 
     Long  = details$Long )
    temp[, setdiff(names(lpti), names(temp))] <- NA
    lpti <- rbind(lpti, 
           temp[nrRows + 1:newnrRows, names(lpti)])
   }
   else if (newnrRows < nrRows) print( country  % %  'has' % %  newnrRows % % 
                                      'instead of' % % nrRows  % %  'rows. this might mean you have missing data? Oi va voi!')
  }
 }
 lpti
}

graph_DemoDoubling <- function(lpti = ECDC, doublingDays = 3, nrRows = -1){
 simulGrow(lpti, "France", minVal = 10, doublingDays)  %>%  
  graphit("France" % % doublingDays  % %  "days", xvar = 'day', yvars = c('active', 'recovered', 'deaths', 'confirmed', "population") , logy = TRUE, to = max(.$Date))  %>% 
  .[c('deaths', 'active', 'confirmed', 'recovered')]  %>%  view  
}


addTotals3 <- function(lpti = ECDC0, totregions , ID = 'Region'){
 if (missing(totregions)) totregions <- unique(lpti[[ID]])
 lpti1 <- lpti  %>% filter(!(!!ID %in% totregions)) 
  #just to be sure, that if i do it twice i dont get double counts. We will get double entries tho! 
 #this might show up in the graphs or not depending on sorting. 
 varnames  = c("confirmed",  "deaths", "population") 
 #Africa totals recovered and active seem to mirror imputed values. 
 if (!('Lat' %in% names(lpti))) lpti1$Lat <- NA
 if (!('Long' %in% names(lpti))) lpti1$Long <- NA
 lpti <- rbind(lpti, lpti  %>%  total("", varnames = varnames, newrow = "World")) 
 for (regio in totregions[!is.na(totregions)])
  lpti <- rbind(lpti, 
        lpti1  %>%  total(regio , ID = ID, varnames = varnames, newrow = regio))
  
 lpti
}
checkTotals3 <- function(lpti = ECDC0, totregions , ID = 'Region'){
  if (missing(totregions)) totregions <- unique(lpti[[ID]])
  lpti1 <- lpti  %>% filter(!(!!ID %in% totregions)) 
  #just to be sure, that if i do it twice i dont get double counts. We will get double entries tho! 
  #this might show up in the graphs or not depending on sorting. 
  varnames  = c("confirmed",  "deaths", "population") 
  #Africa totals recovered and active seem to mirror imputed values. 
  if (!('Lat' %in% names(lpti))) lpti1$Lat <- NA
  if (!('Long' %in% names(lpti))) lpti1$Long <- NA
  if (!all(sort(names(lpti)) == sort(names( lpti  %>%  total("", varnames = varnames, newrow = "World")) )))
    {print(sort(names( lpti  %>%  total("", varnames = varnames, newrow = "World"))))
    print("while " )
    print(sort(names(lpti)))
  }
  else TRUE  
}
  names(total(ECDC0, 'Europe' , ID = 'Region', varnames = c('confirmed','deaths'), newrow = 'Europe'))
addCountryTotals <- function(lpdf = JHH, varnames = c("confirmed","recovered", "deaths","population")){
  existingTotals <- c("China","Australia","Canada",'US')
  #just to be sure, that if i do it twice i dont get double counts. 
  #And omit US as country and US states. 
  lpti <- lpdf  %>% 
    filter(!(PSCR %in% existingTotals )) #
  rbind(lpti, 
        lpti  %>%  totals(c("China","Australia", "Canada",'US'),
                        ID = "Country.Region", varnames = varnames, needAggreg = FALSE))
}

addRegionTotals <- function(lpdf = JHH, totRegions, 
                            varnames = c("confirmed","recovered", "deaths","population") ){
  existingTotals <- c("China","Australia","Canada",'US')
  newTotals <- c("South_America", "Asia", "Africa", "Europe",'North_America',"World")
  lpdf <- lpdf  %>%  filter(!(PSCR %in% newTotals))
  #just to be sure, that if i do it twice i dont get double counts. 
  #And omit US as country and US states. 
  if ( 'Country.Region' %in% names(lpdf))   
    lpti <- lpdf  %>%  filter(!(Country.Region == "US")) #we have US and USA in JHH
  else lpti <- lpdf
  lpti <- lpti  %>%  filter( ! PSCR %in% existingTotals ) #avoid double counting
  World <- unique(lpti[['PSCR']])
  if (missing(totRegions)) totRegions <- newTotals
    #c(regios,list('World' = c('World',World))) ## this is buggy if we do it again we will have twice benelux etc. 
  if (verbose >= 5) {
    print('world totals include the following countries: ')
    print(paste(World,collapse = ","))}
  
  for (myRegion in regios[totRegions]) {
    lpdf <- rbind(lpdf, 
                  total(lpti, myRegion, ID = 'PSCR', newrow = myRegion[1], varnames = varnames))
  }
  lpdf
}

loadJHH <- function() {
  ti <-  Sys.time()
  JHH <- makeJHH(force = TRUE) 
  reportDiffTime('loading and melting JHH:',ti,'secs')
  JHH0 <- JHH
  regios <<- c(list(World = c('World', unique(JHH[['PSCR']]))), provincializeJHH(), regios) 
  ti <-  Sys.time()
  JHH <- JHH0 %>%
    addPopulation() %>% addCountryTotals() %>% addRegionTotals() %>%
    addRegions( Regiolist = regios) %>% arrange(PSCR,Date) %>%
    imputeRecovered() %>% extravars()#
  if (verbose >= 1) reportDiffTime('adding population, totals, imputations, and daily vars in JHH:',ti,'secs')
  ti = Sys.time()
  JHH <- JHH %>%  addDoublingDaysPerCountry(variable = 'confirmed') %>% 
    addDoublingDaysPerCountry(variable = 'active_imputed') 
  if (verbose >= 1) reportDiffTime('adding the doubling days (twice) in JHH:',ti,'mins')
  
  #ti = Sys.time()
  #JHH <- addSimVars(JHH, minVal = 100) %>% 
  #  addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")
  #reportDiffTime('adding the simulated values in JHH:',ti,'mins')
  
  JHHRegios <<- makeRegioList(JHH)
  #writeWithCounters(JHH,name = "Covid19JHH")
  JHH
} 

#same with ECDC
loadECDC <- function() {
  tim = Sys.time()
  ECDC0 <- makeECDC()
  reportDiffTime('load ECDC:',tim,'mins')
  tim = Sys.time()
  ECDC  <- ECDC0 %>% correctMissingLastDay() %>% 
    addTotals3 %>% imputeRecovered %>%  extravars %>%
    mutate(Country.Region = PSCR) %>%
    addDoublingDaysPerCountry(variable = 'active_imputed') %>%
    addDoublingDaysPerCountry(variable = 'confirmed') 
  if (verbose >= 1) reportDiffTime('correct, add totals, imputations, vars, doubling days in ECDC:',tim,'mins')
  
  #tim = Sys.time()
  #ECDC <- ECDC %>%  
  #addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") #%>% #, maxDate = Sys.Date() - 1
  # Because of missing Spain data, growth in Europe on last day is negative. hence sim does not work
  #ECDC <- ECDC %>% addSimVars(minVal = 100)  #gives errors. cayman islands follows conveyance_Japan
  #if (verbose >= 1) reportDiffTime('adding the simulated values in ECDC:',tim,'secs')
  ECDCRegios <<- makeDynRegions( ECDC, piecename = 'ECDC World')
  ECDC
}
loadTesting <- function() {
  testing <- readTesting()
  write.csv(testing,myPlotPath %//% "data" %//% 'testing.csv' )
  testing
}
# end of data input cleaning and preparation. 
# from here, prepare data for the csv-writing or graphing 
addcounterfrommin <- function(lpdf = JHH, minv = 0, varname = "confirmed", ID = "PSCR", counter = "day"){
 lpdf[, counter] <- as.numeric(NA)
 lpdf <- lpdf %>%  filter(!is.na(!!varname))  #[!is.na(lpdf[, varname]), ] #should not have any! effect for "confirmed" 
 if (sum(lpdf[, varname] >= minv) > 0) 
  lpdf[lpdf[[varname]] >= minv, ] <- 
     ddply(lpdf[lpdf[[varname]] >= minv, ], ID, 
      function(lpdf){
       lpdf[[counter]] <- seq_along(lpdf$Date)
       lpdf}
      )
  lpdf
}

addcounterfrommin_New <- function(lpdf = JHH, minv = 0, varname = "confirmed", ID = "PSCR", counter = "day"){
  lpdf[, counter] <- as.numeric(NA)
  lpdf <- lpdf %>%  filter(!is.na(!!varname))  #[!is.na(lpdf[, varname]), ] #should not have any! effect for "confirmed" 
  if (sum(lpdf[, varname] >= minv) > 0) 
    lpdf[lpdf[[varname]] >= minv, ] <- lpdf[lpdf[[varname]] >= minv, ] %>% group_by(!!ID) %>%
        mutate(!!counter := seq_along(Date)
    )
  lpdf
}


### make day vars for tableau & Excel
makecountname <- function(countname, minv){paste(countname, minv, sep = "_")}

writeWithCounters <- function(lpdf = JHH, varname = "confirmed", ID = "PSCR", name = "JHH") {
  lpdf <- as.data.frame(lpdf)
   lpdf <- lpdf[!is.na(lpdf[c(varname)]), ]
  for (minv in c(1, 20, 100, 400, 1000, 2000, 5000, 1e4, 5e4, 1e5, 5e5, 1e6)) {
   lpdf <- addcounterfrommin(lpdf = lpdf, minv = minv, 
                varname = varname, ID = ID, 
                counter = makecountname("day", minv))
  }
  filename = paste(name, "days.csv", sep = "_")
  write_csv(lpdf, path = dataPath %#% "/" %#% filename, na = "")
  write_csv(lpdf, path = myPlotPath %//% "data" %//% filename, na = "")
  if (verbose >= 1) print(paste("Written the current data with counters to disk as", filename, "for use in Tableau or Excel"))
}

#some output tables. 
days2overtake <- function(lpti = JHH ,
                          #countries = c('Belgium', 'China', 'Russia', 'Netherlands', 'Colombia', 'Iran'), 
                          varname = 'confirmed', newvarname = 'new_confirmed'){
  #'listed by decreasing nr of confirmed, and smallest is equals to largest in so many days / so many days ago')
  
  lastdata <- lpti#[lpti$PSCR %in% countries,]  %>%     filter(Date  == max(Date))
  
  lastdata <- (lastdata[order( -lastdata[[varname]]), ])
  variable <- lastdata[[varname]] 
  names(variable) <- lastdata$PSCR
  newvar <-  lastdata[[newvarname]]
  names(newvar) <- lastdata$PSCR
  mconf <- outer(variable, variable, `-`)
  mnew <- -(outer(newvar, newvar, `-`))
  round(mconf/mnew, 1)
}

overtakeDays_v <- function(lpti, country, who = 'theyme', varname = 'confirmed', nr = 10, lastDays=3){
  if (varname %in% c('active', 'active_imputed', 'active_p_m', 
                     'active_imputed_p_M')) 
    prefix <- 'net_'
  else if (varname %in% 
           c('confirmed', 'confirmed_p_M', 
             'recovered', 'recovered_p_M', 'deaths', 'deaths_p_M', 
             'recovered_imputed', 'recovered_imputed_p_M')) 
    prefix <- 'new_'
  newvarname <- prefix %#% varname
  lastdata <- lpti[, c('PSCR','Date', varname, newvarname)]  %>%  group_by(PSCR)  %>%  
    filter(Date >= max(Date) - lastDays + 1)  %>% 
    mutate(!!newvarname := ma( .data[[newvarname]],lastDays,sides = 1))  %>%   
    #mutate_at( .vars = 4, .funs = ma)  %>% 
    filter(Date  == max(Date))
  mydata <- subset(lastdata, PSCR  ==  country)
  if (who  ==  'theyme') {
    countries <- lastdata[lastdata[[varname]] <=  mydata[[varname]] &
                            lastdata[[newvarname]] >=  mydata[[newvarname]], 'PSCR']
    colName <- 'overtakes'  % %  country  } 
  else if (who  ==  'Ithem') {
    countries <- lastdata[lastdata[[varname]] >=  mydata[[varname]] &
                            lastdata[[newvarname]] <=  mydata[[newvarname]], 'PSCR']
    colName <- country  % %  'overtakes'  }
  else stop('who can only be theyme or Ithem')
  oc <- days2overtake(lastdata[lastdata$PSCR %in% countries$PSCR,], varname, newvarname)[, country]
  if (is.null(names(oc))) names(oc)  = c(colName, rep('?', length(oc) - 1)) 
  #else {} #names(oc)[1] <- colName
  oc <-   oc[order(oc, na.last  = FALSE)][1:nr]
  names(oc)[1] <- colName
  oc
}
overtakeDays_df <- function(...){
  vec <- overtakeDays_v(...)
  if (is.null(names(vec))) names(vec)  = rep('?', length(vec)) #return (vec)
  #else 
  tib <- tibble(country = names(vec), Days = vec)
  names(tib)[1] <- names(vec)[1]
  names(tib)[2] <- 'days'
  tib[2:nrow(tib), ]
}


dataprep <- function(lpdf = JHH, minVal = 1, ID = "PSCR", 
           xvar = "day", yvars = c("confirmed", "recovered"), 
           logx = FALSE, logy = TRUE, sorted = TRUE){
 if (!(xvar %in% names(lpdf))) 
    lpdf <- lpdf  %>%  addcounterfrommin(minVal, varname = "confirmed", ID = ID, counter = xvar)
 if (logy){ #get rid of negative and zeros for the log
    eps <-  1e-5
    for (varname in yvars) {
      if (sum((!is.na(lpdf[, varname])) & lpdf[, varname] <=  eps) > 0) 
      #if all NAs, one row replaces zero rows -> error!
        lpdf[(!is.na(lpdf[, varname])) & lpdf[, varname] <=  eps, varname] <- NA 
    } }
 if (logx) lpdf[lpdf[[xvar]] <= 0, xvar] <- NA #was 1
 if (sorted) {
  lpdf[[ID]] <- sortIDlevels(lpdf = lpdf, varname = yvars[1]) 
  if (verbose >=  7) {print('dataprep ID before sort:');print(unique(lpdf[[ID]]))}
  lpdf <- lpdf[order(lpdf[[ID]], lpdf[[xvar]]), ] 
  if (verbose >= 7) print('Dataprep IDs after sort:'  % %  
                            paste(levels(lpdf[[ID]]), collapse = '-'))
 } 
 lpdf <- lpdf[, c(xvar, ID, yvars)]# , 'Date' ungroup  %>%  arrange(PSCR)
}
#Bug potential: after the sort, PSCR is a factor. before, it was character!



# +
#geom_point(shape = "\u2620", size  = 4) #skulls
# + scale_color_manual(values = c('#999999', '#E69F00', '#56B4E9'))
graphit <- function(lpti, countries, minVal  = 1, ID  = "PSCR", xvar  = "Date", 
                    yvars  = c("active", "recovered", "deaths", "confirmed"), 
                    fuzzy  = FALSE, logx  = FALSE, logy  = FALSE, intercept  = FALSE, slope = FALSE,
                    myFolderDate  = 'random', myFolderType  = "", savename  = "", putlegend = TRUE, size = 2, 
                    returnID  = "PSCR", area  = FALSE, position  = 'stack', facet  = FALSE, 
                    sorted  = TRUE, from = '2019-12-01', to  = Sys.Date()){
  
  lpdf <- as.data.frame(lpti[lpti$Date >=  from & lpti$Date <=  to & lpti$confirmed >=  minVal, ])
  lastdate <- max(lpdf$Date)
  if (typeof(to)  == "character") to = as.Date(to, format = "%Y-%m-%d")
  if (missing(countries)) {
    countries <- unique(lpdf[[returnID]])
    if (length(countries > 40)) return(print('too many countries, you wont see anything. Please select less countries'))
  } else countries <- findIDnames(lpdf, testIDnames = countries, searchID = ID, 
                                  fuzzy = fuzzy, returnID = returnID) #}
  ID <- returnID
  if (verbose >= 8) {print(countries)}
  lpdf <- lpdf[lpdf[[ID]] %in% countries,] #PSCR
  if (verbose >= 9) {print('graphi countrydata' );print(head(lpdf))}
  y_lab <- paste(sort(yvars), collapse = " & ") % % ifelse(logy, "(log)", "")
  if (str_length(y_lab) > 80) 
    y_lab <- paste(initials(sort(yvars)), collapse = "&")  % %  
    ifelse(logy, "(log)", "")
  mytitle <- savename % % y_lab % % "by" % % xvar % %  "for" % % minVal %#% "+"  % %  "confirmed"
  myFilename <- "C19" % % mytitle
  
  if (nrow(lpdf)  ==  0 ) {return( if (verbose >=  4) {print('graphi'  % %  mytitle  % % " No data")} ) }
  mytitle <- "C19" % % format(min(lpdf$Date), format  = "%Y-%m-%d")  % % '-' % %  
    format(lastdate, format  = "%Y-%m-%d")  % %  mytitle
  
  lpdf <- dataprep(lpdf, ID  = ID, minVal  = minVal, xvar  = xvar, yvars  = yvars,
                   logx  = logx, logy  = logy, sorted  = sorted)
  if (verbose >= 7) {print('graphi columns left');print( names(lpdf))}
  if (nrow(lpdf)  == 0 || all(is.na(lpdf[, xvar])) || all(is.na(lpdf[, yvars])))
    return(if (verbose >= 6) print('graphi'  % %  paste(mytitle, "Too little data to graph. Maybe lower the mininum value, take more territories?")))
  
  lpdf <- lpdf  %>%  
    melt(lpdf , id = c(ID, xvar), measure.vars = yvars, 
         variable.name = "variable", value.name = "count") %>% 
    mutate( mygroup = PSCR %, % variable, #!!ID? you would get zigzags if ID <> PSCR and you havent totalled. 
            variable = factor(variable, levels  = yvars)) %>% drop_na()
  if (verbose >= 8) {print('graphi summary pdf:');print(summary(lpdf))}
  
  if (facet  == 'variable') lpdf$mygroup <- lpdf[[ID]] else 
    if (facet  == ID) lpdf$mygroup <- lpdf$variable
  lines_only <- lpdf %>% select(!!ID,mygroup) %>% group_by_at(c(1,2)) %>% 
    filter(n() > 1) 
  if (verbose >= 7) {view(lines_only)}
  lines_only <- lines_only %>% unique()
  if (verbose >= 7) {view('country-lines with 2+ datapoints:') ; print(lines_only)}
  
  lpdf_lines_only <- lpdf[lpdf[[ID]] %in% lines_only[[ID]] & 
                            lpdf$mygroup %in% lines_only$mygroup, ] 
  if (verbose >= 7) {view(lpdf_lines_only)}
  nrgroups <- length(unique(lpdf$mygroup))
  if (verbose >= 7) print( 'graphi'  % %  xvar % % "from" % %  
                             min(lpdf[, xvar]) % %  "to" % % max(lpdf[, xvar]) %, % 
                             "group by " % %  lpdf$mygroup[1] %, % "facets"  % %  facet)
  nrIDs <- length(unique(lpdf[, ID]))
  myplot <- ggplot(lpdf, 
                   aes_string(y = "count", x = xvar, group = 'mygroup', 
                              color = ifelse(nrIDs  == 1,  'variable' , 
                                             ifelse(facet  == ID, 'variable', ID))
                   ), na.action = na.omit)
  
  if (area) {
    posalpha <- ifelse(position  == 'identity', 0.4, 1)
    myplot <- myplot + geom_area(aes_string(
      color = ifelse(nrIDs  == 1 || facet  == ID, 'variable' , 'mygroup'), 
      fill = ifelse(nrIDs  == 1 || facet  == ID,  'variable' , 'mygroup')), 
      position  = position, alpha = posalpha)
    myscale_fill <- scale_fill_manual(values  = c("red", "green", "black", "darkorange", "lawngreen"))
    if (nrgroups <= 2) myscale_fill <- scale_fill_manual(values  = c("lawngreen", "cyan"))
    myplot <- myplot + myscale_fill + 
      scale_color_manual(values  = c("red", "green", "black", "darkorange", "lawngreen"))
  } else {
    myplot <- myplot + #line plot
      geom_line(data = lpdf_lines_only, alpha = 0.3, size = size*0.7) +
      geom_point(size = size, aes_string(  shape = 'variable')) +
      if (!putlegend || facet  == FALSE) 
        geom_dl(aes_string(x = xvar, y = "count",  label = 'mygroup'),    
                method  = list(dl.trans(x  = x + 0.1 , y = y + 0.1), "last.points", 
                               cex  = 1.2)) 
    if ( intercept || slope ) myplot <- myplot + geom_abline( intercept  = 1*intercept, slope = 1*slope, na.rm  = TRUE) #bug here or somewhere: the line is at 1.? instead of at 0.24
    if (length(unique(lpdf$variable)) <= 6 ) 
      myplot <- myplot + scale_shape_manual(values  = c(0, 1, 3, 2, 1, 0, 10, 5, 6)) #shape = "\u2620" #bug? 
    if (nrgroups <= 6) {
      myscale_color <- scale_color_manual(values = c("red", "darkgreen", "black", "orange", 
                                                     "lawngreen", "tomato"), #darkorange
                                          guide = ifelse(putlegend, "legend", FALSE))
    }else if (nrgroups < 13) {
      palette = ifelse(nrgroups < 8, "Dark2", "Paired") #Spectral Set2 
      myscale_color <- scale_color_brewer(palette = palette)
    } else myscale_color <- scale_color_discrete(guide = ifelse(putlegend, "legend", FALSE))
    myplot <- myplot + myscale_color 
  } 
  
  if (!isFALSE(facet)) {
    myplot <- myplot + facet_wrap(as.formula(paste("~", facet)), strip.position = "bottom")}
  myplot <- myplot + ylab(y_lab) +
    xlab(paste(xvar, ifelse(logx, "(log scale)", ""))) + 
    ggtitle(mytitle) + theme_light() +   
    guides(col  = guide_legend(nrow = 30, ncol  = min(2, (nrgroups - 1) %/% 30 + 1))) 
  
  breaks <- breaks_log(n = 5, base = 10) #rep(c( 1, 5), 21)*10^rep((-10:10), each = 2)
  minor_breaks <- rep( 1:5, 21)*(10^rep(-10:10, each = 5))
  if ( logy  !=  FALSE) myplot <- myplot + scale_y_continuous(trans = 'log10', breaks  = breaks, minor_breaks  = minor_breaks, labels = label_number_si()) + annotation_logticks() 
  if (xvar  == "Date") myplot <- myplot + scale_x_date(labels  = date_format("%d-%m")) else 
    if (logx) myplot <- myplot + scale_x_continuous(trans = 'log10', breaks  = breaks, 
                                                    minor_breaks  = minor_breaks)
  myplot <- myplot + theme(
    axis.text  = element_text(color  = "blue", angle  = 45, 
                              hjust  = 1, vjust  = 0.5, size  = rel(.8)),   
    strip.background  = element_rect(fill = "white", color = 'black'),   
    strip.text  = element_text(color  = 'black'))
  
  if (savename !=  "") {
    if (facet  == FALSE) savename <-  paste(savename, "all-in-one")
    if (area) savename <- paste(savename, "area plot")
    if (myFolderType  == "") { myFolderType <- myFolderDate %//% sort(initials(yvars)) % % 'by' % % xvar}
    else myFolderType <- myFolderDate %//% myFolderType
    if (area) myFolderType <- myFolderType  % %  "area plot"
    if (logy) myFolderType <- myFolderType  % % 'log scale'
    if (facet  == FALSE) myFolderType <-  paste(myFolderType, "all-in-one")
    if (verbose >=  4) print("graphi making plot"  % %  myFolderType %#% "/" %#% mytitle)
    myplot <- myplot + theme(text = element_text(size  = 20), 
                             axis.text  = element_text(color  = "blue", size  = rel(.8)) )
    if (myFolderType  !=  "") myPath <- myPlotPath %//% myFolderType else myPath <- myPlotPath
    if (!dir.exists(myPath)) dir.create(myPath, recursive  = TRUE)
    on.exit(while (!is.null(dev.list())) dev.off() )
    #suppressWarnings(#options(warn = -2)
    png(filename  = myPath %//% myFilename %#% ".png", width  = 1600, height  = 900)
    print(myplot)
    #)
    dev.off()
  }else {
    print(myplot + theme(title  = element_text(size  = 11)))
  }
  invisible(lpdf)
} 

graphCodes <- function(){ 
 print('naming system:')
 print(list(nrvars = 1:6, 
     letters = tibble(codes = c('d/D/c/d', 'acrd_', 'f', 'i', 'M', 'n', 'y', 'a/l'), 
     meaning = c('day, Date, confirmed, death xvar', 'initials/abbrev of yvars', 'facet per ID', 
          'imputed', 'per million', 'new', 'logy', 'area or line')
     )  ))
 writeLines('example graph1Dc_fiMnyl: confirmed by Date,  facet (by ID), use imputed, per Million, new (not cumulative), logy, line plot). 

The variable "myGraphNrs" contains all daily interesting graph functionnames one can pass as a parameter to walkThrough or makeHistoryGraphsRG.')
}

initials <- function(text = c('test_1', 'of_imputation', 'new_recovered_imputed_per_Million')){
 paste(unlist(lapply(
  lapply ( strsplit(text, '_'), function(st) substr(st, 1, 1)), 
  function(vec) paste(vec, collapse = "_")
 )), collapse = '+')
}
rm(list = ls(pattern = "graph[[:digit:]]"))
#in alfphabetical order of outputs

graph6Dardcra_fiMyl <- function(lpdf  = JHH, countries, logy  = TRUE, ...){
 graphit(lpdf, countries, xvar = "Date", logy  = logy, facet  = "PSCR", 
     yvars = c('active_imputed_p_M', 'recovered_imputed_p_M', 'deaths_p_M', 
         'confirmed_p_M', 'recovered_p_M', 'active_p_M'), ...)
}

graph6Dardcra_fiyl <- function(lpdf = JHH, countries, logy  = TRUE, ...){
 graphit(lpdf, countries, xvar = 'Date', logy = logy, facet  = 'PSCR', 
        yvars = c('active_imputed', 'recovered_imputed', 'deaths', 
            "confirmed", 'recovered', 'active'), ...)
}
#same as areas
#
graph3Dard_fia <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = "Date", area  = TRUE, facet  = 'PSCR', 
     yvars = c('active_imputed', 'recovered_imputed', 'deaths'),  ...) 
}


#new --
graph3Dard_fina <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = "Date",  facet = 'PSCR', area  = TRUE, 
     yvars = c('net_active_imputed', 'new_recovered_imputed', 'new_deaths'), ...) 
}

graph6Dardcra_finyl <- function(lpdf = JHH, countries, logy  = TRUE, ...){
 graphit(lpdf, countries, xvar = "Date", logy = logy, facet  = 'PSCR', 
     yvars = c('net_active_imputed', 'new_recovered_imputed', 'new_deaths', 
         'new_confirmed', 'new_recovered', 'net_active'),  ...) 
}

graph6Dardcra_fiMnyl <- function(lpdf = JHH, countries, 
               logy = TRUE, ...){
 graphit(lpdf, countries, xvar = "Date", logy = logy, facet = "PSCR", 
     yvars = c( 'net_active_imputed_p_M', 'new_recovered_imputed_p_M', 
          'new_deaths_p_M', 'new_confirmed_p_M', 'new_recovered_p_M', 
             'net_active_p_M'), ...)
}

graph1Da_finl <- function(lpdf = JHH, countries, ...){
  graphit(lpdf, countries, xvar = 'Date', 
          yvars = c("net_active_imputed"), facet = 'PSCR', putlegend = TRUE, ...)
}
graph1Da_fil <- function(lpdf = JHH, countries, logy = FALSE, ...){
  graphit(lpdf, countries, xvar = 'Date', yvars = c("active_imputed"),  facet = 'PSCR', ...)
}

graphDc_fnl <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = 'Date', 
         yvars = c("new_confirmed"), facet = 'PSCR', putlegend = TRUE, ...)
}
graphDc_fl <- function(lpdf = JHH, countries, logy = FALSE, ...){
 graphit(lpdf, countries, xvar = 'Date', yvars = c("confirmed"),  facet = 'PSCR', ...)
}

#other graphs

graphDg_fyl <- function(lpdf = JHH, countries, logy  = TRUE, ...){
 graphit(lpdf, countries, xvar = 'Date', 
         yvars  = c('confirmed_growthRate'), 
         logy  = logy,  ...)
}
graphDggnar_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
 graphit(lpdf, countries, xvar = 'Date', 
         yvars = c('active_imputed_growthRate', 'confirmed_growthRate', "new_active_rate"), 
         logy = logy, intercept  = stableRate, facet = 'PSCR', ...)
}

graph1dnar_iyl <- function(lpdf  = JHH, countries, minVal = 10, logy = TRUE, ...){
  graphit(lpdf, c(countries), minVal, xvar = 'day', 
          yvars = c('new_active_rate'), logy = logy, intercept = stableRate, ...) 
}

graphDgnar_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
 graphit(lpdf, countries, xvar = 'Date', 
         yvars = c("new_active_rate", 'active_imputed_growthRate'), 
         logy = logy,  intercept = stableRate, facet = 'PSCR', ...)
}

graph2crd_il <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = 'confirmed', 
         yvars = c('recovered_imputed', 'deaths'), slope = 1, ...)
}

graph1dr_iyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
 graphit(lpdf, countries, xvar  = 'deaths', yvars  = c('recovered_imputed'), 
         logy  = logy, logx  = TRUE, ...)
}
graph1Drr_il <- function(lpdf = JHH, countries, myFolderType  = '', ...){
 graphit(lpdf, countries, xvar = "Date", 
         yvars = c('recovered_imputed_per_confirmed'), 
         myFolderType = myFolderType %#%"recovery rate", 
         putlegend = FALSE , ...   )
}
#Simulation included
graphDccp_fyl <- function(lpdf = JHH, countries, logy = TRUE , ext = "_sim", ...){
 graphit(lpdf, countries, myFolderType = "Confirmed infections simulated", 
     yvars = c('confirmed' %#% ext, 'confirmed', "population"), 
     logy = logy, facet = 'PSCR')
}

graphDccprr_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ext = '_sim', ...){
 graphit(lpdf, countries, myFolderType = "Confirmed recovered simulated", 
     yvars = c('confirmed' %#% ext, 'confirmed', "population", 'recovered' %#% ext, 
         'recovered_imputed'), 
     logy = logy, facet  = 'PSCR', ...)
}

graphDddp_fyl <- function(lpdf = JHH, countries, logy  = TRUE, ext = "_sim", ...){
 graphit(lpdf, countries, minVal = minVal, myFolderType = "deaths simulated", 
     yvars = c('deaths' %#% ext, 'deaths', "population"), 
     logy = logy, facet = 'PSCR', ...)
}



#legacy. 
graphdarc_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
 graphit(lpdf, countries, xvar = 'day', 
         yvars = c('active_imputed', 'recovered_imputed', "confirmed"), logy = logy, 
         facet = 'PSCR', ...) #putlegend = TRUE
}
graphdarcs_iyl <- function(lpdf , countries, logy = TRUE, ...){
 graphit(lpdf, c(countries, doublingDays % % "days"), xvar = 'day', 
             yvars = c('active_imputed', 'recovered_imputed', "confirmed"), 
             logy = logy, putlegend = TRUE, ...)
}
graph2dac_iyl <- function(lpdf = JHH, countries, minVal  = 100,  logy = TRUE, ...){
 graphit(lpdf, c(countries), minVal, xvar = 'day', 
     yvars = c('active_imputed', "confirmed"), 
     logy = logy, ... ) #putlegend = TRUE, 
}


# for testing imputation quality, do not add numbers otherwise they get done for all regios.
graphDaa_fia <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = "Date", 
         yvars = c('active_imputed', 'active'), 
         area = TRUE, position = 'identity', facet = 'PSCR', ...) 
}

graphDaa_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
  graphit(lpdf, countries, xvar = 'Date', 
     yvars = c( 'active_imputed', 'active'), facet = 'PSCR', 
     logy = logy, ... ) #putLegend = TRUE
}

graphDrr_fia <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = "Date", 
         yvars = c('recovered_imputed', 'recovered'), 
         area = TRUE, position = 'identity', facet = 'PSCR', ...) 
}

myGraphList <- ls(pattern  = "graph")
myGraphNrs <- ls(pattern  = "graph[[:digit:]]")
myGraphListbyDay <- ls(pattern  = 'graphd') 
myGraphListbyDate <- ls(pattern  = 'graphD')


graphs <- function(lpdf  = JHH, countries  = "World", graphlist  = myGraphNrs, ...) {
  #deprecated not used. 
   for (myGraph in graphlist) {
  if (verbose >=  3) print( 'graph:'  % %  myGraph)
  do.call(myGraph, args  = list(lpdf, countries, savename, ...))
 }
}
reportDiffTime <- function(message, startTime, units = 'auto', precision  = 2){
 dt <- round( difftime( Sys.time(), startTime, units = units), precision)
 print(message % % dt 
       % %  ifelse(units == 'auto','units',units))
 dt
}

timer <- function(mycall, message  = 'duration', verbosity  = 1, ...){
 if (verbose > verbosity) {
  tistart  = Sys.time(); print(format(Sys.time(), "%H:%M:%S " )  % %  message)}
 do.call(myCall, ...)
 if (verbose >=  verbosity) {reportDiffTime(myCall %:% message, tistart)}
}

graphOnRegion <- function(lpdf, myRegion, myGraph, saveit = TRUE, ...) {
 if (verbose >= 7) print( 'territories' % %    paste(myRegion, collapse = "/ "))
 if (verbose >= 5) { tir <- Sys.time() ; print(tir %: % myGraph  % %  myRegion[1] ) }
 do.call(myGraph, 
      args = list(lpdf, myRegion, savename  = ifelse(saveit == TRUE, myRegion[1], ifelse(saveit == FALSE, "", saveit)), ...))
 if (verbose >= 6) {
  reportDiffTime(myGraph  % %  myRegion[1] % %  "duration:", tir)}

}

walkThrough <- function(lpdf = ECDC, regions= ECDCRegios, graphlist=myGraphNrs , 
                        saveit = TRUE, ordre = 'RG', ...){
  misreg <- missing(regions)
  if (!misreg)  {
    print(length(regions) % % "regions and" % % length(graphlist) % % "graphs." % %
    "at 5 seconds per graph/region, this would last" % % (length(regions)* length(graphlist)/12) % % "minutes") 
    # "results of 2020-09-18"   # "ECDC graphs 15.83 mins"    # "JHH graphs 32.1 mins"
    if (typeof(regions)  == "character") { regions = list(regions) }
  } 
  if (ordre == 'RG') {
    walk(graphlist, function(myGraph){
      if (misreg) {
        switch(EXPR = 1 + grepl("n", myGraph, fixed=TRUE),
               {regions <- makeDynRegions(lpdf, byVar = "active_imputed")},
               {regions <- makeDynRegions(lpdf, byVar = "net_active_imputed")}
              )
        }
      if (verbose >= 4) {tig = Sys.time(); print(format(Sys.time(), "%H:%M:%S " ) % % myGraph)}
      walk(regions, function(myRegion)  graphOnRegion(lpdf = lpdf, myRegion, myGraph, saveit = saveit, ...) )
      if (verbose >= 5) {reportDiffTime(myGraph, tig)}
    })} else {
    walk(regions, function(myRegion){
      if (verbose >= 4) {tig = Sys.time(); print(format(Sys.time(), "%H:%M:%S " ) % % myRegion[1])}
      walk(graphlist, function(myGraph) graphOnRegion(lpdf  = lpdf, myRegion, myGraph, saveit = saveit, ...) )
      if (verbose >= 5) {reportDiffTime(myRegion[1], tig)}
    })}
}


makeDate <- function(chardate = "", format  = myDateFormat){
 tryCatch(as.Date(chardate, format = format), 
      error = function(e){print(paste("Either enter a date or a string (please use the following Date format for the string:", myDateFormat ))})
}

makeHistoryGraphs <- function(lpdf, regions, graphlist = myGraphNrs, 
                              fromDates, toDates, ...) {
  on.exit({options(warn = 0) }) 
  if (missing(regions) ) stop("no regions to graph")
  if (missing(fromDates) ) {
    if (missing(toDates)) {
        fromDates = min(lpdf$Date)
        toDates  = max(lpdf$Date)
    } else {fromDates <- as.character(rep(min(lpdf$Date), length(toDates)))} 
    } else {if (missing(toDates)) {
      toDates <- as.character(as.Date(fromDates[2:length(fromDates)]) - 1)
      fromDates <- fromDates[1:(length(fromDates) - 1)]
    }}
  walk2(fromDates, toDates, function(from, to){
    if (verbose >= 1) {
      ti_da = Sys.time() 
      print(format(ti_da, "%H:%M:%S ")  % %  "doing"  % %  as.Date(from, origin = "1970-01-01") % % as.Date(to, origin = "1970-01-01"))
    }
    if (nrow(lpdf[lpdf$Date >= from & lpdf$Date <= to, ]) > 0) {
      walkThrough(lpdf, regions, graphlist, saveit = TRUE, from  = from, to  = to, 
                  myFolderDate  = from % % 'to' % % to, ...)
    }
    else print("no data from " % %  from  % % 'to'  % % to )
    if (verbose >= 2) {
      reportDiffTime( as.Date(to, origin = "1970-01-01")  % %  "duration: ", ti_da)}
    while (!is.null(dev.list())) dev.off() 
  })
}

makeHistoryOneGraph <- function(lpdf, regions, graph = 'graph3ard_fia', 
                              fromDates, toDates, ...) {
  on.exit({options(warn = 0) }) 
  if (missing(regions) ) stop("no regions to graph")
  if (missing(fromDates) ) {
    if (missing(toDates)) {
      fromDates = min(lpdf$Date)
      toDates  = max(lpdf$Date)
    } else {fromDates <- as.character(rep(min(lpdf$Date), length(toDates)))} 
  } else {if (missing(toDates)) {
    toDates <- as.character(as.Date(fromDates[2:length(fromDates)]) - 1)
    fromDates <- fromDates[1:(length(fromDates) - 1)]
  }}
  walk2(fromDates, toDates, function(from, to){
    if (verbose >= 1) {
      ti_da = Sys.time() 
      print(format(ti_da, "%H:%M:%S ")  % %  "doing"  % %  as.Date(from, origin = "1970-01-01") % % as.Date(to, origin = "1970-01-01"))
    }
    if (nrow(lpdf[lpdf$Date >= from & lpdf$Date <= to, ]) > 0) {
      walkThrough(lpdf, regions, graph, saveit =  from  % % 'to'  % % to, from  = from, to  = to, 
                  myFolderDate  = graph, ...)
    }
    else print("no data from " % %  from  % % 'to'  % % to )
    if (verbose >= 2) {
      reportDiffTime( as.Date(to, origin = "1970-01-01")  % %  "duration: ", ti_da)}
    while (!is.null(dev.list())) dev.off() 
  })
}

# check th lags between recovered, confirmed and deaths 
ccf.vf <- function(var1 = c(1, 2), var2 = c(2, 2), lag.max  = 30, saveit  = FALSE, 
          plotit  = FALSE, printit  = FALSE){
 title  = paste("ccf of ", var1 , " vs ", var2, ".png", sep  = "")
 if (saveit) {png(filename  = paste("plots/ccf/", title))}
 myplot  = ccf(var1, var2, lag.max  = lag.max, main  = title, plot  = plotit, na.action  = na.omit)
 if (printit) print(myplot)
 if (saveit) {dev.off()} #else {myplot}
 myplot
 }

findMaxCCF <- function(var1 = "new_recovered", var2  = "new_confirmed", myPSCR  = "Hubei,China", 
            lpti  = JHH, N  = 5){
 if (myPSCR  !=  "") lpdf <- lpti[lpti$PSCR  ==  myPSCR, ]
 lpdf <- lpdf[lpdf$Date > "2020-01-22", c("Date", var1, var2)]  #dubious way of excluding potential missing daily growth. 
    #ECDC starts before but has no recovery variable, so we could only use it for DC lags.
 if (all(is.na(lpdf[, var1])) || all(is.na(lpdf[, var2]))) 
  return(data.frame( cor  = NA, lag  = NA)) #
 d <- ccf.vf(lpdf[, var1], lpdf[, var2], lag.max = 30, plotit = FALSE)
 if (verbose >= 2) print(myPSCR)
 res  = data.frame( cor = d$acf[, , 1], lag = d$lag[, , 1])
 if (N %% 2  == 0) N = N - 1
 a <- res[order(res$cor, decreasing = TRUE)[1:N], ]
 if (verbose >= 5) print(a)
 res_max  = median( a$lag) #which.max(res$cor) #instead of max, take the n largest: order(R, decreasing = TRUE)[1:N]
 return(res[res$lag  == res_max, ]) 
} 

findMaxCCFs_deprecated <- function(var1 = "new_recovered", var2 = "new_confirmed", myPSCR = "", lpdf = JHH) {
 a <- ddply( lpdf, "PSCR", function(lpdfp){findMaxCCF(var1 = var1, var2 = var2, myPSCR = myPSCR, lpdf = lpdfp)})
 a[!is.na(a$lag), ]
}
findMaxCCFs <- function(var1 = "new_recovered", var2 = "new_confirmed", myPSCR = "", lpdf = JHH) {
  a <- lpdf %>% group_by(PSCR) %>% do( findMaxCCF(var1 = var1, var2 = var2, myPSCR = myPSCR, lpdf = .))
  a[!is.na(a$lag), ]
}

#end. Now run loadECDC() and loadJHH() and loadTesting() 
# which is done in Output.Rmd/ipynb and Graphs.Rmd/ipynb
