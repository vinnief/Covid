# -*- coding: utf-8 -*-

#suppressPackageStartupMessages(
source("requirements.R")
#)
#Global assumptions
LAGRC <- 42
LAGRD <- 35
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
getEnv <- function(){
  switch(get_os(), 
       windows = {myMessage <- "I run MS Windows"; myPlotPath <<- "G:/My Drive/Covid19"},
       linux   = {myMessage <- "I run Linux"; myPlotPath <<- "~/Covid19_plots"},
       osx     = {myMessage <- "I run OSX" ; myPlotPath <<- "~/Covid19_plots"},
       {myMessage <- 'OS not recognized' ; myPlotPath <<- "~/Covid19_plots"})
switch(.Platform$GUI, 
       RTerm = {myMessage <- myMessage % % "and Jupyter Notebook/Lab."#; myPlotPath <- "G:/My Drive/Covid19_plots"
                }, # Jupyter answers this
       Rgui   = {myMessage <- myMessage % % "and just RGui."},
       RStudio     = {myMessage <- myMessage % % "and RStudio."},
       {myMessage <- myMessage % % "and the GUI is" % % .Platform$GUI })
    myMessage
}
myMessage <- getEnv() 
if (verbose >= 2) message(myMessage)
rm(myMessage)

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
readJHHUS <- function(dataversion = "confirmed"){#deaths and recovered are the other options. 
 filename <- paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_', dataversion, "_US.csv", sep = "")
 tryCatch( wpdf <- read.csv(filename) , 
      error = function(e) message(" The data was not found: Are you sure this file exists? " % % filename % % e)
          )
 return(wpdf)
}

readTesting <- function(col_types = cols(
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
    )){
  testing <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv', 
                      col_types=col_types)  %>%  
    select(c('Entity', 'ISO code','Date', 'Cumulative total', 'Daily change in cumulative total',
             "Cumulative total per thousand", "Daily change in cumulative total per thousand"))  %>%  
    rename(theDate = "Date",
           tests  = "Cumulative total", 
           new_tests = "Daily change in cumulative total", 
           new_tests_per_thousand = "Daily change in cumulative total per thousand",
           ISOcode  = "ISO code") # %>% select(-'Cumulative total', -'Daily change in cumulative total', -'ISO code')
  coco <- as.data.frame(str_split_fixed(testing$Entity, ' - ', n = 2))
  testing$PSCR <- coco[, 1]
  
  testing[testing$PSCR == 'United States','PSCR'] <- 'United States of America'
  testing[testing$PSCR == 'Czech Republic','PSCR'] <- 'Czechia'
  testing$PSCR <- testing$PSCR  %>%  str_replace_all(' ','_')  
  
  testing$comment <- coco[,2]
  testing
}

readJHH <- function(dataversion = "confirmed"){#deaths and recovered are the other options. 
 filename <- paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_', 
          dataversion, "_global.csv", sep = "")
 tryCatch( wpdf <- read.csv(filename) , 
      error = function(e) message("Error" % % e % % " The data was not found: Are you sure this file exists? ", filename)
 )
 return(wpdf)
}
makePSCR <- function(PS, CR){PS %,% CR}

wide2LongJHH <- function(wpdf, coltype = "date", values.name = "count", US = FALSE){ 
 ID <- c("Country.Region", "PSCR", "Province.State", "Lat", "Long")
 if (US) { ID <- c(ID, "Combined_Key")  }
 wpdf$PSCR <- (ifelse(""  == wpdf$Province.State, 
           as.character(wpdf$Country.Region), 
           makePSCR(wpdf$Province.State , wpdf$Country.Region) ))
 lpdf <- reshape2::melt(wpdf, id = ID, 
            variable.name = coltype, value.name = values.name) 
 #lpdf <- wpdf  %>%  pivot_longer(????)
 lpdf$theDate <- as.Date(lpdf[, coltype], format = "X%m.%d.%y")# %>% # %y 2 digits %Y 4d year
                 # format(, "20%y-%m-%d") %>%  #add century, get rid of X
                #  as.Date()   # convert to date again. 
 if (verbose >=2) message("Last JHH date:"& last(lpdf$theDate) )
 #lpdf$date <- NULL
 return(lpdf)
} 

findIDnames <- function(lpdf = JHH, testIDnames = c("Neth", "India"), searchID = "PSCR", 
             fuzzy = TRUE, returnID){
 lpdf <- as.data.frame(lpdf)
 allIDs <- (unique(lpdf[, searchID]))  #error maybe? [ for dataframe 
 if (!fuzzy) {a1 <- intersect(testIDnames, allIDs)
 } else a1 <- allIDs[unlist(llply(testIDnames, function(a) grep(a, allIDs, ignore.case = TRUE)))] 
 # dplyr try: #testIDnames %>% grep( allIDs, ignore.case = TRUE)
 if (missing(returnID)) return ( a1) #returnID = searchID
 else if (searchID  == returnID) {
  if (verbose >= 10) message('no need for returnID if same as searchID')
  return ( a1)} #returnID = searchID
 unique(lpdf[lpdf[, searchID] %in% a1, returnID])
} 

aggreg <- function(avector){
 len <- length(unique(avector))
 if (len  == 1){avector[1]
 }else 
  paste(avector[1], len-2, avector[length(avector)], sep = "_")
}


total_plyr <- function(lpdf = JHH, rows = "", 
         ID = "PSCR" , 
         varnames = c("confirmed", "deaths", "recovered") , 
         newrow = ""
         ) {
 if (rows[1]  == "") rows = unique(lpdf[[ID]])  
 ans <- ddply(lpdf[lpdf[[ID]] %in% rows, ], c("theDate"), 
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
  ans <- lpdf[lpdf[[ID]] %in% rows, ] %>% group_by(theDate) %>% 
    summarize(
      ISOcode  = newrow,
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
      year = ifelse(!"year" %in% names(lpdf), NA, year),
      month = ifelse(!"month" %in% names(lpdf), NA, month),
      monthday = ifelse(!"monthday" %in% names(lpdf), NA, monthday),
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
 if (verbose >= 5) message("Making the total for " % % paste(rows, collapse = "/ ") % % "in" % % ID)
 ans <- ldply(rows, function(a) lpdf  %>%  total(a, ID, varnames, ifelse(needAggreg,  "", a)))
}

totals_dplyr <- function(lpdf = JHH, rows = "", ID = "Country.Region",  # dplyr try. 
                   varnames = c("confirmed", "deaths", "recovered"), needAggreg=TRUE ){
  if (rows[1]  == "") rows = (unique(lpdf[[ID]]))
  if (verbose >= 5) message("Making the total for " % % paste(rows, collapse = "/ ")% %  "in" % % ID)
  ans <- lpdf %>% filter(!!ID %in% rows) %>% group_by(!!ID) %>% 
    group_modify( .f = (function(a) {total( first(a[[ID]]), ID, varnames, ifelse(needAggreg,  "", first(a[[ID]])))}) )
}

correctnames <- function(df){
 names(df)[match("Long_", names(df))] <- "Long"
 names(df)[match("Province_State", names(df))] <- "Province.State"
 names(df)[match("Country_Region", names(df))] <- "Country.Region"
 df[, !names(df) %in% c("Admin2" , "UID", "iso2", "iso3", "code3", "FIPS")]
}

makeJHHUSStates <- function(){
 wc <- readJHHUS('confirmed') 
 #geo.location <- wc[, c("Combined_Key", "Country_Region", "Province_State", "Admin2", "UID", "Lat", "Long_")]
 wc <- correctnames(wc)
 #write.csv( geo.location, file = dataPath %#% "/" %#% "geo.location.US.csv", na = "")
 #rm(geo.location)
 # wc <- wc[, !names(wc) %in% c("Admin2" , "UID", "iso2", "iso3", "code3", "FIPS")]
 confirmed <- wide2LongJHH(wc, values.name = "confirmed", US = TRUE)
 wd <- readJHHUS("deaths")
 wd <- correctnames(wd)
 #wd <- wd[, !names(wd) %in% c("Admin2" , "UID", "iso2", "iso3", "code3", "FIPS")]
 deaths <- wide2LongJHH(wd, values.name = "deaths", US = TRUE)
 lpdf <- merge(confirmed, deaths, all.x = TRUE, 
        by = c("Country.Region", "Province.State", "PSCR", "Combined_Key", "Lat", "Long", "theDate"), sort  = FALSE)
 #tryCatch( wr <- readUSdata("recovered"), error = function(e){message(e)})
 #if ("wr" %in% ls()){
 # wr <- correctnames(wr)
 # recovered <- wide2LongJHH(wr, values.name = "recovered")
 # lpdf <- merge(lpdf, recovered, all = TRUE, 
 #  by = c("Country.Region", "Province.State", "PSCR", "Combined_Key", "Lat", "Long", "theDate"))
 #}else 
 lpdf$recovered <- as.numeric(NA ) #just in case NA totalled into 0. 
 lpdf
}

makeJHHUS <- function() {
 JHHUSStates <<- makeJHHUSStates()
 lpdf <- JHHUSStates  %>%  totals(rows = "",  ID = "Province.State", varnames = c('confirmed', 'deaths'), needAggreg = TRUE) 
    # depends on all provinces being chosen to sum! 
 lpdf$Combined_Key <- NULL
 if (!all(is.na(lpdf$recovered)))
  if ( max(lpdf$recovered, na.rm = TRUE) <= 0) lpdf$recovered <- as.numeric(NA ) #just in case NA totalled into 0. 
 lpdf 
}

makeJHHcountries <- function() {
 wc <- readJHH('confirmed') #"Confirmed")
 geo.location <- wc[c("Country.Region", "Province.State", "Lat", "Long")]
 #write.csv( geo.location, file = dataPath %#% "/" %#% "geo.location.csv", na = "")
 confirmed <- wide2LongJHH(wc, values.name = "confirmed")
 wd <- readJHH("deaths")
 deaths <- wide2LongJHH(wd, values.name = "deaths")
 wr <- readJHH("recovered")
 names(wr)[1] <- names(wc)[1] #"Province.State without strange characters BOM?
 recovered <- wide2LongJHH(wr, values.name = "recovered")
 lpdf <- merge(confirmed, recovered, all.x = TRUE, #, sort = FALSE, 
         by = c("Country.Region", "PSCR", "Province.State", "theDate", "Lat", "Long"), sort  = FALSE)
 lpdf <- merge(lpdf, deaths, all.x = TRUE, 
         by = c("Country.Region", "PSCR", "Province.State", "theDate", "Lat", "Long"), sort  = FALSE)
 lpdf[lpdf$PSCR  == "US", ]$PSCR <- as.character("USA") #to distinguish from the detailed data
 #levels(lpdf$Country.Region) <- c(levels(lpdf$Country.Region), "USA")
 lpdf[lpdf$PSCR  == "USA", "Country.Region"] <- "USA" 
 lpdf
}

updateJHHFromWeb <- function(nameUS = "JHH_US.csv", namenonUS = "JHH_non_US.csv") {
 
 CUS <-  makeJHHUS() %>% as_tibble()
 Cworld <-  makeJHHcountries() %>% as_tibble( )
 rbind.data.frame(Cworld, CUS)#, StringsAsFactors = FALSE)
 }

readLocalData <- function(nameUS = "JHH_US.csv", namenonUS = "JHH_non_US.csv"){
 #CUS <- read.csv(dataPath %#% "/" %#% nameUS, stringsAsFactors = FALSE)#colClasses = ("theDate" = "character"))
 CUS2 <- read_csv(nameUS)
 #Cworld <- read.csv(dataPath %#% "/" %#% namenonUS, stringsAsFactors = FALSE)
 Cworld2 <- read_csv(namenonUS)
 #lpdf <- rbind(Cworld, CUS)
 lpdf2 <- rbind(Cworld2, CUS2)
 #as_tibble(lpdf)
}

makeJHH <- function(name = "JHH", force = FALSE) {
 nameUS <- paste( paste(name, "US", sep = "_"),      "csv", sep = ".")
 namenonUS <- paste( paste(name, "non", "US", sep = "_"),  "csv", sep = ".")
 namedays <- paste(name, "_days.csv", sep = "")
 if (force || (difftime(Sys.time(), file.info(namedays)[, "mtime"], units  = "hours") > 6)) {
  lpdf <- updateJHHFromWeb(nameUS, namenonUS)
  if (verbose >= 1) message("updating JHH from Github")
 } else {
  if (verbose >= 1) message("loading local"% % namedays )
  lpdf <- read.csv(dataPath %#% "/" %#% namedays, stringsAsFactors  = FALSE)
 }
 if (typeof(lpdf$theDate)  == "character") 
  lpdf$theDate <- as.Date(lpdf$theDate, "%Y-%m-%d") #strptime gives timezones! no need for timezones
 if (verbose >= 1) {a = as.numeric(max(lpdf$theDate) - min(lpdf$theDate) + 1)
  message('JHH' %: % a % % "dates" %, % (nrow(lpdf)/a) % % "regions, last date:" % %  
      max(lpdf$theDate) %, % "with"  % % 
      sum(is.na(lpdf[lpdf$theDate >= "2020-02-02", ]))  % % 
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
regios <- c(list(
     EU6 = c("EU6", regios$Benelux[2:4], "Germany", "France", "Italy"), 
     Asia = setdiff(c("Asia", regios$SAsiaIO, regios$SouthEastAsia, 
              regios$SouthWestAsia, regios$EastAsia, regios$China, 
              regios$CIS), 
             c("Madagascar", "East Asia", "South Asia & Indian Ocean", 
              "South East Asia", "South West Asia", "CIS", "Moldova", 'Russia', 
              "Belarus", "Georgia", "Azerbaijan", "Armenia")),
     AfricaSS = c("Africa South of Sahara", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo (Kinshasa)", "Congo (Brazzaville)", "Cote d'Ivoire", "Djibouti", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe"),
     North_Africa = c("Africa", "Algeria", "Egypt", "Libya", "Morocco",  "Tunisia", "Western Sahara")), 
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
     Africa = c("Africa", setdiff(c( regios$North_Africa, regios$AfricaSS), c("Sub Saharan Africa", "North Africa"))), 
     Oceania = c("Oceania", "Australia", "New Zealand", "Vanuatu", "Tuvalu", "Fiji", "Guam", "French Polynesia", "New Caledonia" )
     ), 
   regios)

makePlaces <- function() {
  wc <- read_csv("https://coviddata.github.io/coviddata/v1/places/cases.csv", 
                 col_types = cols(
                   .default = col_double(),
                   Place = col_character(),
                   Region = col_character(),
                   Country = col_character()
                 ))
  confirmed <- gather(wc, "theDate", "confirmed",-c(Place,Region,Country) )
  wd <- read_csv("https://coviddata.github.io/coviddata/v1/places/deaths.csv", 
                 col_types = cols(
                   .default = col_double(),
                   Place = col_character(),
                   Region = col_character(),
                   Country = col_character()
                 ))
  deaths <- gather(wd, "theDate", "deaths", -c(Place,Region,Country))
  wr <- read_csv("https://coviddata.github.io/coviddata/v1/places/recoveries.csv", 
                 col_types = cols(
                   .default = col_double(),
                   Place = col_character(),
                   Region = col_character(),
                   Country = col_character()
                 ))
  recovered <- gather(wr, "theDate", "recovered", -c(Place,Region,Country))
  
  lpdf <- merge(confirmed, recovered, all.x = TRUE, #, sort = FALSE, 
                  by = c("Place", "Region", "Country", "theDate"), sort  = FALSE)
  lpdf <- merge(lpdf, deaths, all.x = TRUE, 
                by = c("Place", "Region", "Country", "theDate"), sort  = FALSE)
  lpdf$PSCR <- lpdf$place
  lpdf
}

##data from Sciensano, belgium 
##  #https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI_Cum.csv
##"NIS5","TX_DESCR_NL","TX_DESCR_FR","TX_ADM_DSTR_DESCR_NL","TX_ADM_DSTR_DESCR_FR","PROVINCE","REGION","CASES"
readBelgiumMuni <- function(){ 
  Bconfirmed <- 
    read_csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI.csv",
             na  = "" , 
             col_types = cols(
                            NIS5 = col_character(), 
                            DATE = col_character(),
                            TX_DESCR_NL = col_character(), 
                            TX_DESCR_FR = col_character(), 
                            TX_ADM_DSTR_DESCR_NL = col_character(),
                            TX_ADM_DSTR_DESCR_FR = col_character(),
                            PROVINCE = col_character(),
                            REGION = col_character(), #REGION
                            CASES = col_integer() #CASES
                            ) 
                   ) %>%  
    #lpti <- lpti %>%
      mutate(theDate = as.Date(DATE), 
             PSCR = TX_DESCR_NL,
             year = year(theDate),
             month = month(theDate),
             monthday = day(theDate), 
             confirmed = cumsum(CASES),
             population = 1)%>%
          #as.Date(dateRep, format = "%d/%m/%Y")) 
      rename(ISOcode  = NIS5, 
             new_confirmed = CASES, 
             Region = REGION,
             )  %>%
      arrange(PSCR, theDate)  %>%  group_by(PSCR)  %>%  
    mutate(deaths  = as.numeric(NA) ,  
           recovered = as.numeric(NA))
}

readBelgiumage <- function(){
  #https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv
  #https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI.csv
  Belgium.confirmed <- 
    read_csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", 
              col_types = cols(DATE = col_date(),
                               PROVINCE = col_character(),
                               REGION = col_character(),
                               AGEGROUP = col_character(),
                               SEX = col_character(),
                               CASES = col_integer()))%>%
    rename(confirmed = CASES, theDate = DATE) %>%
    mutate(PSCR = PROVINCE, recovered = as.numeric(NA), population = 1)
  
  Belgium.mortality <- read_csv("https://epistat.sciensano.be/Data/COVID19BE_MORT.csv", 
                                col_types = cols(DATE = col_date(),
                                                 REGION = col_character(),
                                                 AGEGROUP = col_character(),
                                                 SEX = col_character(),
                                                 DEATHS = col_integer())
                                ) %>% 
    rename(deaths = DEATHS, theDate = DATE) 
  Belgium.cm <- merge(Belgium.confirmed, Belgium.mortality)
  Belgium.population <- read_tsv("data/Belgium data/Belgium Population_per_commune.csv", 
                                 col_types = cols(
                                   NIS5 = col_character(),
                                   `Lieu de Résidence` = col_character(), #<e9>
                                   Hommes = col_integer(),
                                   Femmes = col_integer(),
                                   Total = col_integer())
                                 )
  Belgium.cm = merge(Belgium.cm, Belgium.population, by.x="PROVINCE", by.y ="Lieu de Résidence", 
                     all.x = T, all.y = F)
  if (verbose >= 1) {a = as.numeric(max(Belgium.cm$theDate) - min(Belgium.cm$theDate) + 1)
  message('Belgium ' %: % a % % "dates" %, % length(unique(Belgium.cm$PSCR)) % % 
          "regions, last date:" % % max(Belgium.cm$theDate) %, % "with"  % % 
          sum(is.na(Belgium.cm[Belgium.cm$theDate >= "2020-02-02", ]))  % % 
          "missing values after 2020-02-01")}
  Belgium.Regios <<- makeDynRegions(Belgium.cm, "confirmed", piecename = "Belgium")
  Belgium.cm
}
##https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv
readItaly <- function () {
  
}

readNetherlands <- function(){
  
}

readGermany <- function() {
  
}

readFrance <- function () {
  
}
### data from ECDC - World bank. 
### https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
readECDCdaily <- function(){ #deprecated: since 20201201 the ECDC provides weekly data
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
  mutate(theDate  = as.Date(dateRep, format = "%d/%m/%Y")) %>%
  rename( PSCR = countriesAndTerritories, 
      ISOcode  = countryterritoryCode, 
      confirmed_today = cases, 
      deaths_today = deaths, population  = popData2019, 
      Region = continentExp,
      monthday = day)        %>% 
  select( -geoId,
          #-popData2019,-cases , -countriesAndTerritories, 
          # -continentExp, -countryterritoryCode,
      -dateRep) %>%
  arrange(PSCR, theDate)  %>%  group_by(PSCR)  %>%  
  mutate(confirmed  = cumsum(confirmed_today), 
      deaths  = cumsum(deaths_today), 
      recovered = as.numeric(NA))  %>% 
  select(-confirmed_today, -deaths_today)
 if (verbose >= 1) {a = as.numeric(max(lpti$theDate) - min(lpti$theDate) + 1)
  message('ECDC' %: % a % % "dates" %, % length(unique(lpti$PSCR)) % % "regions, last date:" % %  
     max(lpti$theDate) %, % "with"  % % 
     sum(is.na(lpti[lpti$theDate >= "2020-02-02", ]))  % % 
     "missing values after 2020-02-01")}
 lpti
}
readECDCweekly <- function(){
  lpti <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na  = "" ,
                   col_types = cols(
                     dateRep = col_character(),
                     year_week = col_character(),
                     cases_weekly = col_double(),
                     deaths_weekly = col_double(),
                     countriesAndTerritories = col_character(),
                     geoId = col_character(),
                     countryterritoryCode = col_character(),
                     popData2019 = col_double(),
                     continentExp = col_character(),
                     "notification_rate_per_100000_population_14-days" = col_double()
                   ))  %>%  #fileEncoding  = "UTF-8-BOM" doesn use bom in readr tidyverse. 
    mutate(theDate  = as.Date(dateRep, format = "%d/%m/%Y")) %>%
    rename( PSCR = countriesAndTerritories, 
            ISOcode  = countryterritoryCode, 
            confirmed_today = cases, 
            deaths_today = deaths_weekly, population  = popData2019, 
            Region = continentExp)        %>% 
    select( -geoId,
            #-popData2019,-cases , -countriesAndTerritories, 
            # -continentExp, -countryterritoryCode,
            -dateRep) %>%
    arrange(PSCR, theDate)  %>%  group_by(PSCR)  %>%  
    mutate(confirmed  = cumsum(confirmed_today), 
           deaths  = cumsum(deaths_today), 
           recovered = as.numeric(NA))  %>% 
    select(-confirmed_today, -deaths_today)
  if (verbose >= 1) {a = as.numeric(max(lpti$theDate) - min(lpti$theDate) + 1)
  message('ECDC' %: % a % % "dates" %, % length(unique(lpti$PSCR)) % % "regions, last date:" % %  
            max(lpti$theDate) %, % "with"  % % 
            sum(is.na(lpti[lpti$theDate >= "2020-02-02", ]))  % % 
            "missing values after 2020-02-01")}
  lpti
}

correctMissingLastDay <- function(lpti = ECDC0){
  maxDate <- max(lpti$theDate)
  lpti <- lpti %>% group_by(PSCR) 
  missingPSCR <- setdiff( unique(lpti$PSCR) ,
                          lpti %>% filter(theDate == maxDate) %>% pull(PSCR) )
  for (myPSCR in missingPSCR) {
    countryData <- filter(lpti, PSCR == myPSCR )
    countryLastDate <- max(countryData$theDate)
    missingRows <- countryData %>% filter( theDate == countryLastDate)
    countryLastDate <- as.Date(countryLastDate, format = '%y-%m-%d')
    neededExtraDates<- unique(lpti[lpti$theDate > countryLastDate, "theDate"]) #as.Date(maxDate, format = '%Y-%m-%d') - lastDate %y instead of %Y?
    missingRows <- missingRows[rep(1, nrow(neededExtraDates)) ,]
    missingRows <- missingRows %>% mutate(theDate = neededExtraDates$theDate,
                                          year_week= year(theDate) %_% week(theDate)) #as.Date(CountryLastDate + row_number(),  origin = '1970-01-01'))
      if (verbose >= 5) {message("missingRows ");View(missingRows)}
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
  message("Regions added:" % %  length(unique(lpdf[[varname]])) )
  if (sum(is.na(lpdf[[varname]])) > 0) {
   message("Not attributed regions top 20:"  % % 
   ( lpdf[is.na(lpdf[[varname]]), ]$PSCR %>% unique %>% head  %>% paste ( collapse = "; ") ) )
  }
 }
 lpdf
}

sortIDlevels <- function(lpdf, varname  = "active_imputed", ID  = "PSCR", ondate){
  if (missing(ondate)) { 
    theDateData <- lpdf[, c(varname, ID, 'theDate')]  %>%  group_by(PSCR)  %>%  
      filter(theDate  ==  max(theDate))  %>%  ungroup
  } else {
    theDateData <- lpdf[lpdf$theDate  ==  ondate, c(varname, ID, 'theDate')]
    if (nrow(theDateData)  ==  0)
      stop("Cannot sort if the date is not present in the Data")
  }
  if (nrow(theDateData)  !=  NROW(unique(lpdf$PSCR)) & (verbose >=  3)) {
    message(' sorting just lost you these countries'  % %  
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

makeDynRegions <- function(lpti = JHH, byVar = "active_imputed", gridsize = 5*6, piecename = 'World', ratio = 5) {
  lpti <- lpti %>%  group_by(PSCR)  %>%  
    filter( theDate  == max(theDate)) %>%  ungroup  #%>% 
    #select( PSCR, confirmed, active_imputed) %>%  
    #arrange(desc(active_imputed)) 
    #   arrange(desc(!!enquo(byVar)) ) !! does not work. {{}} neither.
  lpti <- lpti[order(-lpti[[byVar]]), , drop = FALSE]  #why not sortbyvar?
  
  nr = 1
  mylist = vector(mode  = "list", length  = 0)
  while (nrow(lpti) > gridsize) {
    if (verbose >= 2) message(piecename %#% nr % % paste(lpti$PSCR[1:3], collapse = "/"))
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

provincializeJHH <- function(lpdf = JHH){
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
 c(  
  lpti  %>%  makeDynRegions(piecename = piecename % % 'World'), 
  lpti  %>%  filter(PSCR %in% c("EU",regios$Europe) )  %>%  
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

 if (verbose >= 2 || length(popunknown) > 0) message("population unknown:"  % %  

      ifelse(length(popunknown)  ==  0, 0, paste(popunknown, collapse = "; ")))
 lpdf
}


imputeRecovered <- function(lpdf = ECDC, lagrc = LAGRC, lagrd = LAGRD, 
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
 if (verbose >= 5) message("imputing recovered for:" % % 
            length(unique(lpdf[rowstodo, ][["PSCR"]]))
             % %  'Regions.')
 if (verbose >= 7) {message('imputing territories:' );message(paste(unique(lpdf[rowstodo, ][["PSCR"]]), collapse = "; "))}
 if (sum(rowstodo)  == 0) return(lpdf)
 lpdf <- lpdf %>%  group_by(PSCR)  %>% 
  mutate_cond(rowstodo, imputed = TRUE )
    # %>%    mutate_cond(rowstodo, recovered = recovered_imputed)
}

frac <- function(n, d){ifelse(d !=  0, n/d, NA)}

diff.sl <- function(avector, n = 1, fill = NA){c(rep(fill, n), diff(avector, n))} #sl for same length?
p_M <- function(quantity, population) 1e6*quantity/population

extravars <- function(lpdf, lagrc = 0, lagdc = 0){
 tempwarn <- getOption("warn")
 options(warn = -1)
 on.exit(options(warn = tempwarn))
 if (!"year" %in% names(lpdf)) lpdf$year <- year(lpdf$theDate)
 if (!"month" %in% names(lpdf)) lpdf$month <- month(lpdf$theDate)
 if (!"monthday" %in% names(lpdf)) lpdf$monthday <- day(lpdf$theDate)
 lpdf <- lpdf  %>%  ungroup  %>%  
  arrange(PSCR, theDate)  %>% 
  group_by(PSCR)  %>%  
  mutate( active           =  confirmed - deaths - recovered, 
      active_imputed       =  confirmed - deaths - recovered_imputed, 
      new_confirmed        =  (diff.sl(confirmed)), #mac
      net_active           =  (diff.sl(active)), #mac  # bug? why is the first element of net_active not NA and first element of new_confirmed is NA? 
      net_active_imputed   =  (diff.sl(active_imputed)), #mac
      new_recovered        =  (diff.sl(recovered)), #mac
      new_recovered_imputed=  (diff.sl(recovered_imputed)), #mac
      new_deaths           =  (diff.sl(deaths)), #mac
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
      if (verbose >=  5) message('doublingline:1 doublindDays = '  % %  doublingDays)
      growthRate  = 2^(1/doublingDays)}
    else doublingDays <- 1/log2(growthRate)
  if (NROW(lpti)  ==  0) stop("No country '" %#% country %#% "'in the data")
 }
 if (!missing(nrRows)) maxDate <- max(lpti$theDate) + nrRows
 else {maxDate <- max(lpti$theDate)}
 if (verbose >=  5) message('doublingline: doublindDays = '  % %  doublingDays)
 if (missing(growthRate)) {growthRate  = 2^(1/doublingDays)}
 nrRows <- maxDate - min(lpti$theDate) + 1
 if (maxDate  ==  -Inf) stop("Max date equals -inf. Probably we have an empty data set. Did you choose the right country? ")
 if (verbose >=  3) message('doublingLine:'  % %  "R0 = "  % %  round(2^(lagrc/doublingDays) - 1, 2) %, % 'Simulated'  % %  nrRows % % 'days until'  % %  maxDate)
 out = tibble(theDate = seq(from = min(lpti$theDate), to = maxDate, by = 1))
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
  mutate(theDate  = theDate + 1, 
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
 if (verbose >=  4) {message('doublingline made' );message(out%>% tail)} #to compare with growonce results. 
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
  message('addsimcountry:'  % % paste(missingCols, collapse = ',') % %  'filled with NAs')
 out[missingCols] <- NA
 lpti <- rbind(lpti, out ) # bug: need to adjust the names of simulated variables in out.
 lpti
}

# from R0 to doubling days: #2^(LAGRC/dd) = (R0+1)   1 should yield LAGRC. 
R02doublingDays <- function(R0 = 1){
 LAGRC/(log2(R0 + 1))
}
doublingDays2R0 <- function(doublingDays = 3) {2^(LAGRC/doublingDays) - 1}


estimateDoublingDaysOneCountry <- function(lpti, variable = 'confirmed', nrDays = 9, minDate = "2019-12-31", maxDate = '2021-12-31'){
 getGR <- function(rowNr){
  lptiSel <- lpti[(rowNr - nrDays + 1):rowNr, ] #potential Bug: assumes data sorted by increasing date! is it? should be!
  if (sum(!is.na(lptiSel[[variable]])) >= 3 ) {
   #https://win-vector.com/2018/09/01/r-tip-how-to-pass-a-formula-to-lm/
   f <- as.formula('log2(' %#% variable %#% ') ~ theDate')
   growthRate <-  lm( f, data  = lptiSel, na.action  = na.exclude )$coefficients[['theDate']] 
          }
  else {growthRate <- NA
  if (verbose >=  3) message('addDoublingDays'  % %  lpti$PSCR[1]  % % 
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
   message("AddSimVars: no country given, simulating:"  % %  length(countries)  % %  'countries' )
   if (verbose >=  4) message(paste(countries, collapse  = "/"))}
 }
 if (!('confirmed' %#% ext %in% names(lpti))) {
   lpti[, c('confirmed', 'active', 'recovered', 'deaths') %#% ext ] <- NA}
 lptivalid <- lpti[ lpti$confirmed >=  minVal & lpti$theDate >=  minDate & lpti$theDate <=  maxDate, ]
 for (country in countries) {
   nrRows <- NROW(lptivalid[lptivalid$PSCR  ==  country, ])
   if ( nrRows  <=  0) { 
     if (verbose >=  4 )
       {message('addsimvarsCountry not simulated'  % %  country  % %  'minVal = '  % %  minVal )
       }
   }
  else {
    details <- lptivalid[lptivalid$PSCR  ==  country, ][1, ]
    temp <- simulGrow(lptivalid[lptivalid$PSCR  ==  country, ], country, ...) 
    names(temp)[names(temp) %in% c('confirmed', 'deaths', 'recovered', 'active')] <- 
      names(temp)[names(temp) %in% c('confirmed', 'deaths', 'recovered', 'active')] %#% ext
    #c('confirmed', 'deaths', 'recovered', 'active') %#% ext
    newnrRows <- nrow(temp)
    if (any(is.na(lpti[lpti$PSCR == country ,c('confirmed','theDate')]))) { 
      message("Simulation issue:" % % country % % 'nrows' % % nrRows % % newnrRows) 
      message(which((is.na(lpti[lpti$PSCR == country ,c('confirmed','theDate')]))))}
    lpti[lpti$PSCR == country & lpti$confirmed >= minVal & lpti$theDate >= minDate & 
         lpti$theDate <=  maxDate, c('confirmed', 'deaths', 'recovered', 'active') %#% ext] <- 
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
   else if (newnrRows < nrRows) message(country  % %  'has' % %  newnrRows % % 
                                      'instead of' % % nrRows  % %  'rows. this might mean you have missing data? Oi va voi!')
  }
 }
 lpti
}

graph_DemoDoubling <- function(lpti = ECDC, doublingDays = 3, nrRows = -1){
 simulGrow(lpti, "France", minVal = 10, doublingDays)  %>%  
  graphit("France" % % doublingDays  % %  "days", xvar = 'day', yvars = c('active', 'recovered', 'deaths', 'confirmed', "population") , logy = TRUE, to = max(.$theDate))  %>% 
  .[c('deaths', 'active', 'confirmed', 'recovered')]  %>%  view  
}


addTotals3 <- function(lpti = ECDC, totregions , ID = 'Region'){
 if (missing(totregions)) totregions <- c(unique(lpti[[ID]]))
 if (!('Lat' %in% names(lpti))) lpti$Lat <- NA  
 if (!('Long' %in% names(lpti))) lpti$Long <- NA
 lpti1 <- lpti  %>% filter(!(!!ID %in% totregions)) 
  #just to be sure, that if i do it twice i dont get double counts. We will get double entries tho! 
 #this might show up in the graphs or not depending on sorting. 
 varnames  = c("confirmed",  "deaths", "population") 
 lpti <- rbind(lpti, lpti  %>%  total("", varnames = varnames, newrow = "World")) 
 for (regio in totregions[!is.na(totregions)])
   lpti <- rbind(lpti, 
                 lpti1  %>%  
                   total(regio , ID = ID, varnames = varnames, newrow = regio))
 totCountries <- c(regios$EU, regios$CIS, regios$MENA, regios$AfricaSS)
 regio = regios$Benelux
 ECDC2 <<- lpti1  %>%  
   total(regio , ID = 'PSCR', varnames = varnames, newrow = regio)
 for (regio in totCountries)
   ECDC2 <- rbind(ECDC2, 
                 lpti1  %>%  
                  total(regio , ID = 'PSCR', varnames = varnames, newrow = regio))
  
 lpti
} #bug. check ECDC2 and then integrate into LPTI if it is correct. the current world totals are no good in lpti. 

addCountryTotals <- function(lpdf = JHH, varnames = c("confirmed","recovered", "deaths","population")){
  existingTotals <- c("China","Australia","Canada",'US')
  #just to be sure, that if i do it twice i dont get double counts. 
  #And omit US as country and US states. 
  lpti <- lpdf  %>% 
    filter(!(PSCR %in% existingTotals )) #
  rbind(lpti, 
        lpti  %>%  totals(c("China","Australia", "Canada"), #,'US' #adds US but we have USA already. 
                        ID = "Country.Region", varnames = varnames, needAggreg = FALSE))
}

addRegionTotals <- function(lpdf = JHH, totRegions = c("South_America", "Asia", "Africa", "Europe", "EU",'North_America',"World"), 
                            varnames = c("confirmed","recovered", "deaths","population") ){
  existingTotals <- c("China","Australia","Canada",'US')
  #newTotals <- 
  lpti <- lpdf  %>%  filter(!(PSCR %in% c(totRegions, existingTotals)))
  #just to be sure, that if i do it twice i dont get double counts. 
  #And omit US as country and US states. 
  if ( 'Country.Region' %in% names(lpti))   #Indirect test for JHH
    lpti <- lpti  %>%  filter(!(Country.Region == "US")) #get rid of the US individual states as well as we have USA in JHH
  #lpti <- lpti  %>%  filter( ! PSCR %in% existingTotals ) #avoid double counting
  World <- unique(lpti[['PSCR']])
  
  if (verbose >= 5) {
    message('world totals include the following countries: ')
    message(paste(World, collapse = ","))}
  
  for (myRegion in regios[totRegions]) {
    lpdf <- rbind(lpdf, 
                  total(lpti, myRegion, ID = 'PSCR', newrow = myRegion[1], varnames = varnames))
  }
  lpdf
}

loadJHH <- function() {
  ti <-  Sys.time()
  JHH <- makeJHH(force = TRUE) 
  if (verbose >= 1) reportDiffTime('loading and melting JHH:',ti,'secs')
  JHHExtra<<- JHH[JHH$PSCR == "Repatriated Travellers,Canada", ]
  JHH0 <- JHH[JHH$PSCR != "Repatriated Travellers,Canada", ]
  regios <<- c(list(World = c('World', unique(JHH[['PSCR']]))), provincializeJHH(JHH), regios) 
  ti <-  Sys.time()
  JHH <- JHH0 %>%
    addPopulation() %>% addCountryTotals() %>% addRegionTotals() %>%
    addRegions( Regiolist = regios) %>% arrange(PSCR,theDate) %>%
    imputeRecovered() %>% extravars()#
  if (verbose >= 1) reportDiffTime('adding population, totals, imputations, and daily vars in JHH:',ti,'secs')
  
  #ti = Sys.time()
  #JHH <- addSimVars(JHH, minVal = 100) %>% 
  #  addSimVars(minDate = Sys.Date() - 10, ext = "_endsim")
  #reportDiffTime('adding the simulated values in JHH:',ti,'mins')
  
  JHH.Regios <<- makeRegioList(JHH)
  writeWithCounters(JHH,name = "Covid19JHH")
  JHH
} 

#same with ECDC
loadECDCdaily <- function() {
  tim = Sys.time()
  ECDC0 <- readECDC()
  if (verbose >= 1) reportDiffTime('load ECDC:',tim,'mins')
  tim = Sys.time()
  ECDC  <- ECDC0 %>% correctMissingLastDay() %>% 
    addTotals3 %>% imputeRecovered %>%  extravars %>%
    mutate(Country.Region = PSCR) 
  #%>%
   # addDoublingDaysPerCountry(variable = 'active_imputed') %>%
    #addDoublingDaysPerCountry(variable = 'confirmed') 
  if (verbose >= 1) reportDiffTime('correct, add totals, imputations, vars, doubling days in ECDC:',tim,'mins')
  
  #tim = Sys.time()
  #ECDC <- ECDC %>%  
  #addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") #%>% #, maxDate = Sys.Date() - 1
  # Because of missing Spain data, growth in Europe on last day is negative. hence sim does not work
  #ECDC <- ECDC %>% addSimVars(minVal = 100)  #gives errors. cayman islands follows conveyance_Japan
  #if (verbose >= 1) reportDiffTime('adding the simulated values in ECDC:',tim,'secs')
  ECDC.Regios <<- makeDynRegions( ECDC, piecename = 'ECDC World')
  ECDC
}
loadECDC <- function(NrDays=7) {
  tim = Sys.time()
  if (NrDays == 7) 
    ECDC0 <- readECDCweekly()
  else 
    ECDC0 <- readECDCdaily()
  if (verbose >= 1) reportDiffTime('load ECDC:',tim,'mins')
  tim = Sys.time()
  ECDC  <- ECDC0 %>% correctMissingLastDay() %>% 
    addTotals3 %>% imputeRecovered(lagrc = LAGRC%/% NrDays, lagrd = LAGRD%/%NrDays) %>%  extravars %>%
    mutate(Country.Region = PSCR) 
  #%>%
   # addDoublingDaysPerCountry(variable = 'active_imputed') %>%
    #addDoublingDaysPerCountry(variable = 'confirmed') 
  ECDC2  <- ECDC2 %>% imputeRecovered %>%  extravars %>%
    mutate(Country.Region = PSCR) 
  if (verbose >= 1) reportDiffTime('correct, add totals, imputations, vars, doubling days in ECDC:',tim,'mins')
  
  #tim = Sys.time()
  #ECDC <- ECDC %>%  
  #addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") #%>% #, maxDate = Sys.Date() - 1
  # Because of missing Spain data, growth in Europe on last day is negative. hence sim does not work
  #ECDC <- ECDC %>% addSimVars(minVal = 100)  #gives errors. cayman islands follows conveyance_Japan
  #if (verbose >= 1) reportDiffTime('adding the simulated values in ECDC:',tim,'secs')
  ECDC.Regios <<- makeDynRegions( ECDC, piecename = 'ECDC World')
  ECDC
}

loadTesting <- function() {
  testing <- readTesting()
  write.csv(testing, myPlotPath %//% "data" %//% 'testing.csv' )
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
       lpdf[[counter]] <- seq_along(lpdf$theDate)
       lpdf}
      )
  lpdf
}

addcounterfrommin_dplyr <- function(lpdf = JHH, minv = 0, varname = "confirmed", ID = "PSCR", counter = "day"){
  lpdf[, counter] <- as.numeric(NA)
  lpdf <- lpdf %>%  filter(!is.na(!!varname))  
  if (sum(lpdf[, varname] >= minv) > 0) 
    lpdf[lpdf[[varname]] >= minv, ] <- lpdf[lpdf[[varname]] >= minv, ] %>% group_by(!!ID) %>%
        mutate(!!counter := seq_along(theDate)
    )
  lpdf
}


### make day vars for tableau & Excel
makecountname <- function(countname, minv){paste(countname, minv, sep = "_")}

writeWithCounters <- function(lpdf = JHH, name = "JHH", varname = "confirmed", ID = "PSCR", na = "") {
  lpdf <- as.data.frame(lpdf)
   lpdf <- lpdf[!is.na(lpdf[c(varname)]), ]
  for (minv in c(1, 20, 100, 400, 1000, 2000, 5000, 1e4, 1e5, 1e6, 1e7)) {
   lpdf <- addcounterfrommin(lpdf = lpdf, minv = minv, 
                varname = varname, ID = ID, 
                counter = makecountname("day", minv))
  }
  filename = paste(name, "days.csv", sep = "_")
  write_csv(lpdf, path = dataPath %#% "/" %#% filename, na = na)
  write_csv(lpdf, path = myPlotPath %//% "data" %//% filename, na = na)
  if (verbose >= 1) message("Written the current data with counters to disk as" % % filename % % "for use in Tableau or Excel")
}

#some output tables. 
days2overtake <- function(lpti = JHH ,
                          #countries = c('Belgium', 'China', 'Russia', 'Netherlands', 'Colombia', 'Iran'), 
                          varname = 'confirmed', newvarname = 'new_confirmed'){
  #'listed by decreasing nr of confirmed, and smallest is equals to largest in so many days / so many days ago')
  
  lastdata <- lpti#[lpti$PSCR %in% countries,]  %>%     filter(theDate  == max(theDate))
  
  lastdata <- (lastdata[order( -lastdata[[varname]]), ])
  variable <- lastdata[[varname]] 
  names(variable) <- lastdata$PSCR
  newvar <-  lastdata[[newvarname]]
  names(newvar) <- lastdata$PSCR
  mconf <- outer(variable, variable, `-`)
  mnew <- -(outer(newvar, newvar, `-`))
  round(mconf/mnew, 1)
}

overtakeDays_v <- function(lpti, country, who = 'theyme', varname = 'confirmed', nr = 10, lastDays = 7){
  if (varname %in% c('active', 'active_imputed', 'active_p_m', 
                     'active_imputed_p_M')) 
    prefix <- 'net_'
  else if (varname %in% 
           c('confirmed', 'confirmed_p_M', 
             'recovered', 'recovered_p_M', 'deaths', 'deaths_p_M', 
             'recovered_imputed', 'recovered_imputed_p_M')) 
    prefix <- 'new_'
  newvarname <- prefix %#% varname
  lastdata <- lpti[, c('PSCR','theDate', varname, newvarname)]  %>%  group_by(PSCR)  %>%  
    filter(theDate >= max(theDate) - lastDays + 1)  %>% 
    mutate(!!newvarname := ma( .data[[newvarname]],lastDays))  %>%   
    #mutate_at( .vars = 4, .funs = ma)  %>% 
    filter(theDate  == max(theDate))
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
overtakeDays_df <- function(lpti, country, ...){# country ,
  vec <- overtakeDays_v(lpti, country, ...)     # country = country , 
  if (is.null(names(vec))) names(vec)  = rep('?', length(vec)) 
  #else 
  tib <- tibble(country = names(vec), Days = vec)
  names(tib)[1] <- names(vec)[1]
  names(tib)[2] <- 'days' % % initials(country)
  tib[2:nrow(tib), ]
}

smoothem.tib<- function(tib, varNames = c("net_active", "net_active_imputed"), n = FALSE,  ...){ 
  if (n != FALSE & n!= 1) 
    {if (!"data.frame" %in% class(tib)) {tib <- tib[[1]] 
      message("smoothem.tib found a list, not a tibble/df. It deals with the first list element only")
    }# avoid a list of data.frames by taking the first element
    varNames <- tib %>% select(all_of(varNames)) %>% names
    tib[varNames] <- varNames %>% set_names %>% 
      map_dfc(function(colname) {   ma(tib[[colname]],n = n, ...) })
  }
  tib
} #  default n = 1, i.e. no smoothing # bug: need to call this on one country! (altho for the last few days this does not matter.)

smoothem.lpti<- function(lpti, n = FALSE,  ...){ 
  if (!(n == FALSE | n== 1)) {
    if (verbose >= 8) message("smoothem.lpti smoothing a "% % class(lpti))
    nesttib <- lpti %>% group_by (PSCR) %>% 
      nest %>%  
      mutate(data = map(data, (function (tib) {smoothem.tib(tib, n = n,...)} )) ) #%>% 
    #browser()
    nesttib %>%   unnest(cols = c(data))
    }
  else lpti
} #  default n = FALSE or n= 1), i.e. no smoothing 


dataprep <- function(lpdf = JHH, minVal = 1, ID = "PSCR", 
                     xvar = "day", yvars = c("confirmed", "recovered"), 
                     logx = FALSE, logy = TRUE, sorted = TRUE, smoothvars = yvars, smoothn = FALSE){
  usefulvars <-  c( "confirmed", "theDate","year","month","monthday", ID, yvars)
  if (!(xvar %in% names(lpdf)) ){
    lpdf <- lpdf[, usefulvars] 
    lpdf <- addcounterfrommin(lpdf, minVal, varname = "confirmed", ID = ID, counter = xvar)
  } else lpdf <- lpdf[, c(usefulvars,xvar)] 
  if (verbose >= 13) {message("before smoothing") ; browser()}
  if (smoothvars[1] != FALSE)
    lpdf <- lpdf %>%  smoothem.lpti(varNames = smoothvars, n = smoothn)
  if (logy){ #get rid of negative and zeros for the log
    eps <-  1e-5
    for (varname in yvars) {
      if (sum((!is.na(lpdf[, varname])) & lpdf[, varname] <=  eps) > 0) 
        #if all NAs, one row replaces zero rows -> error!
        lpdf[(!is.na(lpdf[, varname])) & lpdf[, varname] <=  eps, varname] <- NA 
    } }
  if (logx) lpdf[lpdf[[xvar]] <= 0, xvar] <- NA #was 1
  if (sorted) {
    lpdf[[ID]] <- sortIDlevels(lpdf = lpdf, varname = yvars[1]) #why not sortbyVar?
    if (verbose >=  7) {message('dataprep ID before sort:');message(unique(lpdf[[ID]]))}
    lpdf <- lpdf[order(lpdf[[ID]]), ] #, lpdf[[xvar]]
    if (verbose >= 7) message('Dataprep first 10 IDs after sort:'  % %  
                              paste(head(levels(lpdf[[ID]]),10), collapse = '-'))
  } 
  lpdf
} 
#Bug potential: after the sort, PSCR is a factor. before, it was character!

# +
#geom_point(shape = "\u2620", size  = 4) #skulls
# + scale_color_manual(values = c('#999999', '#E69F00', '#56B4E9'))

source("graphit.R") # i.e. ignore above definition

graphCodes <- function(){ 
 message('naming system:')
 message(list(nrvars = 1:6, 
     letters = tibble(codes = c('d/D/c/d', 'acrd_', 'f', 'i', 'M', 'n', 'y', 'a/l'), 
     meaning = c('day, theDate, confirmed, death xvar', 'initials/abbrev of yvars', 'facet per ID', 
          'imputed', 'per million', 'new', 'logy', 'area or line')
     )  ))
 writeLines('example graph1Da_fiMnyl: active by Date,  facet (by ID), use imputed, per Million, new (not cumulative), logy, line plot). 

The variable "myGraphNrs" contains all daily interesting graph function names one can pass as a parameter to walkThrough or makeHistoryGraphs.')
}

initials <- function(text = c('test_1', 'of_imputation', 'new_recovered_imputed_per_Million')){
 paste(unlist(lapply(
  lapply ( strsplit(text, '_'), function(st) substr(st, 1, 1)), 
  function(vec) paste(vec, collapse = "_")
 )), collapse = '+')
}
rm(list = ls(pattern = "graph[[:digit:]]"))
#in alfphabetical order of outputs

graph1aa_finl <- function(lpti = JHH, countries, xvar = 'net_active_imputed', 
                          yvars = c('active_imputed'), facet = 'PSCR',
                          smoothvars = xvar, smoothn = 7, labmeth = "dl_top.qp",
                          slope = 20, putlegend = T, ...){
  graphit(lpti, countries, xvar = xvar, facet = facet,
          yvars = yvars, 
          smoothvars = smoothvars, #if(missing(smoothvars)) xvar else smoothvars, 
          smoothn = smoothn, slope = slope, labmeth = labmeth, putlegend = putlegend,
          ...)
}

graph1aa_fiMnl <- function(lpti = JHH, countries,
                           xvar = 'net_active_imputed_p_M', 
                           yvars = c('active_imputed_p_M') ,
                           smoothvars = xvar, #"net_active_imputed_p_M",
                           ...){
  graph1aa_finl(lpti, countries, xvar = xvar, yvars = yvars, 
                smoothvars = smoothvars, ...)
}
graph1cd_finl <- function(lpti = JHH, countries, xvar= "new_confirmed", yvars = "new_deaths",  
                          smoothvars = c(xvar, yvars), slope = deathRate, ...){
  graph1aa_finl(lpti, countries, xvar = xvar, yvars = yvars, smoothvars = smoothvars,
                slope = slope, ...)
}

graph1Da_finl <- function(lpdf = JHH, countries, facet = 'PSCR', smoothn = 7, ...){
  graphit(lpdf, countries, xvar = 'theDate', 
          yvars = c("net_active_imputed"), facet = facet, putlegend = TRUE, smoothn = smoothn, ...)
}
graph1Da_fil <- function(lpdf = JHH, countries, logy = FALSE,  facet = 'PSCR', ...){
  graphit(lpdf, countries, xvar = 'theDate', yvars = c("active_imputed"), 
          , facet = facet, ...)
}

graph1dr_iyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
  graphit(lpdf, countries, xvar  = 'deaths', yvars  = c('recovered_imputed'), 
          logy  = logy, logx  = TRUE, ...)
}

graph1Drr_il <- function(lpdf = JHH, countries, myFolderType  = '', ...){
  graphit(lpdf, countries, xvar = "theDate", 
          yvars = c('recovered_imputed_per_confirmed'), 
          myFolderType = myFolderType %#%"recovery rate", 
          putlegend = FALSE , ...   )
}

graph3Dard_fia <- function(lpdf = JHH, countries, yvars = c('active_imputed', 'recovered_imputed', 'deaths'), ...){
 graphit(lpdf, countries, xvar = "theDate", area  = TRUE, facet  = 'PSCR', 
     yvars = yvars,  ...) 
}

#new --
graph3Dard_fina <- function(lpdf = JHH, countries, smoothn = 7,...){
 graph3Dard_fia(lpdf, countries, smoothn = smoothn,
     yvars = c('net_active_imputed', 'new_recovered_imputed', 'new_deaths'), ...) 
}


graph6Dardcra_fiyl <- function(lpdf = JHH, countries, logy  = TRUE, xvar = 'theDate',
                        yvars = c('active_imputed', 'recovered_imputed', 'deaths', 
                                  "confirmed", 'recovered', 'active'),  ...){
  graphit(lpdf, countries, xvar = xvar, logy = logy, facet  = 'PSCR', 
          yvars = yvars, ...)
}

graph6Dardcra_finyl <- function(lpdf = JHH, countries, logy  = TRUE, smoothn = 7, 
              yvars = c('net_active_imputed', 'new_recovered_imputed', 'new_deaths', 
                        'new_confirmed', 'new_recovered', 'net_active'), ...){
  graph6Dardcra_fiyl(lpdf, countries, logy = logy, smoothn = smoothn,
     yvars = yvars,  ...) 
}

graph6Dardcra_fiMyl <- function(lpdf  = JHH, countries, 
                yvars = c('active_imputed_p_M', 'recovered_imputed_p_M', 
                          'deaths_p_M', 'confirmed_p_M', 'recovered_p_M',
                          'active_p_M'), ...){
  graph6Dardcra_fiyl(lpdf, countries, yvars = yvars, ...)
}

graph6Dardcra_fiMnyl <- function(lpdf = JHH, countries, yvars = 
     c( 'net_active_imputed_p_M', 'new_recovered_imputed_p_M', 
        'new_deaths_p_M', 'new_confirmed_p_M', 'new_recovered_p_M', 'net_active_p_M'),
                ...){
  graph6Dardcra_finyl(lpdf, countries, yvars = yvars, ...)
}

graph1Dc_fnl <- function(lpdf = JHH, countries,  facet = 'PSCR', smoothn = 7, ...){
 graphit(lpdf, countries, xvar = 'theDate', 
         yvars = c("new_confirmed"), facet = facet, putlegend = TRUE, smoothn = smoothn, ...)
}
graphDc_fl <- function(lpdf = JHH, countries, logy = FALSE, ...){
 graphit(lpdf, countries, xvar = 'theDate', yvars = c("confirmed"),  facet = 'PSCR', ...)
}

#other graphs

graphDg_fyl <- function(lpdf = JHH, countries, logy  = TRUE, ...){
 graphit(lpdf, countries, xvar = 'theDate', 
         yvars  = c('confirmed_growthRate'), 
         logy  = logy,  ...)
}
graphDggnar_fiyl <- function(lpdf = JHH, countries, logy = TRUE, facet = 'PSCR', smoothn = 7,  ...){
 graphit(lpdf, countries, xvar = 'theDate', smoothn = smoothn, 
         yvars = c('active_imputed_growthRate', 'confirmed_growthRate', "new_active_rate"), 
         logy = logy, intercept  = stableRate, facet = facet, ...)
}

graph1dnar_iyl <- function(lpdf  = JHH, countries, minVal = 10, logy = TRUE, smoothn = 7,  ...){
  graphit(lpdf, c(countries), minVal, xvar = 'day',  smoothn = smoothn,
          yvars = c('new_active_rate'), logy = logy, intercept = stableRate, ...) 
}

graphDgnar_fiyl <- function(lpdf = JHH, countries, logy = TRUE, facet = 'PSCR', smoothn = 7, ...){
 graphit(lpdf, countries, xvar = 'theDate', smoothn = smoothn, 
         yvars = c("new_active_rate", 'active_imputed_growthRate'), 
         logy = logy,  intercept = stableRate, facet = facet, ...)
}

graph2crd_il <- function(lpdf = JHH, countries, ...){
  graphit(lpdf, countries, xvar = 'confirmed', 
         yvars = c('recovered_imputed', 'deaths'), slope = 1, labmeth = "dl_top.points", ...)
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
 graphit(lpdf, countries, xvar = "theDate", 
         yvars = c('active_imputed', 'active'), 
         area = TRUE, position = 'identity', facet = 'PSCR', ...) 
}

graphDaa_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
  graphit(lpdf, countries, xvar = 'theDate', 
     yvars = c( 'active_imputed', 'active'), facet = 'PSCR', 
     logy = logy, ... ) #putLegend = TRUE
}

graphDrr_fia <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = "theDate", 
         yvars = c('recovered_imputed', 'recovered'), 
         area = TRUE, position = 'identity', facet = 'PSCR', ...) 
}

myGraphList <- ls(pattern  = "graph")
myGraphNrs <- ls(pattern  = "graph[[:digit:]]")
myGraphListbyDay <- ls(pattern  = 'graphd') 
myGraphListbyDate <- ls(pattern  = 'graphD')


reportDiffTime <- function(myMessage, startTime, units = 'auto', precision  = 2){
 dt <- round( difftime( Sys.time(), startTime, units = units), precision)
 message(myMessage % % dt % %  ifelse(units == 'auto','units',units))
 dt
}

timer <- function(mycall, myMessage  = 'duration', verbosity  = 1, ...){
 if (verbose > verbosity) {
  tistart  = Sys.time(); message(format(Sys.time(), "%H:%M:%S" )  % %  myMessage)}
 res <- do.call(myCall, ...)
 if (verbose >=  verbosity) {reportDiffTime(myCall %:% myMessage, tistart)}
 res
}

graphOnRegion <- function(lpdf, myRegion, myGraph, saveit = TRUE, ...) {
 if (verbose >= 7) message( 'territories' % %    paste(myRegion, collapse = "/ "))
 if (verbose >= 5) { tir <- Sys.time() ; message(tir %: % myGraph  % %  myRegion[1] ) }
 do.call(myGraph, 
      args = list(lpdf, myRegion, savename  = ifelse(saveit == TRUE, myRegion[1], 
                                              ifelse(saveit == FALSE, "", saveit))
                                , ...))
 if (verbose >= 6) {
  reportDiffTime(myGraph  % %  myRegion[1] % %  "duration:", tir)}

}

walkThrough <- function(lpdf = ECDC, regions= ECDC.Regios, graphlist=myGraphNrs , 
                        saveit = TRUE, ordre = 'RG', ...){
  theLog = file("the.log", open = "wt")
  sink(theLog, type = "message")
  
  on.exit({
    message("Sink.number = ", 
            sink.number( type = "message")
            )
    sink( type = "message") 
    if (isOpen(theLog)) close(theLog)
    }, add = T )
  
  misreg <- missing(regions)
  if (!misreg)  {
    message(length(regions) % % "regions and" % % length(graphlist) % % "graphs." % %
    "at 4.16 seconds per graph/region, this would last" % % 
      round(length(regions)* length(graphlist)/60*4.16, 2) % % "minutes") 
    # "results of 2020-09-18"   # "ECDC graphs 15.83 mins"    # "JHH graphs 32.1 mins"
    if (typeof(regions)  == "character") { regions = list(regions) }
  } 
  if (ordre == 'RG') {
    walk(graphlist, function(myGraph){
      if (misreg) {
        byVar = switch(EXPR = 1 + grepl("n", myGraph, fixed=TRUE), "active_imputed",  "net_active_imputed") %#% switch(EXPR = 1 + grepl("M", myGraph, fixed=TRUE),  "", "_p_M") 
        regions <- makeDynRegions(lpdf, byVar = byVar , piecename = lpdf % % "World")     
        }
      if (verbose >= 4) {tig = Sys.time(); message(format(Sys.time(), "%H:%M:%S " ) % % myGraph)}
      walk(regions, function(myRegion)  graphOnRegion(lpdf = lpdf, myRegion, myGraph, saveit = saveit, ...) )
      if (verbose >= 5) {reportDiffTime(myGraph, tig)}
    })} else {
        if (verbose >= 4) message("doing all graphs for a region. Pages are defined by decreasing active_imputed. If you want to see decreasing net_active_imputed where relevant, iterate over regions first, then graphs")
    walk(regions, function(myRegion){
      if (verbose >= 4) {tig = Sys.time(); message(format(Sys.time(), "%H:%M:%S " ) % % myRegion[1])}
      walk(graphlist, function(myGraph) graphOnRegion(lpdf  = lpdf, myRegion, myGraph, saveit = saveit, ...) )
      if (verbose >= 5) {reportDiffTime(myRegion[1], tig)}
    })}
}


makeDate <- function(chardate = "", format  = myDateFormat){
 tryCatch(as.Date(chardate, format = format), 
      error = function(e){message("Either enter a date or a string (please use the following Date format for the string:" % % myDateFormat )})
}

makeHistoryGraphs <- function(lpdf, regions, graphlist = myGraphNrs, 
                              fromDates, toDates, saveit = TRUE, myFolderDate = from % % 'to' % % to, ...) {
  on.exit({options(warn = 0) }) 
  if (missing(regions) ) stop("no regions to graph")
  if (missing(fromDates) ) {
    if (missing(toDates)) {
        fromDates = min(lpdf$theDate)
        toDates  = max(lpdf$theDate)
    } else {fromDates <- as.character(rep(min(lpdf$theDate), length(toDates)))} 
    } else {if (missing(toDates)) {
      toDates <- as.character(as.Date(fromDates[2:length(fromDates)]) - 1)
      fromDates <- fromDates[1:(length(fromDates) - 1)]
    }}
  walk2(fromDates, toDates, function(from, to){
    if (verbose >= 1) {
      ti_da = Sys.time() 
      message(format(ti_da, "%H:%M:%S ")  % %  "doing"  % %  as.Date(from, origin = "1970-01-01") % % as.Date(to, origin = "1970-01-01"))
    }
    if (nrow(lpdf[lpdf$theDate >= from & lpdf$theDate <= to, ]) > 0) {
      walkThrough(lpdf, regions, graphlist, saveit = saveit, from  = from, to  = to, 
                  myFolderDate  = myFolderDate, ...)
    }
    else message("no data from " % %  from  % % 'to'  % % to )
    if (verbose >= 2) {
      reportDiffTime( as.Date(to, origin = "1970-01-01")  % %  "duration: ", ti_da)}
    while (!is.null(dev.list())) dev.off() 
  })
}

makeHistoryOneGraph <- function(lpdf, regions, graph = 'graph1aa_finl', 
                              fromDates, toDates, ...) {
    makeHistoryGraphs(lpdf, regions, graph, saveit =  from  % % 'to'  % % to, from  = from, to  = to, 
                  myFolderDate  = graph, ...)
    
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
 lpdf <- lpdf[lpdf$theDate > "2020-01-22", c("theDate", var1, var2)]  #dubious way of excluding potential missing daily growth. 
    #ECDC starts before but has no recovery variable, so we could only use it for DC lags.
 if (all(is.na(lpdf[, var1])) || all(is.na(lpdf[, var2]))) 
  return(data.frame( cor  = NA, lag  = NA)) #
 d <- ccf.vf(lpdf[, var1], lpdf[, var2], lag.max = 30, plotit = FALSE)
 if (verbose >= 2) message("myPSCR" % % myPSCR)
 res  = data.frame( cor = d$acf[, , 1], lag = d$lag[, , 1])
 if (N %% 2  == 0) N = N - 1
 a <- res[order(res$cor, decreasing = TRUE)[1:N], ]
 if (verbose >= 5) message("regions by decreasing cor", paste(a, collapse = ""))
 res_max  = median( a$lag) #which.max(res$cor) #instead of max, take the n largest: order(R, decreasing = TRUE)[1:N]
 return(res[res$lag  == res_max, ]) 
} 

findMaxCCFs_deprecated <- function(var1 = "new_recovered", var2 = "new_confirmed", myPSCR = "", lpti = JHH) {
 a <- ddply( lpti, "PSCR", function(lptip){findMaxCCF(var1 = var1, var2 = var2, myPSCR = myPSCR, lpti = lptip)})
 a[!is.na(a$lag), ]
}
findMaxCCFs <- function(var1 = "new_recovered", var2 = "new_confirmed", myPSCR = "", lpti = JHH) {
  a <- lpti %>% group_by(PSCR) %>% do( findMaxCCF(var1 = var1, var2 = var2, myPSCR = myPSCR, lpti = .))
  a[!is.na(a$lag), ]
}

#end.
# execution and control is in Output.Rmd/ipynb and Graphs.Rmd/ipynb
