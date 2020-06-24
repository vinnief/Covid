source("requirements.R")
#Global assumptions
LAGRC <- 42
LAGRD <- 36
LAGDC <- LAGRC-LAGRD
deathRate = .05
if(!exists("verbose")) verbose <- 1

myDateFormat <- "%Y-%m-%d"
myPlotPath <- "G:/My Drive/Covid19_plots"
myPath <- myPlotPath 
if (!dir.exists(myPlotPath)) dir.create(myPlotPath, recursive = TRUE)
datapath = './data'
if (!dir.exists(datapath)) dir.create(datapath, recursive = TRUE)

#data loading
#*Note*: the data of John hopkins, 
#git@github.com:CSSEGISandData/COVID-19.git
#before 20200325:#c <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
#after 20200326:
#c <- read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_global.csv'

#for the USA: 
 #https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_US.csv
#RAW: 
#https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_Covid19_confirmed_US.csv
#US headers: UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key, 1/22/20, 
readUSdata <- function(dataversion = "confirmed"){#deaths and recovered are the other options. 
 filename <- paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_', dataversion, "_US.csv", sep = "")
 tryCatch( wpdf <- read.csv(filename) , 
      error = function(e) print(paste(e, " The data was not found: Are you sure this file exists? ", filename))
 )
 return(wpdf)
}
readTesting <- function(){
 testing <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv')%>% 
  select(c('Entity', 'ISO code', 'Cumulative total', 'Daily change in cumulative total')) %>% 
  mutate(tests  = `Cumulative total`, 
      new_tests = `Daily change in cumulative total`, 
      ISOcode  = `ISO code`)%>%  
  select(-'Cumulative total', -'Daily change in cumulative total')
 coco <- as.data.frame(str_split_fixed(testing$Entity, ' - ', n = 2))
 testing$PSCR <- coco[, 1]
 testing$comment <- coco[.2]
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
 #lpdf <- wpdf %>% pivot_longer(????)
 lpdf$Date <- as.Date(paste(lpdf[, coltype], "20", sep = ""), format = "X%m.%d.%Y") #add century, get rid of X
 lpdf$date <- NULL
 return(lpdf)
} 

findIDnames <- function(lpdf = JHH, testIDnames = c("Neth", "India"), searchID = "PSCR", 
             fuzzy = TRUE, returnID){
 lpdf <- as.data.frame(lpdf)
 allIDs <- (unique(lpdf[, searchID]))  #error maybe? [ for dataframe 
 if (!fuzzy) {a1 <- intersect(testIDnames, allIDs)
 }else a1 <- allIDs[unlist(llply(testIDnames, function(a) grep(a, allIDs, ignore.case = TRUE)))]
 if (missing(returnID)) return ( a1) #returnID = searchID
 else if(searchID  == returnID) {
  if (verbose >= 4) print('no need for returnID if same as searchID')
  return ( a1)} #returnID = searchID
 unique(lpdf[lpdf[, searchID] %in% a1, returnID])
} 
aggreg <- function(avector){
 len <- length(unique(avector))
 if(len  == 1){avector[1]
 }else #if (len  == 2) {paste(avector, collapse = "_") }else
  paste(avector[1], len-2, avector[length(avector)], sep = "_")
}


total <- function(lpdf = JHH, rows = "", 
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
   if("population"%in% names(df)) ans$population <- sum(df$population)
   if("imputed"%in% names(df)) ans$imputed <- any(df$imputed)
   if("Region" %in% names(lpdf)) 
    ans$Region <- ifelse(newrow !=  "", newrow, aggreg(df$Region))
   if ("County" %in% names(df)) 
    County = ifelse(newrow !=  "", newrow,  as.character(aggreg(df$County)))
   ans
   }) 
 ans[, setdiff(names(lpdf), names(ans))] <- NA
 ans
}
#c("Canada", "US", "Australia", "China")
totals <- function(lpdf = JHH, rows = "", 
         ID = "Country.Region", 
         varnames = c("confirmed", "deaths", "recovered")
         ){
 if (rows[1]  == "") rows = as.list(unique(lpdf[[ID]]))
 if (verbose >= 3) print(paste("Making the total for ", paste(rows, collapse = "/ "), "in", ID))
 ans <- ldply(rows, function(a) lpdf%>% total(a, ID, varnames, a))
}
       
totals2 <- function(lpdf, rows = "", # used only for county to state totalling. 
         ID = "Country.Region", 
         varnames = c("confirmed", "deaths")
           # "recovered"?
         ){
 if (rows[1]  == "") rows = unique(lpdf[, ID])
 if (verbose>5) print(paste("Making totals2 for ", paste(rows, collapse = ", "), "in", ID))
 ans <- ddply(lpdf[lpdf[, ID] %in% rows, ], c("Date", ID), 
       function(a) {
     Country.Region = aggreg(as.character(a$Country.Region))
     Province.State = aggreg(a$Province.State)
     PSCR = ifelse(Province.State  == "", Country.Region, makePSCR(Province.State, Country.Region))
     b1 <- colSums(a[, varnames], na.rm = TRUE) #this creates 0 for recovered in the US if included. 
     nam <- names(b1)
     dim(b1) <- c(1, length(b1))
     b2 <- data.frame(b1)
     colnames(b2) <- nam
     cbind(Country.Region, 
        PSCR, 
        Province.State, 
        Lat = mean(a$Lat) , Long = mean(a$Long), 
        b2
        ) }) 
 ans[, setdiff(names(lpdf), names(ans))] <- NA
 ans
}

correctnames <- function(df){
 names(df)[match("Long_", names(df))] <- "Long"
 names(df)[match("Province_State", names(df))] <- "Province.State"
 names(df)[match("Country_Region", names(df))] <- "Country.Region"
 df[, !names(df) %in% c("Admin2" , "UID", "iso2", "iso3", "code3", "FIPS")]
}
makelpdfUSStates <- function(){
 wc <- readUSdata('confirmed') 
 geo.location <- wc[, c("Combined_Key", "Country_Region", "Province_State", "Admin2", "UID", "Lat", "Long_")]
 wc <- correctnames(wc)
 #write.csv( geo.location, file = datapath %#% "/" %#% "geo.location.US.csv", na = "")
 rm(geo.location)
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
 lpdf <- lpdf %>% totals2("", "Province.State") # depends on all provinces being chosen to sum! 
 lpdf$Combined_Key <- NULL
 if(!all(is.na(lpdf$recovered)))
  if( max(lpdf$recovered, na.rm = TRUE) <= 0) lpdf$recovered <- as.numeric(NA ) #just in case NA totalled into 0. 
 lpdf 
}

makelpdf <- function() {
 wc <- readdata('confirmed') #"Confirmed")
 geo.location <- wc[c("Country.Region", "Province.State", "Lat", "Long")]
 #write.csv( geo.location, file = datapath %#% "/" %#% "geo.location.csv", na = "")
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
 #CUS <- read.csv(datapath %#% "/" %#% nameUS, stringsAsFactors = FALSE)#colClasses = ("Date" = "character"))
 CUS2 <- read_csv(nameUS)
 #Cworld <- read.csv(datapath %#% "/" %#% namenonUS, stringsAsFactors = FALSE)
 Cworld2 <- read_csv(namenonUS)
 #lpdf <- rbind(Cworld, CUS)
 lpdf2 <- rbind(Cworld2, CUS2)
 #as_tibble(lpdf)
}

sortIDlevels1 <- function(lpdf, varname = confirmed, ondate = ""){
 varname <- enquo(varname)
 if (ondate  == ""){ondate = max(lpdf$Date) } else 
  if (nrow(lpdf[lpdf$Date  ==  ondate, ]  == 0)){
   stop("Cannot sort on values of a date which is not present in the Data")}
 PSCRlevels <- lpdf %>% select(c(PSCR, Date, !!varname)) %>% 
  filter(Date  == ondate) %>% 
  arrange(-eval(parse(text = substitute(!!varname))), PSCR, .by_group  = FALSE) 
  arrange(desc({{varname}}), PSCR, .by_group  = FALSE) 
  #arrange(-(!!varname), PSCR, .by_group  = FALSE) 
 lpdf <- lpdf%>%ungroup%>% mutate(PSCR = factor(lpdf$PSCR, levels = PSCRlevels$PSCR))%>%
  group_by(PSCR)
 lpdf$PSCR
} #the desc eval parse substitute !! should have been desc !! according to the manuals. but desc does not respect unquo. 

#lpdf = JHH

sortIDlevels <- function(lpdf, varname  = "confirmed", ID  = "PSCR", ondate){
 if (missing(ondate)) { 
  theDateData <- lpdf[, c(varname, ID, 'Date')] %>% group_by(PSCR) %>% 
   filter(Date  ==  max(Date)) %>% ungroup
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


sortbyvar <- function(lpti, varname = 'confirmed', ID = 'PSCR', ondate = ""){
lpti[[ID]] <- lpti%>% sortIDlevels(varname = varname, ID = ID, ondate = ondate) 
lpti <- lpti[order(lpti[[ID]], lpti[[varname]]), ] #bUG? WHY SORT AGAIN BY same var? 
}

makeJHH <- function(name = "JHH", force = FALSE) {
 nameUS <- paste( paste(name, "US", sep = "_"),      "csv", sep = ".")
 namenonUS <- paste( paste(name, "non", "US", sep = "_"),  "csv", sep = ".")
 namedays <- paste(name, "_days.csv", sep = "")
 if(force|(difftime(Sys.time(), file.info(namedays)[, "mtime"], units  = "hours")>6)) {
  lpdf <- updateJHHFromWeb(nameUS, namenonUS)
  if (verbose >= 1) print("updating JHH from Github")
 } else {
  if (verbose >= 1) print(paste("loading local", namedays))
  lpdf <- read.csv(datapath %#% "/" %#% namedays, stringsAsFactors  = FALSE)
 }
 if (typeof(lpdf$Date)  == "character") 
  lpdf$Date <- as.Date(lpdf$Date, "%Y-%m-%d") #strptime gives timezones! no need for timezones
 if (verbose>0) {a = as.numeric(max(lpdf$Date)-min(lpdf$Date)+1)
  print(a % % "dates"%, % (nrow(lpdf)/a) % % "regions, last date:" % %  
      max(lpdf$Date)%, % "with"  % % 
      sum(is.na(lpdf[lpdf$Date >= "2020-02-02", ]))  % % 
      "missing values after 2020-02-01")}
 lpdf
}


######## make state groups, also useful in tableau

regios <- list(EFTA = c("EFTA", "Iceland", "Liechtenstein", "Switzerland", "Norway"), 
    Benelux = c("Benelux", "Belgium", "Netherlands", "Luxembourg"), 
    US = c("USA"), #"US", 
    SouthWestAsia = c("South West Asia", "Afganistan", "Iran", "Irak", "Syria", "Lebanon", "Turkey", "Israel", "West Bank and Gaza", "Palestine"), 
    SouthEastAsia = c("South East Asia", "Indonesia", "Thailand", "Vietnam", "Laos", "Malaysia", "Cambodia", "Papua New Guinea", "Myanmar", "Burma", "Brunei", "Philippines", "Timor-Leste"), 
    SAsiaIO = c ("South Asia & Indian Ocean", "India", "Pakistan", "Bangladesh", "Sri Lanka", "Comoros", "Maldives", "Madagascar", "Mauritius", "Seychelles", "Bhutan", "Nepal", "Mayotte", "Reunion"), 
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
     Caribbean = c("Caribbean", 'Anguilla', "Antigua and Barbuda", "Bahamas" ,  "Barbados", "Bermuda", "Cayman Islands", "Cuba", "Dominica" , "Dominican Republic", "Grenada", "Haiti" , "Jamaica", "Saint Kitts and Nevis" , "Saint Vincent and the Grenadines", "Saint Lucia" , "Trinidad and Tobago", 'Aruba', 'Curacao', "Bonaire, Sint Eustatius and Saba", "British Virgin Islands", 'Guadeloupe', 'Martinique', 'Sint Maarten', 'St Martin', 'Saint Barthelemy', 'Turks and Caicos Islands', 'Montserrat')), 
     regios)

regios = c(list(
 other = c("Other", 'Diamond Princess', 'MS Zaandam', 'World'), 
 MSM  = c("MSM", "Netherlands", "Belgium", "United Kingdom", "Germany", "Malta", "Egypt", "Suriname", "China", "Vietnam", "Hungary", "Romania", "Kuwait", "Italy", "Ireland", "Iran", "Kazakstan", "Liberia", "Indonesia", "Ethiopia", "Nigeria", "Ghana", "Uganda", "South Africa", "Canada", "Spain", "France"), 
 Vincent = c("Some Selected Regions", "Belgium", "Germany", "Italy", "France", "Kazakhstan", "Indonesia", "Spain", "Netherlands", "Japan", "New York"), 
 continents = c("Continents", "Europe", 'North America', "Africa", "South America", "Asia"), #"USA", "US", 
 WestvsEast = c("WestvsEast", "USA", "United Kingdom", "Italy", "Iran", "Korea, South", "Germany", "France", "Spain", "Sweden", "Norway", "Belgium", "Netherlands", "Singapore", "Japan", "Taiwan*", "Denmark", "Hubei, China", "Hongkong, China", "Jiangsu, China", 'Indonesia'), 
 MENA = c("MENA", "Marocco", "Algeria", "Tunesia", "Libia", "Egypt", "West Bank and Gaza", "Palestine", "Lebanon", "Syria", "Turkey", "Iraq", "Iran", "Afghanistan", "Jordan", "Saudi Arabia", "Kuwait", "Oman", "United Arab Emirates", "UAE", "Yemen", "Bahrain", "Qatar"), 
 SAmerica = c("South America", "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Suriname", "French Guiana", "Venezuela", "Paraguay", "Peru" , "Uruguay", "Falkland Islands (Malvinas)"), 
 Europe = c("Europe", regios$EU[2:28], regios$EFTA[2:5], "United Kingdom", "Russia", "Ukraine", "Belarus", "Moldova", "Georgia", "Armenia", "Azerbaijan", "Andorra", "Monaco", "San Marino", "Vatican", "Holy See", "Albania", "North Macedonia", "Kosovo", "Croatia", "Montenegro", "Bosnia and Herzegovina", "Serbia", "Gibraltar", "Faroe Islands", "Isle of Man", "Channel Islands", "Greenland"), 
 NAmerica = c("North America", "USA", "Canada", "Mexico", "Saint Pierre and Miquelon", "Antilles", "Belize", "Guatemala", "Nicaragua", "Costa Rica", "Honduras", "El Salvador", "Panama", regios$Caribbean), 
 Africa = c("Africa", "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo (Kinshasa)", "Congo (Brazzaville)", "Cote d'Ivoire", "Djibouti", "Egypt",  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara", "Zambia", "Zimbabwe"), 
  Oceania = c("Oceania", "Australia", "New Zealand", "Vanuatu", "Tuvalu", "Fiji", "Guam", "French Polynesia", "New Caledonia" )
      ), 
     regios)

### data from ECDC - World bank. 
### https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
makeECDC <- function(){
 lpti <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na  = "" ) %>% #fileEncoding  = "UTF-8-BOM" doesn use bom in readr tidyverse. 
  mutate( PSCR = countriesAndTerritories, 
      ISOcode  = countryterritoryCode, 
      confirmed_today = cases, 
      deaths_today = deaths, 
      Date  = as.Date(dateRep, format = "%d/%m/%Y"), 
      population  = popData2019, 
      Region = continentExp) %>%
  select(-popData2019, geoId, -day, -month, -year, -cases , -countriesAndTerritories, 
      -dateRep, -continentExp, -countryterritoryCode) %>% 
  arrange(PSCR, Date) %>% group_by(PSCR) %>% 
  mutate(confirmed  = cumsum(confirmed_today), 
      deaths  = cumsum(deaths_today), 
      recovered = as.numeric(NA)) %>%
  select(-confirmed_today, -deaths_today)
 if (verbose > 0) {a = as.numeric(max(lpti$Date) - min(lpti$Date) + 1)
  print(a % % "dates" %, % length(unique(lpti$PSCR)) % % "regions, last date:" % %  
     max(lpti$Date) %, % "with"  % % 
     sum(is.na(lpti[lpti$Date >= "2020-02-02", ]))  % % 
     "missing values after 2020-02-01")}
 lpti
}

addRegions <- function(lpdf = JHH, varname = "Region", Regiolist = "") { 
 if(!varname %in% names(lpdf)) lpdf[[varname]] <- as.character(NA)
 for (Regio in Regiolist) {
  region = Regio[1] %>% strsplit(" countries") %>% unlist %>% strsplit(" Provinces&States")%>% unlist
  lpdf[lpdf$Province.State %in% Regio, varname] <- region
  lpdf[lpdf$PSCR %in% Regio, varname] <- region
  }
 if (verbose >=  1){
  print("Regions added:" % %  length(unique(JHH$Region)) )#paste(unique(JHH$Region), collapse = ", "))
  if(sum(is.na(lpdf$Region))>0){
   print(paste("Not attributed regions:")  % % 
   paste(unique(lpdf[is.na(lpdf$Region), ]$PSCR), collapse = "; "))
  }
 }
 lpdf
}

makeDynRegions <- function(lpti = JHH, gridsize = 5*6, piecename = 'World', ratio = 5) {
 lpti <- lpti%>% group_by(PSCR) %>% 
  filter( Date  == max(Date))%>% ungroup %>%
  select( PSCR, confirmed)%>% 
  arrange(desc(confirmed)) 
 nr = 1
 mylist = vector(mode  = "list", length  = 0)
 while (nrow(lpti)>gridsize){
  minneeded <- lpti$confirmed[1]/ratio
  piece <- c(piecename %#% nr, as.character(head(lpti[lpti$confirmed >= minneeded, ]$PSCR , gridsize) ))
  mylist[[piece[1]]] <- piece
  nextone = length(piece)-1
  if (nextone <= 1) nextone = 2
  lpti <- lpti%>% filter(row_number() >= nextone) #gridsize+1 | confirmed< minneeded)
  #nrow(lpti)
  nr <- nr+1
  }
 piece <- c(piecename %#% nr, as.character(lpti[1:nrow(lpti), ]$PSCR))
 mylist[[piece[1]]] = piece
 mylist
}

provincialize <- function(countries, lpdf = JHH){
 cl1 <- unique(lpdf[lpdf$Country.Region %in% countries, ]$PSCR)
 c(countries[1] % % "Provinces&States", cl1)#, setdiff(cl1, countries))
}

provincializeJHH <- function(){
 lpdf = JHH
 ChinaP <- provincialize(regios$China, lpdf)
 list(CanadaP = provincialize("Canada", lpdf), 
    USS = provincialize(regios$US, lpdf), 
    NorthAmericaS = setdiff(provincialize(c("US", regios$NAmerica), lpdf), "USA"), 
    OceaniaP = (c(provincialize(regios$Oceania, lpdf), regios$Oceania)), #,             "Australia"), 
    AsiaP = c(regios$Asia, (setdiff(ChinaP, "China Provinces&States"))), 
    ChinaP = ChinaP
 )
}

makeRegioList <- function(lpti = JHH, piecename = "JHH"){
 Oceania = piecename  % %  'Oceania'
 Samerica  = piecename  % %  'South America'
 regios  = c(  
  lpti%>% makeDynRegions(piecename = piecename % % 'World'), 
  lpti%>% filter(PSCR %in% regios$Europe )%>% 
   makeDynRegions(gridsize = 20, piecename = piecename % % 'Europe'), 
  lpti%>% filter(PSCR %in% c(regios$AsiaP)) %>% makeDynRegions(piecename = piecename % % 'Asia'), 
  lpti%>% filter(PSCR %in% regios$NorthAmericaS) %>% makeDynRegions(piecename = piecename  % %  'North America'), 
  lpti%>% filter(PSCR %in% regios$Africa )   %>% makeDynRegions(piecename  = piecename  % %  'Africa') , 
  list(SAmerica  = regios$SAmerica, 
    Oceania  = regios$OceaniaP
  ) #World = "World", #regios['continents'], 'WestvsEast', 'Caribbean', 
 ) 
}

addPopulation <- function(lpdf) {
 population <- read.csv(datapath %#% "/" %#% 'population.csv')[c(1, 3)]
 names(population)[2] <- "population"
 rownames(population) <- population$Country.Name
 lpdf[, "population"] <- population[lpdf%>% pull(PSCR), "population"]
 popUS <- read.csv(datapath %#% "/" %#% 'USstatespop2019.csv')[c('State', 'p2019')]
 names(popUS)[2] <- "population"
 rownames(popUS) <- popUS$State
 lpdf[grepl(", US", lpdf$PSCR), "population"] <- 
  popUS[lpdf[grep(", US", lpdf$PSCR), ]$Province.State, "population"]
 popunknown <- unique(lpdf[is.na(lpdf$population), ]$PSCR)
 if (verbose >= 2 | length(popunknown) > 0)print("population unknown:"  % %  
      ifelse(length(popunknown)  ==  0, 0, paste(popunknown, collapse = "; ")))
 lpdf
}

mutate_cond <- function(.data, condition, ..., envir  = parent.frame()) {
 condition <- eval(substitute(condition), .data, envir)
 .data[condition, ] <- .data[condition, ] %>% mutate(...)
 .data
}


imputeRecovered <- function(lpdf = ECDCdata, lagrc = LAGRC, lagrd = LAGRD, # was 22, 16
              dothese = FALSE, correct = FALSE){
 varname = "recovered"
 if(!('recovered' %in% names(lpdf))) lpdf$recovered <- as.numeric(NA)
 #if(any(dothese)) lpdf[, varname %#% "_old"] <- lpdf[, varname]
 if (!"imputed"%in% names(lpdf))lpdf$imputed <- FALSE
 lpdf <- lpdf%>% group_by(PSCR) %>%
  mutate(recovered_imputed  = 
       pmin(confirmed-deaths, 
          pmax(0, recovered, 
            lag(confirmed, lagrc)- lag(deaths, lagrd), 
            na.rm = TRUE), 
          na.rm = FALSE)
           ) 
 rowstobecorrected = correct & ( lpdf$recovered < lpdf$recovered_imputed )
 rowstodo <- is.na(lpdf$recovered)|dothese| rowstobecorrected
 # %>% drop() #no need: is vector already
 if (verbose >= 3)print("imputing recovered for:" % % 
            length(unique(lpdf[rowstodo, ][["PSCR"]]))
             % %  'Regions.')
 if (verbose >= 5) {print('imputing regions:' );print(paste(unique(lpdf[rowstodo, ][["PSCR"]]), collapse = "; "))}
 if (sum(rowstodo)  == 0) return(lpdf)
 lpdf <- lpdf%>% group_by(PSCR) %>%
  mutate_cond(rowstodo, imputed = TRUE )#%>%   mutate_cond(rowstodo, recovered = recovered_imputed)
}

frac <- function(n, d){ifelse(d !=  0, n/d, NA)}

diff.sl <- function(avector, n = 1){c(rep(NA, n), diff(avector, n))}
p_M <- function(a, b)1e6*a/b

extravars <- function(lpdf, lagrc = 0, lagdc = 0){
 tempwarn <- getOption("warn")
 options(warn = -1)
 on.exit(options(warn = tempwarn))
 lpdf <- lpdf %>% ungroup %>% 
  arrange(PSCR, Date) %>%
  group_by(PSCR) %>% 
  mutate( active      =  confirmed - deaths - recovered, 
      active_imputed   =  confirmed - deaths - recovered_imputed, 
      new_confirmed    =  mac(diff.sl(confirmed)), 
      net_active     =  mac(diff.sl(active)), 
      net_active_imputed =  mac(diff.sl(active_imputed)), 
      new_recovered    =  mac(diff.sl(recovered)), 
      new_recovered_imputed = mac(diff.sl(recovered_imputed)), 
      new_deaths     =  mac(diff.sl(deaths)), 
      confirmed_p_M    = p_M(confirmed, population), 
      active_p_M   = p_M(active , population), 
      active_imputed_p_M      = p_M(active_imputed , population), 
      recovered_p_M    = p_M(recovered, population), 
      recovered_imputed_p_M    = p_M(recovered_imputed, population), 
      deaths_p_M      = p_M(deaths  , population), 
      new_confirmed_p_M  = p_M(new_confirmed, population), 
      net_active_p_M   = p_M(net_active  , population), 
      new_recovered_p_M  = p_M(new_recovered, population), 
      net_active_imputed_p_M   = p_M(net_active_imputed  , population), 
      new_recovered_imputed_p_M  = p_M(new_recovered_imputed, population), 
      new_deaths_p_M   = p_M(new_deaths , population), 
      recovered_per_confirmed = frac(recovered, dplyr::lag(confirmed, lagrc)), 
      recovered_imputed_per_confirmed = 
       frac(recovered_imputed, dplyr::lag(confirmed, lagrc)), 
      deaths_per_confirmed = frac(deaths, dplyr::lag(confirmed, lagdc)), 
      recovered_per_deaths = frac(recovered, dplyr::lag(deaths, lagrc-lagdc)), 
      recovered_imputed_per_deaths = 
       frac(recovered_imputed, dplyr::lag(deaths, lagrc-lagdc)), 
      new_active_rate   =  round(new_confirmed/active_imputed, 3) )
}

doublingLine <- function(lpti  = JHH, country, minVal, growthRate, 
            doublingDays  = 5, nrRows  = 100, pop, 
            myDeathRate  = deathRate, lagrc  = LAGRC, lagdc  = LAGDC ){
 if (!missing(country)) {
  if (missing(minVal)) minVal <- lpti$confirmed[1]
  lpti <- lpti %>% filter(PSCR %in% country, confirmed >=  minVal)
  minVal <- lpti$confirmed[1]
  if (missing(pop)) pop <- lpti$population[1]
  if (missing(doublingDays)) 
   if (missing(growthRate)) {
    doublingDays  = lpti$confirmed_doublingDays[1]
    if (verbose >=  5) print('doublingline:1 doublindDays = '  % %  doublingDays)
    growthRate  = 2^(1/doublingDays)}
  else doublingDays <- -log2(growthRate)
  if (NROW(lpti)  ==  0) stop("No country '" %#% country %#% "'in the data")
 }
 if (!missing(nrRows)) maxDate <- max(lpti$Date) + nrRows
 else {maxDate <- max(lpti$Date)}
 if (verbose >=  5) print('doublingline: doublindDays = '  % %  doublingDays)
 if (missing(growthRate)) {growthRate  = 2^(1/doublingDays)}
 nrRows <- maxDate-min(lpti$Date)+1
 if (maxDate  ==  -Inf) stop("Max Date equals -inf. Probably we have an empty data set. Did you choose the right country? ")
 if (verbose >=  3) print('dL:'  % %  'double:'  % %  "R0 = "  % %  round(2^(lagrc/doublingDays)-1, 2) %, % 'Simulated'  % %  nrRows % % 'days until'  % %  maxDate)
 out = tibble(Date = seq(from = min(lpti$Date), to = maxDate, by = 1))
 doubling <- round(minVal*2^((0:(nrow(out)-1))/(doublingDays) ))
 out <- out %>% mutate ( growthRate, doublingDays, 
            confirmed  = pmin(doubling, pop), 
            deaths   = round(myDeathRate*lag(confirmed, lagdc, default = 0)), 
            recovered  = lag(confirmed, lagrc, default = 0), 
            active   = confirmed - deaths - recovered, 
            population  = pop - deaths
 ) 
 out
}
growOnce <- function(lpti, rownr, minVal, doublingDays, growthRate, nrRows, myDeathRate = deathRate, pop, lagrc  = LAGRC, lagdc  = LAGDC){#no need for minVal, doublingDays, nrRows but makes passing arguments easier
 prevrow <- lpti[rownr - 1, ]
 currow <- prevrow %>% #we randomize because this overcomes low rates& small minVals
  mutate(Date  = Date + 1, 
      confirmed  = confirmed +
       round(rpois(1, pmax(0, active*(growthRate - 1) *
                 (population-recovered-active)/population))))
 if(rownr <=  lagdc) currow$deaths <- 0 
  else currow$deaths <- round(myDeathRate*lpti[rownr-lagdc, ]$confirmed) 
 currow$recovered <- 
     ifelse(rownr <= lagrc, 0, round(lpti[rownr-lagrc, ]$confirmed*(1-myDeathRate)))
 currow <-  currow%>% mutate (population = pop-deaths, 
               active = confirmed-deaths-recovered
               )
 currow[setdiff(names(lpti), names(currow))] <- NA
 currow
}

simulGrow <- function(lpti, country, ...){
 out <- doublingLine(lpti = lpti, country = country, ...) 
 if (verbose >=  4) {print('doublingline made' );print(out)} #to compare with growonce results. 
 nrRows <- NROW(out)
 pop  = out$population[1]
 if (nrRows<2) return(out)
 
 growthRate <- out$growthRate[1] #confirmed_
 for(rownr in 2:nrRows) { # one could try pmap here!
  out[rownr, ] <- growOnce(out, rownr = rownr, growthRate = growthRate, pop = pop, ...) 
 }
 out
}

addSimCountry <- function(lpti, country, ...){ 
 out <- simulGrow(lpti = lpti, country = country, ...)%>%
     imputeRecovered %>% extravars
 if(missing(country)){
  out <- out%>% mutate(
   Country.Region = doublingDays%_%'days', 
   PSCR = doublingDays  % %  'days', 
   Province.State  =  doublingDays  % %  'days', 
   Region  ==  doublingDays  % %  'days', 
   Lat = 90, Long = 90)
 } else {
  doublingDays = out$doublingDays[1] #confirmed_ #bug. what are the names of vars in out? 
  out <- out%>% mutate(
   Country.Region = country  % % doublingDays%_%'days', 
   PSCR = country  % % doublingDays  % %  'days', 
   Province.State  = country  % %  doublingDays  % %  'days', 
   Region  ==  lpti2$Region[1]  % % doublingDays  % %  'days', 
   Lat <- lpti2$Lat[1], 
   Long <- lpti2$Long[1] )
 }
 missingCols <- setdiff(names(lpti), names(out))
 if(verbose >= 5 &length(missingCols>0)) 
  print(' addsimcountry:'  % % missingCols % %  'filled with NAs')
 out[missingCols] <- NA
 lpti <- rbind (lpti, out ) # bug: need to adjust the names of simulated variables in out.
 lpti
}

# from R0 to doubling days: not correct. 42 should give 1. 
R02doublingDays <- function(R0 = 1){
 log2(LAGRC/R0)
}


estimateDoublingDaysOneCountry <- function(lpti, variable = 'confirmed', nrDays = 9, minDate = "2019-12-31", maxDate = '2020-12-31'){
 getGR <- function(rowNr){
  lptiSel <- lpti[(rowNr-nrDays+1):rowNr, ] #potential Bug: assumes data sorted by increasing Date! is it? should be!
  if (sum(!is.na(lptiSel[[variable]])) >= 3 ) {
   #https://win-vector.com/2018/09/01/r-tip-how-to-pass-a-formula-to-lm/
   f <- as.formula('log2(' %#% variable %#% ')~Date')
   growthRate <- tryCatch( 
     lm( f, data  = lptiSel, na.action  = na.exclude )$coefficients[['Date']], 
          finally  = NA)}
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
 if (!(variable%_%'doublingDays' %in% names(lpti))) 
  lpti[[variable%_%'doublingDays']] <- as.numeric(NA)
 if(!(variable %_% 'growthRate' %in% names(lpti))) 
  lpti[[variable %_% 'growthRate']] <- as.numeric(NA)
 lpti %>% group_by(PSCR) %>% 
  (function(lpti){
   lpti[lpti[[variable]]>0 , c(variable%_%'doublingDays', variable%_%'growthRate')] <- 
    estimateDoublingDaysOneCountry(lpti[ lpti[[variable]]>0, ], 
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
 # this means if you raise minVal, you leave the old simulation for countries that do not reach it. 
 lptivalid <- lpti[ lpti$confirmed >=  minVal & lpti$Date >=  minDate & lpti$Date <=  maxDate, ]
 for (country in countries) {
  nrRows <- NROW(lptivalid[lptivalid$PSCR  ==  country, ])
  if ( nrRows  ==  0) {
   if (verbose >=  4 )
   {print('addsimvarsCountry not simulated'  % %  country  % %  'minVal = '  % %  minVal )
   }
  }
  else {
   details <- lptivalid[lptivalid$PSCR  ==  country, ][1, ]
   temp <- simulGrow(lptivalid[lptivalid$PSCR  ==  country, ], country, ...) 
   names(temp)[names(temp) %in% c('confirmed', 'deaths', 'recovered', 'active')] <- 
                   c('confirmed', 'deaths', 'recovered', 'active') %#% ext
   newnrRows <- nrow(temp)
   lpti[lpti$PSCR  ==  country & lpti$confirmed >=  minVal & lpti$Date >=  minDate & lpti$Date <=  maxDate, 
           c('confirmed', 'deaths', 'recovered', 'active') %#% ext] <- 
    temp[1:nrRows, c('confirmed', 'deaths', 'recovered', 'active') %#% ext]
   if (newnrRows > nrRows) { # we have simulated extra! 
    temp <- temp %>% mutate(
     Country.Region = details$Country.Region, 
     PSCR = details$PSCR, 
     Province.State  = details$Province.State, 
     Region  = details$Region, 
     Lat  = details$Lat, 
     Long  = details$Long )
    temp[, setdiff(names(lpti), names(temp))] <- NA
    lpti <- rbind(lpti, 
           temp[nrRows+1:newnrRows, names(lpti)])
   }
   else if (newnrRows < nrRows) print( country  % %  'has' % %  newnrRows-nrRows  % %  ' extra rows. Oi va voi!')
  }
 }
 lpti
}

graph_DemoDoubling <- function(lpti = ECDC, doublingDays = 3, nrRows = -1){
 simulGrow(lpti, "France", minVal = 10, doublingDays) %>% 
  graphit("France" % % doublingDays  % %  "days", xvar = 'day', yvars = c('active', 'recovered', 'deaths', 'confirmed', "population") , logy = TRUE, until = max(.$Date)) %>%
  .[c('deaths', 'active', 'confirmed', 'recovered')] %>% view #apply(2, max, na.rm = TRUE) 
}


addTotals3 <- function(lpti = ECDC, totregions = "", ID = 'Region'){
 if (totregions[1]  == "") totregions <- c("World", unique(lpti$Region))
 lpti1 <- lpti %>%
  #just to be sure, that if i do it twice i dont get double counts. 
  #And omit USA as country, as we have the individual states already. 
  filter(!(!!ID %in% totregions)) 
 World <- unique(lpti1[[ID]])
 varnames  = c("confirmed", "recovered", "deaths", "population")
 if(!('Lat' %in% names(lpti))) lpti1$Lat <- NA
 if(!('Long' %in% names(lpti))) lpti1$Long <- NA
 for (regio in totregions[!is.na(totregions)])
  lpti <- rbind(lpti, 
        lpti1%>% total(regio , ID = ID, varnames = varnames, newrow = regio))
 lpti
}


addTotals2 <- function(lpdf = JHH, totregions = "", ID = 'PSCR'){
 lpti <- lpdf %>%
  filter(!(Country.Region  == "US" | !!ID %in% c("South America", "Asia", "Africa", "Europe", "China", "Australia", "Canada", 'North America', "World") )) #%>%   filter()
 #just to be sure, that if i do it twice i dont get double counts. 
 #And omit USA as country, as we have the individual states already.
 
 World <- unique(lpti[[ID]])
 if (verbose >= 2) {
  print('world totals include the following countries: ')
  print(World)}
 varnames = c("confirmed", "recovered", "deaths", "population")
    #, 'recovered_imputed', "active_imputed") # we impute AFTER this functionso no need for these. 
 rbind(lpdf, 
    lpti %>% totals(c("China", "Australia", "Canada"), 
           ID = "Country.Region", varnames = varnames), 
    lpdf %>% totals(c("US"), ID = "Country.Region", varnames = varnames), 
    lpti %>% total(regios$Europe, ID = ID, newrow = "Europe", varnames = varnames), 
    lpti %>% total(regios$Africa, ID = ID, newrow = "Africa", varnames = varnames), 
    lpti %>% total(regios$Asia, ID = ID, newrow = "Asia", varnames = varnames), 
    lpti %>% total(regios$SAmerica, ID = ID, newrow = "South America", varnames = varnames), 
    lpti %>% total(regios$NAmerica, ID = ID, newrow = "North America", varnames = varnames), #bug solved :instead of taking the states, US data, we now use the USA data, whichs has recovered available. 
    lpti %>% total(World , ID = ID, newrow = "World", varnames = varnames)
 ) 
}

days2overtake <- function(lpti = JHH ,
  #countries = c('Belgium', 'China', 'Russia', 'Netherlands', 'Colombia', 'Iran'), 
  varname = 'confirmed', newvarname = 'new_confirmed'){
 #'listed by decreasing nr of confirmed, and smallest is equals to largest in so many days / so many days ago')

 lastdata <- lpti#[lpti$PSCR %in% countries,] %>%    filter(Date  == max(Date))
                                                                                         
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
 lastdata <- lpti[, c('PSCR','Date', varname, newvarname)] %>% group_by(PSCR) %>% 
   filter(Date >= max(Date) - lastDays + 1) %>%
   mutate(!!newvarname := ma( .data[[newvarname]],lastDays,sides = 1)) %>%  
   mutate_at( .vars=4, .funs= ma) %>%
   filter(Date  == max(Date))
 #lastdata <- lpti[, c('PSCR', 'Date', varname, newvarname)] %>% group_by(PSCR) %>% 
  # filter(Date  ==  max(Date))  
 mydata <- subset(lastdata, PSCR  ==  country)
 if (who  ==  'theyme') {
  countries <- lastdata[lastdata[[varname]] <=  mydata[[varname]] &
              lastdata[[newvarname]] >=  mydata[[newvarname]], 'PSCR']
  colName <- 'days to overtake'  % %  country  } 
 else if (who  ==  'Ithem') {
  countries <- lastdata[lastdata[[varname]] >=  mydata[[varname]] &
              lastdata[[newvarname]] <=  mydata[[newvarname]], 'PSCR']
  colName <- country  % %  'will overtake in'  }
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


addcounterfrommin <- function(lpdf = JHH, minv = 0, varname = "confirmed", ID = "PSCR", counter = "day"){
 lpdf[, counter] <- as.numeric(NA)
 lpdf <- lpdf%>% filter(!is.na(!!varname))  #[!is.na(lpdf[, varname]), ] #should not have any! effect for "confirmed" 
 if(sum(lpdf[, varname] >= minv)>0) 
  lpdf[lpdf[[varname]] >= minv, ] <- 
     ddply(lpdf[lpdf[[varname]] >= minv, ], ID, 
      function(lpdf){
       lpdf[[counter]] <- seq_along(lpdf$Date)
       lpdf}
      )
  lpdf
}

### make day vars for tableau & Excel
makecountname <- function(countname, minv){paste(countname, minv, sep = "_")}

writeWithCounters <- function(lpdf = JHH, varname = "confirmed", ID = "PSCR", name = "JHH"){
  lpdf <- as.data.frame(lpdf)
   lpdf <- lpdf[!is.na(lpdf[c(varname)]), ]
  for (minv in c(1, 20, 100, 400, 1000, 2000, 5000, 1e4, 5e4, 1e5, 5e5, 1e6)) {
   lpdf <- addcounterfrommin(lpdf = lpdf, minv = minv, 
                varname = varname, ID = ID, 
                counter = makecountname("day", minv))
  }
  filename = paste(name, "days.csv", sep = "_")
  write_csv(lpdf, path = datapath %#% "/" %#% filename, na = "")
  write_csv(lpdf, path = myPlotPath %//% "data" %//% filename, na = "")
  if (verbose >= 1) print(paste("Written the current data with counters to disk as", filename, "for use in Tableau or Excel"))
}

dataprep <- function(lpdf = JHH, minVal = 1, ID = "PSCR", 
           xvar = "day", yvars = c("confirmed", "recovered"), 
           logx = FALSE, logy = TRUE, sorted = TRUE){
 if (!(xvar %in% names(lpdf))) 
  lpdf <- lpdf %>% addcounterfrommin(minVal, varname = "confirmed", ID = ID, counter = xvar)
 #lpdf <- lpdf %>% filter/mutate(confirmed >= minVal) #bug? is new, moved here from addcountersfrommin
 if (logy){ #get rid of negative and zeros for the log
  
  eps <-  1e-5
  for (varname in yvars) {
   if (sum((!is.na(lpdf[, varname]))&lpdf[, varname] <=  eps)>0) 
    #if all NAs, one row replaces zero rows -> error!
    lpdf[(!is.na(lpdf[, varname]))&lpdf[, varname] <=  eps, varname] <- NA 
 } }
 if(logx) lpdf[lpdf[[xvar]] <= 0, xvar] <- 1 #bug? should be NA to be honest 
 if(sorted) {
  lpdf[[ID]] <- sortIDlevels(lpdf = lpdf, varname = yvars[1]) 
  if (verbose >=  7) print(unique(lpdf[[ID]]))
  lpdf <- lpdf[order(lpdf[[ID]], lpdf[[xvar]]), ] 
  if (verbose >= 5) print('Dataprep'  % %  levels(lpdf[[ID]]))
 } 
 lpdf <- lpdf[, c(xvar, ID, yvars)]# , 'Date' ungroup %>% arrange(PSCR)
}
#Bug potential: after the sort, PSCR is a factor. before, it was character!

graphit <- function(lpti, countries, minVal  = 1, ID  = "PSCR", xvar  = "Date", 
          yvars  = c("active", "recovered", "deaths", "confirmed"), 
          fuzzy  = FALSE, logx  = FALSE, logy  = FALSE, yline  = FALSE, 
          myfolder1  = 'random', myfolder  = "", savename  = "", putlegend = TRUE, size = 2, 
          returnID  = "PSCR", area  = FALSE, position  = 'stack', facet  = FALSE, 
          sorted  = TRUE, from = '2019-12-01', until  = Sys.Date()){
 
 lpdf <- as.data.frame(lpti[lpti$Date >=  from & lpti$Date <=  until & lpti$confirmed >=  minVal, ])
 if (verbose >= 7)print(lpti)
 if (typeof(until)  == "character") until = as.Date(until, format = "%Y-%m-%d")
 lastdate <- min(max(lpdf$Date), until)
 if (missing(countries)) {
  countries <- unique(lpdf[[returnID]])
  if(length(countries> 40)) return(print('too many countries, you wont see anything. Please select less countries'))
  }
  countries <- findIDnames(lpdf, testIDnames = countries, searchID = ID, 
              fuzzy = fuzzy, returnID = returnID) #}
 ID <- returnID
 if (verbose >= 7) {print(countries)}
 lpdf <- lpdf%>% filter(PSCR %in% countries) #&Date <= until) 
 y_lab <- paste(sort(yvars), collapse = " & ") % % ifelse(logy, "(log)", "")
 if (str_length(y_lab)>80) 
  y_lab <- paste(initials(sort(yvars)), collapse = "&")  % %  
  ifelse(logy, "(log)", "")
 mytitle <- #format(min(lpdf$Date), format  = "%Y %m-%d")  % %  'till'  % %  
       #format(lastdate, format  = "%m%d")  % %  
  "C19" % %  savename % %  y_lab % % 
         "by"  % %  xvar  % %  "for"  % %  minVal %#% "+"  % %  "confirmed"
 
 if (nrow(lpdf)  ==  0 ) {if (verbose >=  4) {print('graphit'  % %  mytitle  % % " No data")}
           return() }
 lpdf <- dataprep(lpdf, ID  = ID, minVal  = minVal, xvar  = xvar, yvars  = yvars, logx  = logx, 
          logy  = logy, sorted  = sorted)
 if (verbose >= 5){print ('graphit columns left');print( names(lpdf))}
 if (nrow(lpdf)  == 0| all(is.na(lpdf[, xvar]))|all(is.na(lpdf[, yvars])))
  return(if (verbose >= 4) print ('graphit'  % %  paste(mytitle, "Too little data to graph. Maybe lower the mininum value, take more regions?")))
 
  lpdf <- lpdf %>% 
   melt(lpdf , id = c(ID, xvar), measure.vars = yvars, 
          variable.name = "variable", value.name = "count")%>%
   mutate ( mygroup = PSCR %, % variable, 
        variable = factor(variable, levels  = yvars)) %>%   drop_na()#
 if (verbose >= 7) {print('graphit summary pdf:');print(summary(lpdf))}
 
 if (facet  == 'variable') lpdf$mygroup <- lpdf[[ID]] 
 else if (facet  == ID) lpdf$mygroup <- lpdf$variable
 nrgroups <- length(unique(lpdf$mygroup))
 if (verbose >= 5) print( 'graphit'  % %  parse(text = substitute(xvar)) % % "from" % %  #bug why not just xvar? why parse substitute? 
             min(lpdf[, xvar]) % %  "to" % % max(lpdf[, xvar]) %, % 
            "group by " % %  lpdf$mygroup[1]%, % "facets"  % %  facet)
 len <- length(unique(lpdf[, ID]))
 myplot <- ggplot(lpdf, aes_string(y = "count", x = xvar, group = 'mygroup', 
               color = ifelse(len  == 1,  
                      'variable' , 
                      ifelse(facet  == ID, 'variable', ID))
                  ), na.action = na.omit)
  
 if(area){posalpha <- ifelse(position  == 'identity', 0.4, 1)
  myplot <- myplot + geom_area(aes_string(
      color = ifelse(len  == 1 |facet  == ID, 'variable' , 'mygroup'), 
      fill = ifelse(len  == 1|facet  == ID,  'variable' , 'mygroup')), 
      position  = position, alpha = posalpha)
  myscale_fill <- scale_fill_manual(values  = c("red", "green", "black", "darkorange", "lawngreen"))
  if (nrgroups <= 2) scale_f <- scale_fill_manual(values  = c("lawngreen", "cyan"))#, "black", "darkorange", "lawngreen"))
  myplot <- myplot+myscale_fill+ 
   scale_color_manual(values  = c("red", "green", "black", "darkorange", "lawngreen"))
 }else {
  myplot <- myplot + #line plot
   geom_line(alpha = 0.3, size = size*0.7)+
   geom_point(size = size, aes_string(  shape = 'variable'))+
   if(!putlegend| facet  == FALSE) geom_dl(aes_string(x = xvar, y = "count",  label = 'mygroup'),    
      method  = list(dl.trans(x  = x+0.1 , y = y+0.1), "last.points", cex  = 1.2)) 
  if ( yline ) myplot <- myplot + geom_hline( yintercept  = yline, na.rm  = TRUE) 
  if (length(unique(lpdf$variable)) <= 6 ) 
   myplot <- myplot + scale_shape_manual(values  = c(0, 1, 3, 2, 10, 5, 6)) #shape = "\u2620"
  if (nrgroups <= 6){
     myscale <- scale_color_manual(values = c("red", "darkgreen", "black", "orange", 
                         "lawngreen", "tomato"), #darkorange
                    guide = ifelse(putlegend, "legend", FALSE))
  }else if(nrgroups<13) {
   palette = ifelse (nrgroups <8, "Dark2", "Paired") #Spectral Set2 
   myscale <- scale_color_brewer(palette = palette)
  } else myscale <- scale_color_discrete(guide = ifelse(putlegend, "legend", FALSE))
  myplot <- myplot + myscale 
 } 
 
 if (!isFALSE(facet)) {
  myplot <- myplot+ facet_wrap(as.formula(paste("~", facet)), strip.position = "bottom")}
 if(xvar  == "Date") myplot <- myplot+scale_x_date(labels  = date_format("%d-%m"))
 myplot <- myplot + ylab(y_lab)+
  xlab(paste(xvar, ifelse(logx, "(log scale)", "")))+ 
  ggtitle(mytitle) + theme_light()+   
  guides(col  = guide_legend(nrow = 30, ncol  = min(2, (nrgroups-1) %/% 30+1))) 
 
 breaks <- rep(c(.5, 1, 2), 21)*10^rep((-10:10), each = 3)
 minor_breaks <- rep( 1:5, 21)*(10^rep(-10:10, each = 5))
 if( logy  !=  FALSE) myplot <- myplot+scale_y_continuous(trans = 'log10', breaks  = breaks, minor_breaks  = minor_breaks)+
  annotation_logticks() 
 if(logx) myplot <- myplot+scale_x_continuous(trans = 'log10', breaks  = breaks, minor_breaks  = minor_breaks)
 myplot <- myplot+ theme(
  axis.text  = element_text(color  = "blue", angle  = 45, 
               hjust  = 1, vjust  = 0.5, size  = rel(.8)),   
  strip.background  = element_rect(fill = "white", color = 'black'),   
  strip.text  = element_text(color  = 'black'))

 if (savename !=  "") {
  if(facet  == FALSE) savename <-  paste(savename, "all-in-one")
  if(area) savename <- paste(savename, "area plot")
  if (myfolder  == "") { myfolder <- myfolder1 %//% sort(initials(yvars)) % % 'by' % % xvar}
  else myfolder <- myfolder1 %//% myfolder
  if(area)myfolder <- myfolder  % %  "area plot"
  if (logy) myfolder <- myfolder  % % 'log scale'
  if(facet  == FALSE) myfolder <-  paste(myfolder, "all-in-one")
  if (verbose >=  4) print("graphit making plot"  % %  myfolder %#% "/" %#% mytitle)
  myplot <- myplot + theme(text = element_text(size  = 20), 
  axis.text  = element_text(color  = "blue", size  = rel(.8)) )
  if (myfolder  !=  "") myPath <- myPath %//% myfolder
  if (!dir.exists(myPath)) dir.create(myPath, recursive  = TRUE)
  png(filename  = myPath %//% mytitle %#% ".png", width  = 1600, height  = 900)
  on.exit(while (!is.null(dev.list())) dev.off() )
  
   print(myplot)
  dev.off()
 }else {
  print(myplot + theme(title  = element_text(size  = 11)))
  }
 invisible(lpdf)
}# 

#geom_point(shape = "\u2620", size  = 4) #skulls
#geom_point(aes(shape = cyl, color = cyl, size = cyl))
#+scale_color_manual(values = c('#999999', '#E69F00', '#56B4E9'))
 
graphCodes <- function(){ 
 print('naming system:')
 print(list(nrvars = 1:6, 
     letters = tibble(codes = c('d/D/c/d', 'acrd_', 'f', 'i', 'M', 'n', 'y', 'a/l'), 
     meaning = c('day, Date, confirmed, death xvar', 'initials/abbrev of yvars', 'facet per ID', 
          'imputed', 'per million', 'new', 'logy', 'area or line')
     )  ))
 writeLines('example graph1Dc_fiMnyl: confirmed by Date,  facet (by ID), use imputed, per Million, new (not cumulative), logy, line plot). 

The variable "myGraphNrs" contains all daily interesting graph functionnames one can pass as a parameter to curGraph or makeHistoryGraphsRG.')
}

initials <- function(text = c('test_1', 'of_imputation', 'new_recovered_imputed_per_Million')){
 paste(unlist(lapply(
  lapply ( strsplit(text, '_'), function(st) substr(st, 1, 1)), 
  function(vec) paste(vec, collapse = "_")
 )), collapse = '+')
}
rm(list = ls(pattern = "graph[[:digit:]]"))
#in alfphabetical order of outputs
#graph4dardc_fiMyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
# graphit(lpdf, countries, xvar = "Day", 
#     yvars = c('active_imputed_p_M', 'recovered_imputed_p_M', 'deaths_p_M', 'confirmed_p_M'), 
#     logy = logy, facet = "PSCR", ...)
#}
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

graph1Dc_finl <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = 'Date', 
         yvars = c("new_confirmed"), facet = 'PSCR', putlegend = TRUE, ...)
}
graph1Dc_fil <- function(lpdf = JHH, countries, logy = FALSE, ...){
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
         logy = logy, yline  = 0.025, facet = 'PSCR', ...)
}


graph2Dgnar_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
 graphit(lpdf, countries, xvar = 'Date', 
         yvars = c("new_active_rate", 'active_imputed_growthRate'), 
         logy = logy,  yline = 0.025, facet = 'PSCR', ...)
}

graph2crd_il <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = 'confirmed', 
         yvars = c('recovered_imputed', 'deaths'), ...)
}

graph1dr_iyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
 graphit(lpdf, countries, xvar  = 'deaths', yvars  = c('recovered_imputed'), 
         logy  = logy, logx  = TRUE, ...)
}
graph1Drr_il <- function(lpdf = JHH, countries, myfolder  = '', ...){
 graphit(lpdf, countries, xvar = "Date", 
         yvars = c('recovered_imputed_per_confirmed'), 
         myfolder = myfolder %#%"recovery rate", 
         putlegend = FALSE , ...   )
}
#Simulation included
graphDccp_fyl <- function(lpdf = JHH, countries, logy = TRUE , ext = "_sim", ...){
 graphit(lpdf, countries, myfolder = "Confirmed infections simulated", 
     yvars = c('confirmed' %#% ext, 'confirmed', "population"), 
     logy = logy, facet = 'PSCR')
}

graphDccprr_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ext = '_sim', ...){
 graphit(lpdf, countries, myfolder = "Confirmed recovered simulated", 
     yvars = c('confirmed' %#% ext, 'confirmed', "population", 'recovered' %#% ext, 
         'recovered_imputed'), 
     logy = logy, facet  = 'PSCR', ...)
}

graphDddp_fyl <- function(lpdf = JHH, countries, logy  = TRUE, ext = "_sim", ...){
 graphit(lpdf, countries, minVal = minVal, myfolder = "deaths simulated", 
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
graph1dnar_iyl <- function(lpdf  = JHH, countries, minVal = 10, logy = TRUE, ...){
 graphit(lpdf, c(countries), minVal, xvar = 'day', 
     yvars = c('new_active_rate'), logy = logy, yline = 0.025, ...) #putlegend = TRUE
}

# for testing imputation quality, do not add numbers otherwise they get done for all regios.
graphDaa_fia <- function(lpdf = JHH, countries, ...){
 graphit(lpdf, countries, xvar = "Date", 
         yvars = c('active_imputed', 'active'), 
         area = TRUE, position = 'identity', facet = 'PSCR', ...) 
}

graphDaa_fiyl <- function(lpdf = JHH, countries, logy = TRUE, ...){
  graphit(lpdf, countries, xvar = 'Date', 
     yvars = c( 'active_imputed', 'active'), facet = ID, 
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
 for (myGraph in graphlist) {
  if (verbose >=  3) print( 'graph:'  % %  myGraph)
  do.call(myGraph, args  = list(lpdf, countries, savename, ...))
 }
}
reportDiffTime <- function(message, mytime, units = 'mins', precision  = 2){
 print(message  % %  round( difftime(Sys.time(), mytime, units  = units), precision)  % %  units)
}

timer <- function(mycall, message  = 'duration', verbosity  = 1, ...){
 if (verbose > verbosity) {
  tistart  = Sys.time(); print(format(Sys.time(), "%H:%M:%S " )  % %  message)}
 do.call(myCall, ...)
 if (verbose >=  verbosity) {reportDiffTime(myCall %:% message, tistart)}
}

graphOnRegion <- function(lpdf, myRegion, myGraph, saveit = TRUE, ...){
 if(verbose >= 4) print( 'Regions' % %    paste(myRegion, collapse = "/ "))
 if(verbose >= 3) { tir <- Sys.time() ; print(tir%: % myGraph  % %  myRegion[1] ) }
 do.call (myGraph, 
      args = list(lpdf, myRegion, savename  = ifelse(saveit, myRegion[1], ""), ...))
 if(verbose >= 3) {
  reportDiffTime (myGraph  % %  myRegion[1] % %  "duration:", tir)}
}

byRegionthenGraph <- function(lpdf = JHH, regions, saveit = TRUE, 
               graphlist = c('graphDccprr_fiyl', 'graphDddp_fyl'), ...){
 ### this is writeRegioGraph in same order
 if (typeof(regions)  == "character") { regions = list(regions) }
 walk(graphlist, function(myGraph){
  if(verbose >= 2) {tig = Sys.time(); print(format(Sys.time(), "%H:%M:%S " ) % % myGraph)}
  walk(regions, function(myRegion)
     graphOnRegion(lpdf = lpdf, myRegion, myGraph, saveit = saveit, ...) )
  if(verbose >= 2) {reportDiffTime(myGraph, tig)}
 })
}
byGraphthenRegion <- function(lpdf = JHH, regions, graphlist = c('graphDggnar_fiyl'), saveit = TRUE, ...){ 
 if (typeof(regions)  == "character") { regions = list(regions) }
 walk(regions, function(myRegion){
  if(verbose >= 2) {tig = Sys.time(); print(format(Sys.time(), "%H:%M:%S " ) % % myRegion[1])}
  walk(graphlist, function(myGraph)
    graphOnRegion(lpdf  = lpdf, myRegion, myGraph, saveit = saveit, ...) )
  if(verbose >= 2) {reportDiffTime(myRegion[1], tig)}
 })
} 

makeDate <- function(chardate = "", format  = myDateFormat){
 tryCatch(as.Date(chardate, format = format), 
      error = function(e){print(paste("Either enter a date or a string (please use the following Date format for the string:", myDateFormat ))})
}

curGraph <- function(ord = 'GR', myfolder1  = 'current', ...){
 if (ord  ==  'RG' ) byRegionthenGraph( myfolder1  = myfolder1 , ...)
 else byGraphthenRegion(myfolder1  = myfolder1, ...)
}

makeHistoryGraphsRG <- function(lpdf, regions, graphlist = myGraphNrs, 
                fromdates  = as.Date(max(JHH$Date), format  = myDateFormat), 
                todates  = as.Date(max(JHH$Date), format  = myDateFormat), ...){
 on.exit({options(warn = 0) }) 
 if (missing(regions) ) stop("no regions to graph")
 if (typeof(fromdates)  == "character") { fromdates <- makeDate(fromdates)}
 if (typeof(todates)  == "character") { todates <- makeDate(todates)}
 if (any(is.na(todates)) ) print("Not all dates recognized:" % %  paste(dates, collapse = ", ") %#% 
                ". Either enter an R date or a string in the following Date format:"  % % 
                myDateFormat )
 fromdates[is.na(fromdates)] <- '2019-12-31'
 map2(fromdates, todates, function(from, to){
  if(verbose >= 1) {
   ti_da = Sys.time() 
   print(format(ti_da, "%H:%M:%S ")  % %  "doing"  % %  as.Date(until, origin = "1970-01-01"))
  }
  if(nrow(lpdf[lpdf$Date <=  until, ])>0) {
   byRegionthenGraph(lpdf, regions, graphlist, saveit  = TRUE, 
            from  = from, until  = to, 
       myfolder1  = min(to, max(format(JHH$Date, format  = '%Y-%m-%d') )), ...)
  }
  else print("no data from " % %  as.Date(from, origin = "1970-01-01")  % % 
             'to'  % %  as.Date(to, origin = "1970-01-01"))
  if(verbose >= 1) {
   reportDiffTime( as.Date(to, origin = "1970-01-01")  % %  "duration: ", ti_da)}
  while(!is.null(dev.list())) dev.off() 
 })
}

#options(warn = 2 ) #all warnings become errors, stops execution. 
#traceback() # to trace the nested calls leading to an error. 
#suppresswarnings() to suppresswarnings of the functioninside. 

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

findMaxCCF <- function(var1 = "new_recovered", var2  = "new_confirmed", myPSCR  = "Hubei, China", 
            lpdf  = JHH, N  = 5){
 if (myPSCR  !=  "") lpdf <- lpdf[lpdf$PSCR  ==  myPSCR, ]
 lpdf <- lpdf[lpdf$Date > "2020-01-22", c("Date", var1, var2)]
 if (all(is.na(lpdf[, var1])) | all(is.na(lpdf[, var2]))) 
  return(data.frame( cor  = NA, lag  = NA)) #
 d <- ccf.vf(lpdf[, var1], lpdf[, var2], lag.max = 30, plot  = FALSE)
 if (verbose >= 2) print (myPSCR)
 res  = data.frame( cor = d$acf[, , 1], lag = d$lag[, , 1])
 if (N%%2  == 0)N = N-1
 a <- res[order(res$cor, decreasing = TRUE)[1:N], ]
 if (verbose >= 3) print(a)
 res_max  = median( a$lag) #which.max(res$cor) #instead of max, take the n largest: order(R, decreasing = TRUE)[1:N]
 return(res[res$lag  == res_max, ]) 
} 

findMaxCCFs <- function(var1 = "new_recovered", var2 = "new_confirmed", myPSCR = "", lpdf = JHH){
 a <- ddply( lpdf, "PSCR", function(lpdfp){findMaxCCF(var1 = var1, var2 = var2, myPSCR = myPSCR, lpdf = lpdfp)})
 a[!is.na(a$lag), ]
}


#end. Now run loadData.R

