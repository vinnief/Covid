#deprecated functions


##make sure we have the right column names. This version not used. 
multigrep<- function( searchlist,inlist,ignorecase=FALSE){
  unlist(llply(searchlist,function(a) grep(a,inlist, value=TRUE,ignore.case=ignorecase)))
}
multigrep(testcountries,unique(alldata$Country.Region),ignorecase=TRUE)
#this one finds exact names when needed

sortbyvar <- function(lpti, sortVar = 'active_imputed', ID = 'PSCR', ...){
  lpti[[ID]] <- lpti %>%  sortIDlevels(sortVar = sortVar, ID = ID, ...) 
  lpti <- lpti[order(lpti[[ID]], lpti[[sortVar]]), ]  
}


totals2 <- function(lpdf, rows = "", # before used only for county to state totalling. deprecated 
                    ID = "Country.Region", varnames = c("confirmed", "deaths") ){
  if (rows[1]  == "") rows = unique(lpdf[, ID])
  if (verbose > 5) print(paste("Making totals2 for ", paste(rows, collapse = ", "), "in", ID))
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

addtotals<- function(lpdf=JHH,ID='PSCR'){
  lpt<- 
    #just to be sure, that if i do it twice i dont get double counts. 
    #And omit USA as country, as we have the individual states already. 
    lpdf[! lpdf[[ID]] %in% c("South America", "Asia", "Africa", "Europe","China","Australia","Canada","USA","US","World"),] 
  print("Deprecated. use extravars first, then addtotals2, which does not call extravars several times. ")
  World<- unique(lpt[[ID]])
  varnames=c("confirmed","recovered", "deaths","population",'recovered_imputed',"active_imputed") #use this function AFTER imputing!
  rbind(lpdf, 
        lpt%>% total(World ,ID=ID,newrow="World", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Europe,ID=ID,newrow="Europe", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Africa,ID=ID,newrow="Africa", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Asia,ID=ID,newrow="Asia", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$SAmerica,ID=ID,newrow="South America",varnames= varnames)%>%
          extravars2,
        lpt %>% totals(c("US","China","Australia", "Canada"),
                       ID="Country.Region", varnames= varnames)%>% extravars2
  )
}
addTotals2 <- function(lpdf = JHH, totregions = "", ID = 'PSCR', 
                       varnames = c("confirmed", "recovered", "deaths", "population")){
  lpti <- lpdf  %>% 
    filter(!(Country.Region  == "US" | !!ID %in% c("South America", "Asia", "Africa", "Europe", "China", "Australia", "Canada", 'North America', "World") )) # %>%    filter()
  #just to be sure, that if i do it twice i dont get double counts. 
  #And omit USA as country, as we have the individual states already.
  World <- unique(lpti[[ID]])
  if (verbose >= 2) {
    print('world totals include the following countries: ')
    print(World)}
  rbind(addCountryTotals(lpdf),
        lpti  %>%  total(regios$Europe, ID = ID, newrow = "Europe", varnames = varnames), 
        lpti  %>%  total(regios$Africa, ID = ID, newrow = "Africa", varnames = varnames), 
        lpti  %>%  total(regios$Asia, ID = ID, newrow = "Asia", varnames = varnames), 
        lpti  %>%  total(regios$South_America, ID = ID, newrow = "South America", varnames = varnames), 
        lpti  %>%  total(regios$North_America, ID = ID, newrow = "North America", varnames = varnames), 
        #bug solved :instead of taking the states, US data, we now use the USA data, whichs has recovered available. 
        lpti  %>%  total(World , ID = ID, newrow = "World", varnames = varnames)
  ) 
}

ma.diff.lpdf<- function(lpdf,id="PSCR", varnames=c("confirmed","active","recovered", "deaths"),prefix="new_",n=3){
  ans<- ddply(lpdf, id, 
              function (lpdf){
                data.frame(llply(lpdf[,varnames], 
                                 function(a){c(NA,ma(base::diff(a),n))}
                ))
              }
  )
  ans[,id]<- NULL
  names(ans)<- paste(prefix,varnames,sep="")
  ans
}

doublingLine_old<- function(lpti=JHH,country, start, doublingDays=5,nrRows=100,
                            deathRate=.05,pop=1e7, lagrc=LAGRC,lagdc=LAGDC ){
  
  if(!missing(country)){
    lpti <- lpti%>% filter(PSCR %in% country)
    if (missing(start)) start<-(lpti$confirmed)[1]
    # if (start<1) start<-(lpti$confirmed)[1]
    if (missing(doublingDays)) doublingDays= lpti[lptiPSCR==country,]$confirmed_doublingDays[1]
    if(NROW(lpti)==0) stop("could not find that country '"%+% country %+%"'in the data")
    myPSCR<-lpti$PSCR[1] % % doublingDays % % 'days'
  }else {     myPSCR<- doublingDays % % 'days'   }
  if (!missing(nrRows)&!nrRows<0) maxDate <- min(lpti$Date)+nrRows-1 
  else {maxDate<- max(lpti$Date) ;nrRows<- maxDate-min(lpti$Date)+1}
  if(maxDate==-Inf)stop("Max Date equals -inf. Probably we have an empty data set. Did you choose the right country? ")
  if (verbose>=3) print('dL:' % %myPSCR % % 'to double:' % %"R0=" % % round(2^(lagrc/doublingDays)-1,2) %, %'Simulated' % % nrRows% %'days until' % % maxDate)
  out=tibble(Date= seq(from=min(lpti$Date),to= maxDate,by=1))
  doubling<- round(start*2^((0:(nrow(out)-1))/(doublingDays) ))
  out<- out%>% mutate (
    confirmed= pmin(doubling,pop), 
    deaths= round(deathRate*lag(confirmed,lagdc,default=0)), 
    recovered= lag(confirmed, lagrc,default=0),
    active=confirmed-deaths-recovered,
    population=pop-deaths
  ) 
  out
}

graphit <- function(lpti, countries, minVal  = 1, ID  = "PSCR", xvar  = "theDate", 
                    yvars  = c("active", "recovered", "deaths", "confirmed"), 
                    fuzzy  = FALSE, logx  = FALSE, logy  = FALSE, intercept  = FALSE, slope = FALSE,
                    myFolderDate  = 'random', myFolderType = "", savename  = "", putlegend = TRUE, size = 2, 
                    returnID  = "PSCR", area  = FALSE, position  = 'stack', facet  = FALSE, 
                    sorted  = TRUE, smoothvars = yvars, smoothn = FALSE,
                    from = '2019-12-31', to  = Sys.Date()){
  
  lpdf <- as.data.frame(lpti[lpti$theDate >=  from & lpti$theDate <=  to & lpti$confirmed >=  minVal, ])
  lastdate <- max(lpdf$theDate)
  if (typeof(to)  == "character") to = as.Date(to, format = "%Y-%m-%d")
  if (missing(countries)) {
    countries <- unique(lpdf[[returnID]])
    if (length(countries > 40)) return(message('too many countries, you wont see anything. Please select less countries'))
  } else countries <- findIDnames(lpdf, testIDnames = countries, searchID = ID, 
                                  fuzzy = fuzzy, returnID = returnID) #}
  ID <- returnID
  if (verbose >= 8) {message("countries to graph ", countries)}
  lpdf <- lpdf[lpdf[[ID]] %in% countries,] #PSCR
  if (verbose >= 9) {message('graphi countrydata' );message(head(lpdf))}
  y_lab <- paste(sort(yvars), collapse = " & ") % % ifelse(logy, "(log)", "")
  if (str_length(y_lab) > 70) 
    y_lab <- paste(initials(sort(yvars)), collapse = "&")  % %  
    ifelse(logy, "(log)", "")
  mytitle <- savename % % y_lab % % "by" % % xvar % %  "for" % % minVal %#% "+"  % %  "confirmed"
  myFilename <- "C19" % % mytitle
  
  if (nrow(lpdf)  ==  0 ) {return( if (verbose >=  4) {message('graphi'  % %  mytitle  % % " No data")} ) }
  mytitle <- "C19" % % format(min(lpdf$theDate), format  = "%Y-%m-%d")  % % '-' % %  
    format(lastdate, format  = "%Y-%m-%d")  % %  mytitle
  
  lpdf <- dataprep(lpdf, ID  = ID, minVal  = minVal, xvar  = xvar, yvars  = yvars,
                   logx  = logx, logy  = logy, sorted  = sorted, smoothvars= smoothvars, smoothn = smoothn)
  if (verbose >= 7) {message('graphi columns left');message( names(lpdf))}
  if (nrow(lpdf)  == 0 || all(is.na(lpdf[, xvar])) || all(is.na(lpdf[, yvars])))
    return(if (verbose >= 6) message('graphi'  % %  paste(mytitle, "Too little data to graph. Maybe lower the mininum value, take more territories?")))
  
  lpdf <- lpdf  %>%  
    melt(lpdf , id = c(ID, xvar), measure.vars = yvars, 
         variable.name = "variable", value.name = "count") %>% 
    mutate( mygroup = PSCR %, % variable, #!!ID? you would get zigzags if ID <> PSCR and you havent totalled. 
            variable = factor(variable, levels  = yvars)) %>% drop_na()
  if (verbose >= 8) {message('graphi summary pdf:');message(summary(lpdf))}
  
  if (facet  == 'variable') lpdf$mygroup <- lpdf[[ID]] else 
    if (facet  == ID) lpdf$mygroup <- lpdf$variable
  lines_only <- lpdf %>% select(!!ID,mygroup) %>% group_by_at(c(1,2)) %>% 
    filter(n() > 1) #these are the IDs that need a line. geom_path uses this
  lines_only <- lines_only %>% unique()
  
  lpdf_lines_only <- lpdf[lpdf[[ID]] %in% lines_only[[ID]] & 
                            lpdf$mygroup %in% lines_only$mygroup, ] 
  #if (verbose >= 7) {view(lpdf_lines_only)}
  nrgroups <- length(unique(lpdf$mygroup))
  if (verbose >= 7) message( 'graphi'  % %  xvar % % "from" % %  
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
      geom_path(data = lpdf_lines_only, alpha = 0.3, size = size*0.7) +
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
    xlab(paste(ifelse(xvar =="theDate", "Date", xvar), ifelse(logx, "(log scale)", ""))) + 
    ggtitle(mytitle) + theme_light() +   
    guides(col  = guide_legend(nrow = 30, ncol  = min(2, (nrgroups - 1) %/% 30 + 1))) 
  
  breaks <- breaks_log(n = 5, base = 10) #rep(c( 1, 5), 21)*10^rep((-10:10), each = 2)
  minor_breaks <- rep( 1:5, 21)*(10^rep(-10:10, each = 5))
  if ( logy  !=  FALSE) myplot <- myplot + scale_y_continuous(trans = 'log10', breaks  = breaks, minor_breaks  = minor_breaks, labels = label_number_si()) + annotation_logticks() 
  if (xvar  == "theDate") myplot <- myplot + scale_x_date(labels  = date_format("%d-%m")) else 
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
    if (verbose >=  4) message("graphi making plot"  % %  myFolderType %#% "/" %#% mytitle)
    myplot <- myplot + theme(text = element_text(size  = 20), 
                             axis.text  = element_text(color  = "blue", size  = rel(.8)) )
    myPath <- ifelse(myFolderType  ==  "", myPlotPath,   
                     myPlotPath %//% myFolderType)
    if (!dir.exists(myPath)) dir.create(myPath, recursive  = TRUE)
    on.exit(while (!is.null(dev.list())) dev.off() )
    if (verbose >= 4) message("path length: " % % str_length(myPath %//% myFilename %#% ".png"))
    if (verbose >= 5) message("mypath" %: % myPath %//% myFilename %#% ".png" )
    
    
    png(filename  = myPath %//% myFilename %#% ".png", width  = 1600, height  = 900)
    print(myplot)
    dev.off()
  }else {
    print(myplot + theme(title  = element_text(size  = 10)))
  }
  invisible(lpdf)
} 
# 

graphs <- function(lpdf  = JHH, countries  = "World", graphlist  = myGraphNrs, ...) {
  #deprecated not used. 
  for (myGraph in graphlist) {
    if (verbose >=  3) print( 'graph:'  % %  myGraph)
    do.call(myGraph, args  = list(lpdf, countries, savename, ...))
  }
}

writeRegioGraph<- function(lpdf=JHH,regions,
                           graphlist=c('graphDccp_fyl','graphDccprr_fyl','graphDddp_fyl'), 
                           saveit=TRUE,minVal=1,
                           ID="PSCR", until=Sys.Date()){
  print("Deprecated. Please use byRegionthenGraph or byGraphthenRegion")
  if (typeof(regions)=="character") { regions=list(regions) }
  for (myGraph in graphlist){
    if(verbose>=2) {tig=Sys.time(); print(format(Sys.time(),"%H:%M:%S " )% %myGraph)}
    for (myRegion in regions){
      graphOnRegion(lpdf=lpdf,myRegion,myGraph,saveit=saveit,minVal=minVal,ID=ID,until=until)  }
    if(verbose>=2) {reportDiffTime(myGraph, tig)}
  }
}

#deprecated
makeHistoryGraphs<- function(lpdf,regions="", graphlist=myGraphNrs,
                             ID='PSCR', dates =as.Date(max(JHH$Date), format=myDateFormat)
){
  on.exit(options(warn=0)) 
  if (regions[1]==""){ #bug: if wrong dimensions, we get  Error in Ops.data.frame(lpdf, JHH) :    ‘==’ only defined for equally-sized data frames 
    if (dim(lpdf)==dim(JHH)& (lpdf==JHH)) regions=JHHRegios
    if (dim(lpdf)==dim(ECDC) & (lpdf==ECDC)) regions=ECDCRegios
  }
  if (typeof(dates)=="character") {  makeDate(dates)}
  if(any(is.na(dates))) print(paste("Not all dates recognized: ",paste(dates,collapse=","),". Either enter an R date or a string (please use the following Date format for the string:",myDateFormat ))
  for (until in dates ){
    if(verbose>=1) {
      ti_da=Sys.time() 
      print(format(ti_da,"%H:%M:%S ") % % "doing" % % as.Date(until,origin="1970-01-01"))
    }
    if(nrow(lpdf[lpdf$Date<=until,])>0) {  
      
      for (i in 1:(length(regions))){
        ti_reg=Sys.time()
        IDs<-findIDnames(lpdf=lpdf, testIDnames=regions[[i]],searchID=ID, 
                         fuzzy=FALSE,returnID="PSCR")
        if(verbose>=2) 
          print('At' % %format( ti_reg,"%H:%M:%S ") % % 
                  'doing'% %regions[[i]][1])
        if (verbose>= 4) print('regions'% % paste(IDs,collapse="/ "))
        lpdf%>% 
          graphs(countries =IDs,graphlist=graphlist,savename=regions[[i]][1],  ID=ID, 
                 until=until)
        if(verbose>=2) {print (regions[[i]][1] % %"duration"%: % 
                                 round(difftime(Sys.time(),ti_reg,units='mins'),2)%+%"mins")}
      }
      if(verbose>=1) {
        reportDiffTime( as.Date(until,origin="1970-01-01") % % "duration: ",ti_da)}
    }
    else print("no data for "% % as.Date(until,origin="1970-01-01"))
    while(!is.null(dev.list())) dev.off() 
  }
  
}

addDoublingDaysPerCountry2<- function(lpti,countries,variable='confirmed',...){
  if (!('doublingDays' %in% names(lpti))) lpti$doublingDays<- as.numeric(NA)
  if(!('growthRate' %in% names(lpti))) lpti$growthRate<- as.numeric(NA)
  if (missing(countries)) countries<- unique(lpti$PSCR)
  else countries<- findIDnames(lpti, countries, searchID='PSCR',
                               fuzzy=FALSE)
  for (country in countries){
    lpti[lpti$PSCR==country&lpti[[variable]]>0 , c('doublingDays','growthRate')]<- 
      estimateDoublingDaysOneCountry(lpti[lpti$PSCR==country &
                                            lpti[[variable]]>0,],
                                     variable=variable,...) 
  }
  lpti
}

addDoublingDays1<- function(lpti,variable='confirmed',minVal=100,nrDays=9,minDate="2019-12-31",maxDate='2020-12-31',newVar='doublingDays'){
  if (verbose>=4) print('addDdoublinggDays' % %minVal % % '= minVal, and variable ='% % variable)
  if (!(missing(minDate)|missing(maxDate))) nrDays<-as.numeric(as.Date(maxDate)-as.Date(minDate))+1
  lptisel<- lpti[lpti$confirmed >= minVal & lpti$Date >= minDate & lpti$Date<= maxDate,]%>% head(nrDays)
  if (sum(!is.na(lptisel[variable]))>3 ) 
    slope= lm(log2(confirmed)~Date, data=lptisel,na.action=na.exclude)$coefficients['Date']
  else {slope=0 #do something very wrong that will show up in graphs, but dont make an error. 
  if (verbose>=3) print(paste(unique(lpti$PSCR),collapse = ',')% %'not enough data to estimate the doubling days slope, filling with Zeros')}
  lpti[lpti$Date>=minDate & lpti$Date<=maxDate,newVar]<- round(1/slope,3)
  lpti
}

addrownrs<-function(lpdf,counter="day", sortby="") {
  if (sortby!="") lpdf[order(lpdf[,sortby]),]
  lpdf[,counter]<-as.numeric(row.names(lpdf))
  return(lpdf)
}# two problems: if rownames change to chr, errors occur. if they get out of order because of a sort, the graphs will be a mess. 
addcounter<-function(lpdf=alldata,id="PSCR",counter="day"){
  lpdf[,counter] <- 0 #just to add a column first
  lpdf<- ddply(lpdf,id, function(lpdf){lpdf[,counter]<- lpdf[,counter]<-as.numeric(row.names(lpdf));lpdf} )
  
  return(lpdf)
}

#JHH <-  JHH0 %>%
#          makeGroups( Regiolist= regios)%>% 
#          addPopulation() %>%
#          imputeRecovered2()%>% #correct=TRUE) %>%
#          extravars2() %>%
#          addtotals #(not efficient!)

#*Now plot* note graphit is just here for legacy reasons. it is deprecated. graphit2 will become graphit soon. 
#
#```{r}
graphit <- function(countries=unique(alldata$PSCR), minval=1, ID="PSCR", varname="confirmed",
                    lpdf=alldata, countname="counter", needfuzzy=TRUE,  logy=TRUE,
                    saveit=FALSE, legend=FALSE, size=1){
  countries<- findIDnames(countries,ID,lpdf,needfuzzy)
  lpdf<- addcounter(
    datasel(countries,minval,var=varname,id=ID, lpdf=lpdf, fuzzy=FALSE),
    ID,countname)
  #for (varname in varnames)
  lpdf[lpdf[,varname]==0,varname]<- NA
  myplot<- ggplot(lpdf,aes_string(x=countname,y=varname,color=ID,group=ID)) +  
    geom_line(size=size, alpha=0.2)+geom_point(size=0.7*size,shape=1)+
    geom_dl(aes_string(label = ID) , method = list(dl.trans(x = x + 0.2),
                                                   "last.points", cex = 0.8))+
    ylab(paste(varname, ifelse(logy," (log scale)","")))+
    xlab("day")+
    ggtitle(paste("Covid-19 evolution after the first",minval,varname)) +
    theme_light() + #
    theme(plot.title = element_text(size = 12, face="bold"))
  
  if (length(countries)<12) 
    myplot<- myplot + scale_color_brewer(palette="Spectral",guide = ifelse(legend,"legend",FALSE)) 
  else myplot<- myplot + scale_color_discrete(guide = ifelse(legend,"legend",FALSE))
  if(logy) myplot<- myplot+scale_y_continuous(trans='log2')
  #if (saveit) savePlot(paste("plots/",varname,format(Sys.Date(),format="%Y%m%d")," in ( ", 
  #                     paste(findIDnames(countries,ID,lpdf,needfuzzy),collapse=", " )," )"),
  #                    type= "png")
  return(myplot)
}
#scale_color_brewer(palette="Set3",guide = ifelse(legend,"legend",FALSE)) #Dark2
#geom_text(data = lpdf, aes_string(label = ID,color = ID,x =Inf,y =max(value) ), hjust = -10) 
#
# extravars extract 
if (FALSE){#"pdata.frame" %in% class(lpdf)){
  lpdf$new_confirmed<- base::diff(lpdf$confirmed)
  lpdf$new_deaths<-    base::diff(lpdf$deaths)
  lpdf$new_recovered<- base::diff(lpdf$recovered)
}else{
  warning(paste(quote(lpdf) ,"Not PLM, first date of each ID is going to have wrong 'new_' counts."))
  lpdf$new_confirmed<- diff.lpdf(lpdf$confirmed)
  lpdf$new_deaths<- diff.lpdf(lpdf$deaths)
  lpdf$new_recovered<- diff.lpdf(lpdf$recovered)
  }

#graphit(5) extract: 
  #svg(filename=paste("plots/",mytitle, ifelse(logy,", log scale",""),
  #                  ".svg",sep=""), width=16,height=9) #in inches?
  #print(myplot);dev.off()

#multi lines, without melt, works but has wrong legends, and colors per id only. ! 
graphit3 <- function(countries=NULL, minval=1, id="Country.Region", xvar="day", 
                     yvars=c("confirmed", "recovered"), 
                     lpdf=covid19, fuzzy=TRUE,logx=FALSE, logy=TRUE, savename="", 
                     putlegend=TRUE, size=3,returnid="PSCR"){
  lpdf<- dataprep1(countries,minval,id,xvar,yvars,lpdf,fuzzy,logx,logy,returnid=returnid)
  id<-returnid
  extratext<- paste("by",xvar, "for ", paste(minval,"+ ",yvars[1],sep=""))
  myplot<- ggplot(lpdf) 
  for (varname in yvars) {myplot<- myplot +  
    geom_line(aes_string(x=xvar,y=varname,color=c(id),group=c(id)),alpha=0.2,size=size)+
    geom_point(aes_string(x=xvar,y=varname,color=c(id),group=c(id)),
               size=0.7*size,shape=match(varname,yvars))+
    geom_dl(aes_string(x=xvar,y=varname,color=id,label = id) , 
            method = list(dl.trans(x = x+0.1 ,y=y+0.1),"last.points", cex = 1.2))
  }  
  mytitle<- paste("Covid-19",format(Sys.Date(),format="%Y%m%d"), 
                  savename, paste(yvars,collapse="&"),extratext)
  if (length(countries)<13) {
    myscale<- scale_color_brewer(palette="Paired",guide = ifelse(putlegend,"legend",FALSE)) #"Spectral
  }  else myscale<- scale_color_discrete(guide = ifelse(putlegend,"legend",FALSE))
  myplot<-myplot + 
    ylab(paste(paste(yvars,collapse=", "), ifelse(logy,"(log scale)","")))+
    xlab(paste(xvar,                       ifelse(logx,"(log scale)","")))+ 
    ggtitle(mytitle) +
    theme_light() + theme(plot.title = element_text(size = 20, face="bold")) +
    myscale
  if(logy) myplot<- myplot+scale_y_continuous(trans='log2')
  if(logx) myplot<- myplot+scale_x_continuous(trans='log2')
  if (savename!="") {
    png(filename=paste("plots/",
                       mytitle, ifelse(logy,", log scale",""),
                       ".png",sep=""),
        width=1600,height=900)
    print(myplot);dev.off()
    #svg(filename=paste("plots/",mytitle, ifelse(logy,", log scale",""),
    #                   ".svg",sep=""), width=16,height=9) #in inches?
    #print(myplot);dev.off()
    #print(paste("Plot saved:",mytitle))
  }else return(myplot)
}
writeRegiographs<- function(Regionlist,minval=100,nrs=1:4, id="PSCR",lpdf=covid19, until=Sys.Date()){
  for (i in 1:(length(Regionlist))){
    if (verbose>=2 ) print(paste(Sys.time(),"Region",i,Regionlist[[i]][1]))
    ids<-(findidnames((Regionlist[[i]]),searchid=id,lpdf=lpdf, fuzzy=FALSE,returnid="PSCR"))
    graphs(ids,Regionlist[[i]][1],minval,nrs, id,lpdf,until=until)
  }
}