source("requirements.R")
source('definitions.R')

#traceback() # to trace the nested calls leading to an error. 
#suppressWarnings() to suppresswarnings of the functioninside. does not stop the warning: "geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?"
options(warn = 2)
verbose <- 7
verbose <-  1

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
  if (verbose >= 7) {print(countries)}
  lpdf <- lpdf[lpdf[[ID]] %in% countries,] #PSCR
  if (verbose >= 9) {print('graphit countrydata' );print(head(lpdf))}
  y_lab <- paste(sort(yvars), collapse = " & ") % % ifelse(logy, "(log)", "")
  if (str_length(y_lab) > 80) 
    y_lab <- paste(initials(sort(yvars)), collapse = "&")  % %  
    ifelse(logy, "(log)", "")
  mytitle <- savename % % y_lab % % "by" % % xvar % %  "for" % % minVal %#% "+"  % %  "confirmed"
  myFilename <- "C19" % % mytitle
  
  if (nrow(lpdf)  ==  0 ) {return( if (verbose >=  4) {print('graphit'  % %  mytitle  % % " No data")} ) }
  mytitle <- "C19" % % format(min(lpdf$Date), format  = "%Y-%m-%d")  % % '-' % %  
    format(lastdate, format  = "%Y-%m-%d")  % %  mytitle
  
  lpdf <- dataprep(lpdf, ID  = ID, minVal  = minVal, xvar  = xvar, yvars  = yvars,
                   logx  = logx, logy  = logy, sorted  = sorted)
  if (verbose >= 5) {print('graphit columns left');print( names(lpdf))}
  if (nrow(lpdf)  == 0 | all(is.na(lpdf[, xvar])) | all(is.na(lpdf[, yvars])))
    return(if (verbose >= 4) print('graphit'  % %  paste(mytitle, "Too little data to graph. Maybe lower the mininum value, take more territories?")))
  
  lpdf <- lpdf  %>%  
    melt(lpdf , id = c(ID, xvar), measure.vars = yvars, 
         variable.name = "variable", value.name = "count") %>% 
    mutate( mygroup = PSCR %, % variable, #!!ID? you would get zigzags if ID <> PSCR and you havent totalled. 
            variable = factor(variable, levels  = yvars)) %>% drop_na()
  if (verbose >= 7) {print('graphit summary pdf:');print(summary(lpdf))}
  
  if (facet  == 'variable') lpdf$mygroup <- lpdf[[ID]] else 
    if (facet  == ID) lpdf$mygroup <- lpdf$variable
  lines_only <- lpdf %>% select(!!ID,mygroup) %>% group_by_at(c(1,2)) %>% 
    filter(n() > 1) 
  if (verbose >= 5) {view(lines_only)}
  lines_only <- lines_only %>% unique()
  if (verbose >= 5) {view('country-lines with 2+ datapoints:') ; print(lines_only)}
  
  lpdf_lines_only <- lpdf[lpdf[[ID]] %in% lines_only[[ID]] & 
                            lpdf$mygroup %in% lines_only$mygroup, ] 
  if (verbose >= 5) {view(lpdf_lines_only)}
  nrgroups <- length(unique(lpdf$mygroup))
  if (verbose >= 5) print( 'graphit'  % %  xvar % % "from" % %  
                             min(lpdf[, xvar]) % %  "to" % % max(lpdf[, xvar]) %, % 
                             "group by " % %  lpdf$mygroup[1] %, % "facets"  % %  facet)
  nrIDs <- length(unique(lpdf[, ID]))
  myplot <- ggplot(lpdf, 
                   aes_string(y = "count", x = xvar, group = 'mygroup', 
                              color = ifelse(nrIDs  == 1,  'variable' , 
                                             ifelse(facet  == ID, 'variable', ID))
                   ), na.action = na.omit)
  
  if (area) {posalpha <- ifelse(position  == 'identity', 0.4, 1)
  myplot <- myplot + geom_area(aes_string(
    color = ifelse(nrIDs  == 1 | facet  == ID, 'variable' , 'mygroup'), 
    fill = ifelse(nrIDs  == 1 | facet  == ID,  'variable' , 'mygroup')), 
    position  = position, alpha = posalpha)
  myscale_fill <- scale_fill_manual(values  = c("red", "green", "black", "darkorange", "lawngreen"))
  if (nrgroups <= 2) myscale_fill <- scale_fill_manual(values  = c("lawngreen", "cyan"))#, "black", "darkorange", "lawngreen"))
  myplot <- myplot + myscale_fill + 
    scale_color_manual(values  = c("red", "green", "black", "darkorange", "lawngreen"))
  } else {
    myplot <- myplot + #line plot
      geom_line(data = lpdf_lines_only, alpha = 0.3, size = size*0.7) +
      geom_point(size = size, aes_string(  shape = 'variable')) +
      if (!putlegend | facet  == FALSE) 
        geom_dl(aes_string(x = xvar, y = "count",  label = 'mygroup'),    
                method  = list(dl.trans(x  = x + 0.1 , y = y + 0.1), "last.points", 
                               cex  = 1.2)) 
    if ( intercept | slope ) myplot <- myplot + geom_abline( intercept  = 1*intercept, slope = 1*slope, na.rm  = TRUE) #bug here or somewhere: the line is at 1.? instead of at 0.24
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
    if (verbose >=  4) print("graphit making plot"  % %  myFolderType %#% "/" %#% mytitle)
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




#check addSimVars
lpti <- ECDC[ECDC$PSCR %in% c("Cases_on_an_international_conveyance_Japan", "Cayman_Islands", "Central_African_Republic"),]
minVal <- 100; minDate = '2019-12-31'; maxDate = Sys.Date()

 for (country in countries <- unique(lpti$PSCR)) {
   
   if (any(is.na(lpti[lpti$PSCR == country ,c('confirmed','Date')]))) { 
     print(country % % 'nrows' % % nrRows % % newnrRows) 
     print(which((is.na(lpti[lpti$PSCR == country ,c('confirmed','Date')]))))}
 }
country <- 'Cayman_Islands' 
print(which((is.na(lpti[lpti$PSCR == country ,c('confirmed','Date')]))))
lpti[lpti$PSCR == country & lpti$confirmed >= minVal & lpti$Date >= minDate & lpti$Date <=  maxDate,] %>% view

lpti %>% view
lpti %>% addSimVars(minVal = 100) #error. 
lpti <- ECDC[ECDC$PSCR %in% c("Cases_on_an_international_conveyance_Japan", "Cayman_Islands"),]
lpti %>% addSimVars(minVal = 100) #no error. 
lpti[lpti$confirmed_doublingDays < 0, ] %>% view #39 totally NA rows 
lpti[is.na(lpti$confirmed_doublingDays), ] %>% view # 3172 rows
ECDC[is.na(ECDC$PSCR),] %>% view()   #0 rows. 
E10 <- ECDC %>% addSimVars(minVal = 100) #error with Cayman_Islands
#

lpti <- ECDC[ECDC$PSCR == 'Europe',]
lpti %>% addSimVars(minDate = Sys.Date() - 10, ext = "_endsim2") %>% view #one row less. 
options(warn = 2)
E10 <- ECDC %>% addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") # europe -1 extra row generated this is ok. it was because Spain delivers data later. 
# 2020-07-05 data:  UK has 7 out of 11 rows. 2020-07-03: 10 out of 11
#
## also, NAs produced by rpois (42 times) and 
# In `[<-.data.frame`(`*tmp*`, lpti$PSCR == country, , value = structure(list( ... :
#provided 51 variables to replace 47 variables


# integrate testing into ECDC & JHH
a <- merge(ECDC, testing, by.x = c('ISOcode','Date'), by.y = c('ISOcode','Date'), all.x = TRUE,
           sort = FALSE)
names(testing)

#make gifs
#from loaddata: trial to make the loading only happen if it gives new data
#while((Sys.time()>Sys.Date()% % "22:00:00")| max(JHH$Date)<Sys.Date()-1 ) {
#   source("loaddata.R")
#   if (max(JHH$Date)< Sys.Date()-1)     {
#    print( "failed to get yesterday's values ") 
#   for (i in 1:12){
#    print(Sys.time())
#   Sys.sleep(600)
#}   } }



#12 ggplot extensions: ggcorplot, 
#https://mode.com/blog/r-ggplot-extension-packages
