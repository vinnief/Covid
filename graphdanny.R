#Danny Dorling Graph

graphdanny <- function(lpti, countries, minVal  = 1, ID  = "PSCR", xvar  = "net_active_imputed", 
                    yvars  = c("active_imputed"), 
                    fuzzy  = FALSE, logx  = FALSE, logy  = FALSE, intercept  = FALSE, slope = FALSE,
                    myFolderDate  = 'random', myFolderType  = "Danny", savename  = "", putlegend = TRUE, 
                    size = 2, returnID  = "PSCR", area  = FALSE, position  = 'stack', facet  = FALSE, 
                    sorted  = FALSE, smoothvars = xvar, smoothn = FALSE, labmeth = 'dl_points', 
                    from = '2019-12-01', to  = Sys.Date()){
  
  lpdf <- as.data.frame(lpti[lpti$theDate >=  from & lpti$theDate <=  to & lpti$confirmed >=  minVal, ])
  lastdate <- max(lpdf$theDate)
  if (typeof(to)  == "character") to = as.Date(to, format = "%Y-%m-%d")
  if (missing(countries)) {
    countries <- unique(lpdf[[returnID]])
    if (length(countries > 40)) return(print('too many countries, you wont see anything. Please select less countries'))
  } else countries <- findIDnames(lpdf, testIDnames = countries, searchID = ID, 
                                  fuzzy = fuzzy, returnID = returnID) #}
  ID <- returnID
  if (verbose >= 8) {print(countries)}
  lpdf <- lpdf[lpdf[[ID]] %in% countries,] 
  if (verbose >= 9) {print('graphi countrydata' );print(head(lpdf))}
  y_lab <- paste(sort(yvars), collapse = " & ") % % ifelse(logy, "(log)", "")
  if (str_length(y_lab) > 70) 
    y_lab <- paste(initials(sort(yvars)), collapse = "&")  % %  
    ifelse(logy, "(log)", "")
  x_lab <- ifelse(xvar=="theDate","date", xvar)
  mytitle <- savename % % y_lab % % "by" % % x_lab % %  "for" % % minVal %#% "+"  % %  "confirmed"
  myFilename <- "C19" % % mytitle
  
  if (nrow(lpdf)  ==  0 ) {return( if (verbose >=  6) {print('graphi'  % %  mytitle  % % " No data")} ) }
  mytitle <- "C19" % % format(min(lpdf$theDate), format  = "%Y-%m-%d")  %#% '-' %#%  
    format(lastdate, format  = "%Y-%m-%d")  % %  mytitle
  lpdf <- lpdf %>% 
           dataprep( ID  = ID, minVal  = minVal, xvar  = xvar, yvars  = yvars,
                   logx  = logx, logy  = logy, sorted  = sorted, smoothvars= smoothvars, smoothn = smoothn)
  if (verbose >= 7) {print('graphi columns left');print( names(lpdf))}
  if (nrow(lpdf)  == 0 | all(is.na(lpdf[, xvar])) | all(is.na(lpdf[, yvars])))
    return(if (verbose >= 2) print('graphi'  % %  
                                     mytitle % % "Too little data to graph. Maybe lower the mininum value, take more territories?"))
  
  lpdf <- lpdf  %>%  #gather(variable, count, -!!ID, -!!xvar)
    melt( id = c(ID, xvar), measure.vars = yvars, 
          variable.name = "variable", value.name = "count") %>% 
    mutate( mygroup = PSCR %, % variable, 
            variable = factor(variable, levels  = yvars)) %>% drop_na()
  if (facet  == 'variable') lpdf$mygroup <- lpdf[[ID]] 
  else if(facet  == ID) lpdf$mygroup <- lpdf$variable
  #switch(facet, "variable" = !!ID, !!ID = variable, PSCR %, % variable)
  
  if (verbose >= 7) {print('graphi summary pdf:');print(summary(lpdf))}
  
  lines_only <- lpdf %>% select(!!ID,mygroup) %>% group_by_at(c(1,2)) %>% 
    filter(n() > 1) 
  lines_only <- lines_only %>% unique()
  #these are the IDs that need a line. geom_path uses this
  lpdf_lines_only <- lpdf[lpdf[[ID]] %in% lines_only[[ID]] & 
                            lpdf$mygroup %in% lines_only$mygroup, ] 
  #if (verbose >= 6) {view(lpdf_lines_only)}
  nrgroups <- length(unique(lpdf$mygroup))
  if (verbose >= 6) print( 'graphi'  % %  xvar % % "from" % %  
                             min(lpdf[, xvar]) % %  "to" % % max(lpdf[, xvar]) %, % 
                             "group by " % %  lpdf$mygroup[1] %, % "facets"  % %  facet)
  nrIDs <- length(unique(lpdf[, ID]))
  myplot <- ggplot(lpdf, 
                   aes_string(y = "count", x = xvar, group = 'mygroup', 
                              color = ifelse(nrIDs  == 1,  'variable' , 
                                             ifelse(facet  == ID, 'variable', ID))
                   ), na.action = na.omit)
  xexpand = 0
  myplot <- myplot + #line plot
      geom_path(data = lpdf_lines_only, alpha = 0.3, size = size*0.7) + 
      geom_point(size = size, aes_string(  shape = 'variable')) 
    if (!putlegend | facet  == FALSE) {
    labmeth = strsplit(labmeth,"_")
    switch(labmeth[1],
           'none' = {},
           'dl' = {myplot = myplot +
                      geom_dl(aes_string(x = xvar, y = "count",  label = 'mygroup'),
                              method  = list(dl.trans(x  = x + 0.1 , y = y + 0.1), "last." %#% 
                                               labmeth[2], cex  = 1.2))
                  xexpand = 0.25} ,
           'repel' = {data = lpdf[lpdf[[xvar]] == max(lpdf[[xvar]]),]
                      myplot = myplot + ifelse(labmeth[2] == "label" , 
                             geom_label_repel(data, aes(label = mygroup), 
                                              label.size = 0.1, box.padding = 0.1, 
                                              label.padding = 0.2, size = 3),
                             geom_text_repel(data, aes(label = mygroup), box.padding = 0.1, size = 3))
                      xexpand = 0.4} ,
           {myplot = myplot + 
             geom_dl(aes_string(x = xvar, y = "count",  label = 'mygroup'),   #default
                     method = list(dl.trans(x  = x + 0.1 , y = y + 0.1), "last.points", cex  = 1.2)) 
            xexpand = 0.65} 
    )}
    
  if ( intercept != FALSE & slope == FALSE ) myplot <- myplot + geom_hline( yintercept  = intercept, na.rm  = TRUE)  
  if (slope != FALSE) myplot <- myplot + geom_abline( intercept  = intercept, slope = slope, na.rm  = TRUE)  
  if ((intercept | slope) != FALSE & (verbose >= 2)) print(intercept % % slope % % "intercept and slope")
    
    if (length(unique(lpdf$variable)) <= 6 ) 
      myplot <- myplot + scale_shape_manual(values  = c(0, 1, 3, 2, 1, 0, 10, 5, 6)) #shape = "\u2620" #bug? 
    if (nrgroups <= 6) {
      myscale_color <- scale_color_manual(values = c("red", "darkgreen", "black",
                                                     "orange", "lawngreen", "tomato"), 
                                          guide = ifelse(putlegend, "legend", FALSE))
    }else if (nrgroups < 13) {
      palette = ifelse(nrgroups < 8, "Dark2", "Paired") 
      myscale_color <- scale_color_brewer(palette = palette)
    } else myscale_color <- scale_color_discrete(guide = ifelse(putlegend, "legend", FALSE))
    myplot <- myplot + myscale_color 

  if (!isFALSE(facet)) {
    myplot <- myplot + facet_wrap(as.formula(paste("~", facet)), strip.position = "bottom")}
  
  myplot <- myplot + ylab(y_lab) +
    xlab( x_lab % % ifelse(logx, "(log scale)","")) + 
    ggtitle(mytitle) + theme_light() +   
    guides(col  = guide_legend(nrow = 30, ncol  = min(2, (nrgroups - 1) %/% 30 + 1))) 
  
  breaks <- breaks_log(n = 5, base = 10) #rep(c( 1, 5), 21)*10^rep((-10:10), each = 2)
  minor_breaks <- rep( 1:5, 21)*(10^rep(-10:10, each = 5))
  if ( logy  !=  FALSE) myplot <- myplot + scale_y_continuous(trans = 'log10', breaks  = breaks, minor_breaks  = minor_breaks, labels = label_number_si()) + annotation_logticks() 
  if (xvar  == "theDate") 
    myplot <- myplot + scale_x_date(labels  = date_format("%d-%m"), expand = c(xexpand, 0)) 
  else
    if (logx) myplot <- myplot + scale_x_continuous(trans = 'log10', breaks  = breaks, minor_breaks  = minor_breaks, expand = c(xexpand, 0))

  ## Code to turn off clipping of the labels outside the plot area. 
  #gt1 <- ggplotGrob(myplot)  
  #gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
  #grid.draw(gt1)
  #or with : coord_cartesian(clip="off")
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
    if (verbose >= 2)print(myPath %//% myFilename %#% ".png" % % length(myPath %//% myFilename %#% ".png"))
    png(filename  = myPath %//% myFilename %#% ".png", width  = 1600, height  = 900)
    print(myplot)
    dev.off()
  }else {
    print(myplot + theme(title  = element_text(size  = 11)))
  }
  invisible(lpdf)
}
