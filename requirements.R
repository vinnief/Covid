
#after an update of R: or regularly
# copy ~\documents\ R\win-library\4.0 to R\win-library\4.1 if major upgrade
#update.packages(checkBuilt=TRUE, ask=FALSE)

#`%+%`<- function(x,y) {paste(x,y,sep = "")}
`%#%` <- function(x,y) {paste(x,y,sep = "")}
`%-%` <- function(x,y) {paste(x,y,sep = "-")}
`%_%` <- function(x,y) {paste(x,y,sep = "_")}
`%,%` <- function(x,y) {paste(x,y,sep = ",")}
`%, %` <- function(x,y) {paste(x,y,sep = ", ")}
`% %`  <- function(x,y) {paste(x,y,sep = " ")}
`%: %` <- function(x,y) {paste(x,y,sep = ": ")}
`%//%` <- function(a,b) {paste(a,b,sep = "/")}
# note %/% is remainder of division modulo!!!
options(encoding = "UTF-8")

ma <- function(x, n = 7, na.rm = TRUE, sides = 1, digits = 1) { # can be  sides = 2
  n = min(length(x),n)
  cx <- round(stats::filter(x, rep(1 / n, n), method = "convolution", sides = sides),
              digits) #default is sides = 2 and thats wrong mostly. so we invert the default. 
  if (na.rm) cx <-   ifelse(is.na(cx),x,cx) # is there a way to have partial moving averages for the first n-1? 
  cx
}


mac <- function(x,minval=5, ...){
  cx <- ma(x,...)
  #cx <- ifelse(is.na(cx),x,cx)
  ifelse(abs(cx) < minval, x, cx   )
}

#pacman p_load automates this and the following also: 
#automate the syntax... Thanks Simon https://stackoverflow.com/users/1478381/simon-ohanlon
getPackages <- function(x){
  logFileName = "packageloading.log"
  theLog = file(logFileName, open = "wt")
  sink(theLog, type = "message")# , append = T)
  on.exit({
    message(class(theLog) ," is the Class of the Log ", logFileName,# At this moment i should close the message sink. but that gives an error. 
            "\nSink.number = ",
            sink.number( type = "message"))
    sink( type = "message"); 
    if (isOpen(theLog)) close(theLog)
    } 
    )
  lapply(x,function(i){
    #  require returns TRUE invisibly if it was able to load package
    if ( !require( i , character.only = TRUE, quietly = !verbose ) ) {
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  })
}
options(tidyverse.quiet = TRUE)
#suppressPackageStartupMessages({
getPackages(c("lubridate",'rvest',"plyr","lubridate", "scales", "reshape2", "tidyverse",'profvis',
              "RColorBrewer","ggthemes", "directlabels", "ggrepel","JuliaCall", "reticulate",
              "naniar", "tsibble")) 
  #"xts", "plm","scales","ggplot2",
#})
options(dplyr.summarise.inform = FALSE)

# if (!require(devEMF)){ install.packages('devEMF')  require(devEMF)}
# for other formats of saving of plots 

mutate_cond <- function(.data, condition, ..., envir  = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
#install.packages("JuliaCall")
library(JuliaCall)
#julia <- julia_setup(JULIA_HOME = "C:/Users/feltkamp/AppData/Local/Programs/Julia/Julia 1.5.0/bin")
