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

ma <- function(x, n = 7, na.rm = TRUE, ...) { #... = sides=2
  n = min(length(x),n)
  cx <- stats::filter(x, rep(1 / n, n), method = "convolution", ...)
  if ( na.rm) cx <-        ifelse(is.na(cx),x,cx) 
  cx}


mac <- function(x,minval=5, ...){
  cx <- ma(x,...)
  #cx <- ifelse(is.na(cx),x,cx)
  ifelse(abs(cx) < minval, x, cx   )
}

#pacman p_load automates this and the following also: 
#automate the syntax... Thanks Simon https://stackoverflow.com/users/1478381/simon-ohanlon
getPackages <- function(x){
  for (i in x ) {
    #  require returns TRUE invisibly if it was able to load package
    if ( !require( i , character.only = TRUE, quietly = TRUE ) ) {
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}
getPackages <- function(x){
  lapply(x,function(i){
    #  require returns TRUE invisibly if it was able to load package
    if ( !require( i , character.only = TRUE, quietly = TRUE ) ) {
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
              "RColorBrewer","ggthemes", "directlabels", "ggrepel","JuliaCall", "reticulate")) 
  #"plm",#"scales","ggplot2",
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
