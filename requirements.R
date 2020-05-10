`%+%`<- function(x,y){paste(x,y,sep="")}
`%,%`<- function(x,y){paste(x,y,sep=",")}
`%, %`<- function(x,y){paste(x,y,sep=", ")}
`% %`<- function(x,y){paste(x,y,sep=" ")}
`%: %`<- function(x,y){paste(x,y,sep=": ")}
ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}

ma <- function(x, n = 5){
  n=min(length(x),n)
  stats::filter(x, rep(1 / n, n), method="convolution", sides = 2)}

#pacman p_load automates this and the following also: 
#automate the syntax... Thanks Simon https://stackoverflow.com/users/1478381/simon-ohanlon
getpackages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

getpackages (c("reshape2","plyr","ggplot2","RColorBrewer","ggthemes","scales","directlabels", "ggrepel","tidyverse")) #"plm",

# note ggrepel also does labels next to lines. 

#if (!require(devEMF)){ install.packages('devEMF')  require(devEMF)}
#for other formats of saving of plots 

