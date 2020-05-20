`%+%`<- function(x,y){paste(x,y,sep="")}
`%,%`<- function(x,y){paste(x,y,sep=",")}
`%, %`<- function(x,y){paste(x,y,sep=", ")}
`% %`<- function(x,y){paste(x,y,sep=" ")}
`%: %`<- function(x,y){paste(x,y,sep=": ")}
`%//%` <- function(a,b){paste(a,b,sep="/")}
# note %/% is remainder!!!

ma <- function(x, n = 5,na.rm=TRUE,...){ #... = sides=2
  n=min(length(x),n)
  cx<- stats::filter(x, rep(1 / n, n), method="convolution", ...)
  if( na.rm)cx<-        ifelse(is.na(cx),x,cx) 
  cx}


mac <- function(x,minval=40, ...){
  cx<- ma(x,...)
  #cx<- ifelse(is.na(cx),x,cx)
  ifelse(cx<minval, x, cx   )
}
#test if mac does not reduce the amounts too much (after all we only average above a threshhold)
rs<- function(n=200,s=50) {
  x<- rnorm(100,n,s)
  round(rowSums( rbind(x,
      mac.1=mac.(x,sides=1),ma1=ma(x,sides=1),
      mac.2=mac.(x,sides=2),mac.=mac.(x),ma=ma(x)))
      /sum(x)*100,2)
}

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

