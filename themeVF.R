pal_VF <- c("red", "green","black","darkorange","lawngreen")
pal_VF3<-   c("#d695ef", "#ff7cd8",  "#ff42c1",	"#6696fd", 	"#6b41e8")

# http://www.color-hex.com/color-palette/12376
pal_VF2 <- c("#ffd9d9", "#d4a8a8",  "#fd9fe1",	"#d0b3cc", 	"#ffeeee")


############# color pieces!
my_scale_fill <- function(){
  structure(list(    scale_fill_manual(values=pal_VF)   ))
}

my_scale_color_discrete <- function(){
  structure(list(   scale_color_manual(values=pal_VF)   ))
}

my_scale_color_continuous <- function(){
  structure(list(     scale_color_gradientn(colours = pal_VF)   ))
}
#scale_shape_manual(values = c(0,1,3,2,10,5,6)) #shape="\u2620"
my_scale_shape_manual <- function(){
  structure(list( 'a','r','c',"\u2620",'r','u'))
}
##################################

theme_VF <- function(base_size=12, font=NA){
  txt <- element_text(size = base_size + 2, colour = "black", face = "plain")
  bold_txt <- element_text(size = base_size+2, colour = "black", face = "bold")
  
  theme_classic(base_size = base_size, base_family = font) +
    theme(
      ###### clean up!
      #legend.key = element_blank(), 
      #strip.background = element_blank(), 
      ########### text basics
      text = txt, 
      plot.title = txt, 
      
      axis.title = txt, 
      axis.text = txt, 
      
      legend.title = bold_txt, 
      legend.text = txt ) +
    
    ############## AXIS lines
    theme(
      axis.line.y = element_line(colour = "pink", size = 1, linetype = "dashed"),
      axis.line.x = element_line(colour = "pink", size = 1.2,linetype = "dashed"),
      #### remove Tick marks
      #axis.ticks=element_blank(),
      
      ### legend  top and no title!
      #legend.position = "top", 
      #legend.title = element_blank(),
      legend.key = element_rect(fill = "lightskyblue1", color = "lightskyblue1"),
      legend.background = element_rect( fill = "lightskyblue1",color = "pink", size = 0.5,linetype = "longdash"),
      
      ## background
      plot.background = element_rect(fill = "white",colour = "pink",size = 0.5, linetype = "longdash")
    )
}