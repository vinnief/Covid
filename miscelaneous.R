source("requirements.R")
source("definitions2.R")


devtools::install_github("dgrtwo/gganimate")# and install image_magick. 
# see http://www.ggtern.com/2017/07/23/version-2-2-1-released/
# add  frame = Date  to aes of ggplot, then
# gganimate(myplot, "filename.gif" )  # or mp4, html, swf. 
# p3 <- ggplot(gapminder, aes(gdpPercap, lifeExp, frame = year)) +
#geom_path(aes(cumulative = TRUE, group = country)) +
#  scale_x_log10() +
#  facet_wrap(~continent)
#12 ggplot extensions: ggcorplot, 
#https://mode.com/blog/r-ggplot-extension-packages
