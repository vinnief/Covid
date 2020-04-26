#####Making GIFS
WELCOME!
  Here you will find daily news and tutorials about R, contributed by hundreds of bloggers.
There are many ways to follow us -
  By e-mail:
  Your e-mail here
On Facebook: 
  If you are an R blogger yourself you are invited to add your own R content feed to this site (Non-English R bloggers should add themselves- here)
RSS JOBS FOR R-USERS
Research Lab Coordinator at the University of Iowa
Senior Enterprise Advocate (Sales – New Business)
Major Accounts Executive
Solutions Engineer
Data Scientist Position for Developing Software and Tools in Genomics, Big Data and Precision Medicine
RECENT POSTS
Making Of: A Free API For COVID-19 Data
Facts About Coronavirus Disease 2019 (COVID-19) in 5 Charts created with R and ggplot2
foreach 1.5.0 now available on CRAN
Blogging A to Z: The A to Z of tidyverse
Visualizing decision tree partition and decision boundaries
Updates to R GUIs: BlueSky, jamovi, JASP, & RKWard
Contagiousness of COVID-19 Part I: Improvements of Mathematical Fitting (Guest Post)
What is a dgCMatrix object made of? (sparse matrix format in R)
Close Encounters of the R Kind
COVID-19 in Belgium
Can unbalanced randomization improve power?
  Predicting 1,000,000 of COVID-19 done with R before March 18 – call for help
Analyzing Remote Sensing Data using Image Segmentation
R Tip: How To Look Up Matrix Values Quickly
Why R? Webinars
OTHER SITES
Jobs for R-users
SAS blogs
Animate .gif images in R / ImageMagick
November 21, 2010
By markheckmann

[This article was first published on "R" you ready?, and kindly contributed to R-bloggers]. (You can report issue about the content on this page here)
Want to share your content on R-bloggers? click here if you have a blog, or here if you don't.
Share
Tweet
Yesterday I surfed the web looking for 3D wireframe examples to explain linear models in class. I stumbled across this site where animated 3D wireframe plots are outputted by SAS.  Below I did something similar in R. This post shows the few steps of how to create an animated .gif file using R and ImageMagick. Here I assume that you have ImageMagick installed on your computer. As far as I know it is also possible to produce animated .gif files using R only, e.g. with write.gif() from the caTools package. But using ImageMagick is straighforward, gives you control over the conversion and .gif production and is the free standard program for conversion.

First a simple countdown example. To be sure not to overwrite anything I will create a new folder and set the working directory to the new folder�.

dir.create("examples")
setwd("examples")

# example 1: simple animated countdown from 10 to "GO!".
png(file="example%02d.png", width=200, height=200)
for (i in c(10:1, "G0!")){
plot.new()
text(.5, .5, i, cex = 6)
}
dev.off()

# convert the .png files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
system("convert -delay 80 *.png example_1.gif")
# this works only if you have image magick. 

