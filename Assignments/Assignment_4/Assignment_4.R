library(tidyverse)

#Create a new R script as part of your Assignment 4 R-project. Name it "Assignment_4.R"
# That script should do the following:
  
ITS<-read.delim("./ITS_mapping.csv")

summary(ITS)
plot(ITS)

ggEcobyLat<-ggplot(ITS, aes(x=Ecosystem, y=Lat))+geom_boxplot()
   #Somehow summarize all of the columns and do a bit of additional exploration (play with some functions)
 #Make a boxplot where "Ecosystem"" is on the x-axis and "Lat" is on the y-axis
  #Write code to export this boxplot to a new file in your Assignment_4 directory called "silly_boxplot.png"
      #Hints on below ...


png(filename="silly_boxplot.png")
plot(ggEcobyLat)
dev.off()

