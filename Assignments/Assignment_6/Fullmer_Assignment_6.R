library(tidyverse)
##loads the mtcars data set
{r, echo=TRUE}
data("mtcars")
str(mtcars)

## subsets the mtcars dataframe to include only **automatic transmissions**
mtcarsAutomatic<-mtcars %>% filter(am==0)
##  saves this new subset as a new file called "automatic_mtcars.csv" in your Assignment_5 directory
write.csv(mtcarsAutomatic,"./automatic_mtcars.csv")
##plots the effect of horsepower on miles-per-gallon (update plot to have meaningful labels and title)
## saves this plot as a png image called "mpg_vs_hp_auto.png" in your Assignment_5 directory
png("./mpg_vs_hp_auto.png")
ggplot(mtcars, aes(x=hp,y=mpg))+geom_point()+geom_smooth(method = "lm")+labs(x="Gross Horsepower",y="Miles/(US) gallon", title="Horsepower v Miles Per Gallon")
dev.off()
## plots the effect of weight on miles-per-gallon (with improved labels, again)
##  saves this second plot as a **tiff** image called "mpg_vs_wt_auto.tiff" in your Assignment_5 directory
tiff("./mpg_vs_wt_auto.tiff")
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()+geom_smooth(method = "lm")+labs(x="Weight (1000 lbs)",y="Miles/(US) gallon", title="Weight v Miles Per Gallon")
dev.off()
## subsets the original mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.
smallerDisplacement<-mtcars%>%filter(disp<=200)
## saves that new subset as a csv file called mtcars_max200_displ.csv
write.csv(smallerDisplacement,"./mtcars_max200_displ.csv")
## includes code to calculate the maximum horsepower for each of the three dataframes (original, automatic, max200)
## prints these calculations (from task 10) in a readable format to a new plaintext file called hp_maximums.txt
]
hp_maximums<-data.frame(automatic,max200,original)

original<-max(mtcars$hp)
automatic<-max(mtcarsAutomatic$hp)
max200<-max(smallerDisplacement$hp)

sink("hp_maximums.txt")
print(hp_maximums)
sink()
