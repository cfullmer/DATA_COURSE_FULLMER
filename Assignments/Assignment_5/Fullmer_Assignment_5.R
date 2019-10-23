library(tidyverse)

#Load the "iris" data set
data("iris")

str(iris)

jpeg("./One")
ggplot(iris, aes(x=iris$Sepal.Length, y=iris$Petal.Length, color=Species))+geom_point()+geom_smooth(method="lm",aes(color=Species))+labs(x="Sepal.Length", y= "Petal.Length", title="Sepal length vs petal length", subtitle = "for three iris species")
dev.off()

jpeg("./Two")
ggplot(iris, aes(x=Petal.Width))+geom_density(aes(fill=Species, alpha=.5, )) + theme_minimal() + labs(title="Distribution of petal width", subtitle = "for three iris species")
dev.off()


# Duplicate the following 3 figures (on next 3 pages) and save them in your Assignment_5 directory as "iris_fig1.png", "iris_fig2.png", "iris_fig3.png", respectively.
# Keep in mind that by default, I make most of my figures with theme_minimal() 
#Read through the different plot types on this website and use the info to reproduce the fourth figure below. Save it as "iris_fig4.png"




