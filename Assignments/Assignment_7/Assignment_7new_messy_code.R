# Assignment 6 messy code
# Change this to "tidy" format using dplyr verbs

# There's an intuitive dplyr version for everything you see here.

# Note: Do not erase the original code, just comment it out and put your own equivalent code below each section
# i.e., change each line of indicated code to a tidy version that does the same thing.


library(tidyverse)

##########################
#        Part 1          #
##########################

# load data (wide format)
#utah = read.csv("Data/Utah_Religions_by_County.csv")
utah=read.csv("./Utah_Religions_by_County.csv")

names(utah)
utah_long = gather(utah,key = Religion, value = Proportion, c(5:17))


# subset to only counties with buddhists observed
#buddhist = utah[utah$Buddhism.Mahayana > 0,]


buddhist<- utah_long%>%filter(Religion=="Buddhism.Mahayana",Proportion>0)


# order rows by population (descending)
#buddhist = buddhist[order(buddhist$Pop_2010, decreasing = TRUE),]

buddhist<-buddhist%>%arrange(desc(Pop_2010))


# write this new dataframe to a file
write.csv(buddhist, file = "./buddhist_counties.csv", row.names = FALSE, quote = FALSE)

## get group summaries of religiousity based on population ##

# divide each county into one of six groups based on populations
# note: keep these two lines the same in your updated code!
groups = kmeans(utah$Pop_2010,6) # clusters data into 6 groups based on proximity to mean of potential groups
utah$Pop.Group = groups$cluster # assigns a new variable to utah giving group for each county

utah_long$Pop.Group =groups$cluster

# subset to each group and find summary stats on Religiosity for each
#group1a = mean(utah[utah$Pop.Group == 1,]$Religious)
#take the mean of the ratios of overall religiosity by the poulation group

#group2 = mean(utah[utah$Pop.Group == 2,]$Religious)
#group3 = mean(utah[utah$Pop.Group == 3,]$Religious)
#group4 = mean(utah[utah$Pop.Group == 4,]$Religious)
#group5 = mean(utah[utah$Pop.Group == 5,]$Religious)
#group6 = mean(utah[utah$Pop.Group == 6,]$Religious)

group1<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==1)%>%summarise(mean(Religious))
group2<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==2)%>%summarise(mean(Religious))
group3<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==3)%>%summarise(mean(Religious))
group4<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==4)%>%summarise(mean(Religious))
group5<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==5)%>%summarise(mean(Religious))
group6<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==6)%>%summarise(mean(Religious))

# same, but mean population
#group1.pop = mean(utah[utah$Pop.Group == 1,]$Pop_2010)
#group2.pop = mean(utah[utah$Pop.Group == 2,]$Pop_2010)
#group3.pop = mean(utah[utah$Pop.Group == 3,]$Pop_2010)
#group4.pop = mean(utah[utah$Pop.Group == 4,]$Pop_2010)
#group5.pop = mean(utah[utah$Pop.Group == 5,]$Pop_2010)
#group6.pop = mean(utah[utah$Pop.Group == 6,]$Pop_2010)

group1.pop<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==1)%>%summarise(mean(Pop_2010))
group2.pop<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==2)%>%summarise(mean(Pop_2010))
group3.pop<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==3)%>%summarise(mean(Pop_2010))
group4.pop<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==4)%>%summarise(mean(Pop_2010))
group5.pop<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==5)%>%summarise(mean(Pop_2010))
group6.pop<-utah_long%>%group_by(Pop.Group)%>%filter(Pop.Group==6)%>%summarise(mean(Pop_2010))


# make data frame of each group and mean religiosity
#religiosity = data.frame(Pop.Group = c("group1","group2","group3","group4","group5","group6"),
           #Mean.Religiosity = c(group1,group2,group3,group4,group5,group6),
        # Mean.Pop = c(group1.pop,group2.pop,group3.pop,group4.pop,group5.pop,group6.pop))



religiositytidy<-rbind(group1, group2, group3, group4, group5, group6)

poptidy <- rbind(group1.pop, group2.pop, group3.pop, group4.pop, group5.pop, group6.pop)

meanReligiosityAndPopGrouped<-cbind(religiositytidy,poptidy[-c(1)])


#religiosity # take quick look at resulting table

meanReligiosityAndPopGrouped

names(meanReligiosityAndPopGrouped)<-c("Group","Religious (mean)", "Population (2010)")

# order by decreasing population
#religiosity = religiosity[order(religiosity$Mean.Pop, decreasing = TRUE),]
meanReligiosityAndPopGrouped <- meanReligiosityAndPopGrouped%>%arrange(desc(`Population (2010)`))

#religiosity # take quick look at resulting table

meanReligiosityAndPopGrouped

#####################################
#              Part 2               #
# Beginning to look at correlations #
# run this code without changing it #
#####################################

# Look for correlations between certain religious groups and non-religious people
religions = names(utah)[-c(1:4)]

for(i in religions){
  rsq = signif(summary(lm(utah[,i] ~ utah$Non.Religious))$r.squared, 4)
  plot(utah[,i] ~ utah$Non.Religious, main = paste(i,"RSq.Val=",rsq), xlab = "Non_Religious",ylab=i)
  abline(lm(utah[,i] ~ utah$Non.Religious), col="Red")
}

# Browse through those plots and answer the following questions:
# 1.  Which religious group correlates most strongly in a given area with the proportion of non-religious people?
#The  LDS religion correlates most strongly with the proportion of non-religious people, with an R-squared value of 0.7565
# 2.  What is the direction of that correlation?
#There is a negative correlation between the amount of LDS people and the ratio of Non_Religious People. Which means, the more LDS people there are, the smaller the ratio of non-religious people. 
# 3.  Which religious group has the second stronglest correlation, as above?
#The second strongest correlation is between the amout of members of the Episcopal Church and the ratio of non-religious people; the R-squared value is 0.3557.
# 4.  What is the direction of THAT correlation?
#In contrast to the LDS church, in Utah, there is a positive relationship beween the amount of Episcopalians and the number of non-religious people. More episcopalians means more non religious people. 
# 5.  What can you say about these relationships?
#Episcopalians tend to live in places that are less religious overall, and mormons tend to live in places that are more relgious overall. 

# UPLOAD YOUR ANSWERS TO CANVAS
# DON'T FORGET TO PUSH YOUR TIDY CODE TO GITHUB AS WELL!
