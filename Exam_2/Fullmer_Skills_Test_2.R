library(tidyverse)
library(ggiraphExtra)
library(plotly)

#1) 
salaries<-read.csv("./salaries.csv")

#2)
university<-salaries$UnivName
state<-salaries$State
university_tier<-salaries$Tier
AssistProfSalary<-salaries$AssistProf
AssocProfSalary<-salaries$AssocProf
FullProfSalary<-salaries$FullProf


AssistantProfessorSalaries<-data.frame(university,state,university_tier,"Assistant Professor")

AssistantProfessorSalaries<-mutate(AssistantProfessorSalaries,AssistProfSalary)

AssociateProfessorSalaries<-data.frame(university,state,university_tier,"Associate Professor")

AssociateProfessorSalaries<-mutate(AssociateProfessorSalaries, AssocProfSalary)

FullProfessorSalaries<-data.frame(university,state,university_tier,"Full Professor")

FullProfessorSalaries<-mutate(FullProfessorSalaries,FullProfSalary)

ProfessorSalaries <-data.frame(university,state,university_tier)

names(AssistantProfessorSalaries)

new_names<-c("University","State","Tier","Faculty Rank","Salary")
names(AssistantProfessorSalaries)<-new_names
names(AssociateProfessorSalaries)<-new_names
names(FullProfessorSalaries)<-new_names

CleanSalaries<-rbind(AssistantProfessorSalaries,AssociateProfessorSalaries,FullProfessorSalaries)
#3 & #4)
jpeg("FULLMER_exam2_plot1.jpeg")
salaryplot<-ggplot(CleanSalaries, aes(x=Tier, y=Salary))+geom_boxplot(aes(fill=`Faculty Rank`))+labs(title = "Faculty Salaries - 1995")
print(salaryplot)
dev.off()

#Part Two
#Response Variable - Diversity (discrete & quantitative, the number of species in each air sample, will use log-linear model)
#Explanatory Variables - the other variable=s

library(modelr)
library(broom)
library(dplyr)
library(fitdistrplus)
library(tidyr)
library(ggplot2)
#1)
atmosphere<-read.csv("atmosphere.csv")
#atmosphere1 <- atmosphere[-c(91,71,62,64,260,216,18,108,48),]  #91  71  62  64 260 216  18 108  48
#?data.frame()
#names(atmosphere1)[7] <- "Aerosol"
#names(atmosphere1)

#2&#3a)

ggpairs(data=atmosphere, columns=c(2,3,4,5,7,8,9,10), title="Atmosphere")
#Model 1 - Diversity:Precipitation

mod1 = lm(Diversity~Precip, data=atmosphere)
summary(mod1)
plot(Diversity~Precip, data=atmosphere)
abline(mod1)

#Model 2 - Diversity:Precipitation*Year*Aerosol ( Strongest Model WINNER)
mod2 = lm(Diversity~Precip*Year*Aerosol_Density, data=atmosphere)
summary(mod2)
plot(Diversity~Precip*Year*Aerosol_Density, data=atmosphere)
abline(mod2)

#Model 3 - Diversity:Precip+Aerosol_Density
mod3 = lm(Diversity~Aerosol_Density, data=atmosphere)
summary(mod3) #
plot(Diversity~Precip+Aerosol_Density, data=atmosphere)
abline(mod3)


#3b) Model 2 produces a much narrower spread of residuals, centered around a median (of the smallest magnitude of the three)  -7.52. Furthermore, with an adjusted R-squared at .9823. The model appears to provide the most explanatory power, of the three. Since all p-values are smaller than .05, we reject the null hypothesis that the relationship is due to chance alone. There appears to be a strong relationship between these variables. 

mean(mod1$residuals^2) #44008.48
mean(mod2$residuals^2) #8174.751 ***Again, Model Two provides most explanatory power based upon the significantly smaller mean of residuals that it produces . 
mean(mod3$residuals^2) #295394.6


#4 
predictions<-gather_predictions(atmosphere,mod1,mod2,mod3)


#5) Make a plot showing actual Diversity values, along with the three models' predicted Diversity values. 
#Use color or some other aesthetic to differentiate the actual values and all three predictions (20 points)                                                      

jpeg("./PartII_Question5.jpeg")

ggplot(predictions,aes(x=Precip,color=model)) + geom_point(aes(y=Diversity),color="Blue") + geom_point(aes(y=pred),alpha=.5) + facet_wrap(~model)+
  labs(x=expression("Precipitation (cm"^{3}*")"),y="Diversity (count)")

dev.off()


#6)Write code to show the predicted values of Diversity for each model using the hypothetical new data found in hyp_data.csv (10 points)
hypothetical<-read.csv("hyp_data.csv")

hypothetical<-gather_predictions(hypothetical,mod1,mod2,mod3)

#7)
sink("./model_summaries.txt", append = TRUE,type = c("output"))
summary(mod1)
summary(mod2)
summary(mod3)
sink()



#8. Add these hypothetical predicted values (from hypothetical data - Part II, Step 6) to a plot of actual data and differentiate them by color. (10 bonus points possible for a pretty graph)


names(hypothetical)
names(atmosphere)

hypothetical1<-hypothetical[c(1,3,4,5,6,7,8,10,9)]
names(hypothetical1)[8]<-"Diversity"
names(hypothetical1)

atmosphere1<-atmosphere[c(2,3,4,5,7,8,9,10)]
atmosphere2<-data.frame("Real",atmosphere1)
names(atmosphere2)[1]<-"model"
names(atmosphere2)

both<-rbind(atmosphere2,hypothetical1)



jpeg("./extraCreditEightggplot.jpeg")
ggplot(both,aes(x=Precip, y=Diversity, color=model)) + geom_point()+
  labs(x=expression("Precipitation (cm"^{3}*")"),y="Diversity (count)")+theme_minimal()

dev.off()

#3d pretty
plot_ly(x=both$Precip, y=both$Diversity, z=both$Aerosol_Density, type="scatter3d", mode="markers", color = both$model, opacity=.5)


