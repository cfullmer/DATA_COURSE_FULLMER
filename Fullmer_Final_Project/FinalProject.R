library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

##what drugs are invlived with drug deaths by year? 
  

##is the problem centralized to a particulr area? 
  

#what are the demographics of the individuals who die by year



drugDeaths<-read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")
CleanOther<-read.csv("cleanData.csv", header = TRUE,blank.lines.skip = FALSE)






for (i in 1:13){
CleanOther<-add_row(CleanOther)
i+1}



drugDeaths$Date<-as.POSIXct(drugDeaths$Date,tz="",format="%m/%d/%Y %H:%M:%S")




#remove columns we dont need/large unneseary objects
drugDeaths<-select(drugDeaths, -Other)
drugDeaths<-select(drugDeaths, -DateType)
drugDeaths<-select(drugDeaths, -Location)
drugDeaths<-select(drugDeaths, -LocationifOther)
drugDeaths<-select(drugDeaths, -DescriptionofInjury)
drugDeaths<-select(drugDeaths, -InjuryPlace)
drugDeaths<-select(drugDeaths, -COD)
drugDeaths<-select(drugDeaths, -OtherSignifican)
drugDeaths<-select(drugDeaths, -MannerofDeath)
drugDeaths<-select(drugDeaths, -AnyOpioid)




substances_y<-names((drugDeaths[,c(14:28)]))

substances_y

#why cant i replace above with the variable names? i.e heropin & hydromorphone


for(i in substances_y){
  drugDeaths[i] <- ifelse(drugDeaths[i] == "Y", yes=as.numeric(1), no=as.numeric(0))
}


for (varname in substances_y){
  drugDeaths[varname] <- ifelse(drugDeaths[[varname]]== 1, yes=glue::glue("{varname}"), no=NA)
}

#split Cllean.Other


drugDeaths<-add_column(drugDeaths, CleanOther$Other, .after="OpiateNOS")

colnames(drugDeaths)[colnames(drugDeaths)=="CleanOther$Other"] <- "Other"
?add_column()

#scrubadub
drugDeaths$Other <- as.character(drugDeaths$Other)
drugDeaths$Other[drugDeaths$Other == "Hydromorph"] <- "Hydromorphone"



drugDeaths<-separate(drugDeaths, Other, into=c("Other_1","Other_2") ,sep = ",", remove = TRUE,
                     convert = FALSE, extra = "merge", fill = "right")

drugDeaths$Other_1 <- as.character(drugDeaths$Other_1)
drugDeaths$Other_1[drugDeaths$Other_1 == ""] <- NA






## For accurate count of drugs per death (run below with adjusted column subset) 

##drugDeaths$Other <- ifelse(drugDeaths$Other == "", yes=as.numeric(0), no=as.numeric(1))
##drugDeaths<-mutate(drugDeaths, substance_count=rowSums(drugDeaths[21:35])) 



##how many drugs were associated with each death by year? 

#convert to posix format we can work with the years

drugDeaths$Date<-as.POSIXlt(drugDeaths$Date)

drugDeaths$day <- drugDeaths$Date$mday       
drugDeaths$month <- drugDeaths$Date$mon + 1   
drugDeaths$year <- drugDeaths$Date$year + 1900

drugDeaths$Date<-as.POSIXct(drugDeaths$Date)



##lengthen

length(drugDeaths$oth)

names(drugDeaths)[14:30]<- paste(".", colnames(drugDeaths[14:30]), sep = "")
longertest<-pivot_longer(drugDeaths, cols=starts_with("."), names_to = "Substance", values_to = "Substance", names_prefix = ".",names_repair = "unique",  values_ptypes = list(val = 'character'),values_drop_na = TRUE)

longertest<-longertest%>%select(-Substance...20)

names(longertest)[20]<-"Substance"





for (i in 14:30){
  drugDeaths[i] <- ifelse(is.na(drugDeaths[i]), yes=as.numeric(0), no=as.numeric(1))
}

drugDeaths<-mutate(drugDeaths, substance_count=rowSums(drugDeaths[14:30])) 

drugDeaths$substance_count<-as.factor(drugDeaths$substance_count)

ggplot(drugDeaths)+geom_bar(aes(x=substance_count,fill=substance_count))+ylab(label ="number of deaths")+xlab(label="number of substances")+facet_wrap(~year)


