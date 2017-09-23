library(ggplot2)
setwd("E:\\Ramesh\\DataScience\\Project\\Practise\\speed-dating-experiment")
dating <- read.csv("Speed Dating Data.csv")
str(dating)
summary(dating)
View(dating)

## Unknown Fields Prob_o,met_o

#Spliting Male and Female Dataset
maleData <- as.data.frame(subset(dating,dating$gender==1))
femaleData <- as.data.frame(subset(dating,dating$gender==0))

#Check column name
names(femaleData)

######### STATED PREFERENCE OF PARTNER BY FEMALE

femaleDataRating <- as.data.frame(subset(femaleData,select=c(pf_o_att,pf_o_sin,pf_o_int,pf_o_fun,pf_o_amb,pf_o_sha)))
str(femaleDataRating)
dim(femaleDataRating)
summary(femaleDataRating)


femaleDataRating <- apply(femaleDataRating,2,convert_count)

convert_count <- function(x) {
  y <- ifelse(is.na(x),0,x)
}

femaleDataRating <- apply(femaleDataRating,2,mean)
femaleDataRating <- round(femaleDataRating)
femaleDataRating
dim(femaleDataRating)


femaleDataRating <- as.data.frame(femaleDataRating)
femaleDataRating$category <- row.names(femaleDataRating)
colnames(femaleDataRating) <- c("Rating","Attribute")


for (i in 1:nrow(femaleDataRating)) {

    if (femaleDataRating$Attribute[i] == "pf_o_att") {
      femaleDataRating$Attribute[i] = "Attractive"
    } else if (femaleDataRating$Attribute[i] == "pf_o_sin") {
      femaleDataRating$Attribute[i] = "Sincere"
    } else if (femaleDataRating$Attribute[i] == "pf_o_int") {
      femaleDataRating$Attribute[i] = "Intelligent "
    } else if (femaleDataRating$Attribute[i] == "pf_o_fun") {
      femaleDataRating$Attribute[i] = "Fun"
    } else if (femaleDataRating$Attribute[i] == "pf_o_amb") {
      femaleDataRating$Attribute[i] = "Ambitious"
    } else if (femaleDataRating$Attribute[i] == "pf_o_sha") {
      femaleDataRating$Attribute[i] = "Shared interests/hobbies"
    }
}

American = ggplot(femaleDataRating) + aes(x="American", y=Rating, fill=Attribute) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(femaleDataRating$Rating) - femaleDataRating$Rating/2

American = American + scale_y_continuous(breaks=y.breaks, labels=femaleDataRating$Attribute) +
  ggtitle("Female Preference") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3","yellow","green","violet"))

American
pie(femaleDataRating$Rating,labels = femaleDataRating$Attribute,col=c("indianred1","deepskyblue","chartreuse3","yellow","green","violet"))

## MOST : Attractive,Intelligent
## LEAST : Ambitious,Shared Int

######### DESIRABLE ATTRIBUTE IN MALE PARTNER(BY FEMALE)

## "attr1_1"  "sinc1_1"  "intel1_1" "fun1_1"   "amb1_1"   "shar1_1" 

femaleDataRating <- as.data.frame(subset(femaleData,select=c(attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1)))
str(femaleDataRating)
dim(femaleDataRating)
summary(femaleDataRating)


femaleDataRating <- apply(femaleDataRating,2,convert_count)

convert_count <- function(x) {
  y <- ifelse(is.na(x),0,x)
}

femaleDataRating <- apply(femaleDataRating,2,mean)
femaleDataRating <- round(femaleDataRating)
femaleDataRating
dim(femaleDataRating)


femaleDataRating <- as.data.frame(femaleDataRating)
femaleDataRating$category <- row.names(femaleDataRating)
colnames(femaleDataRating) <- c("Rating","Attribute")


for (i in 1:nrow(femaleDataRating)) {
  
  if (femaleDataRating$Attribute[i] == "attr1_1") {
    femaleDataRating$Attribute[i] = "Attractive"
  } else if (femaleDataRating$Attribute[i] == "sinc1_1") {
    femaleDataRating$Attribute[i] = "Sincere"
  } else if (femaleDataRating$Attribute[i] == "intel1_1") {
    femaleDataRating$Attribute[i] = "Intelligent "
  } else if (femaleDataRating$Attribute[i] == "fun1_1") {
    femaleDataRating$Attribute[i] = "Fun"
  } else if (femaleDataRating$Attribute[i] == "amb1_1") {
    femaleDataRating$Attribute[i] = "Ambitious"
  } else if (femaleDataRating$Attribute[i] == "shar1_1") {
    femaleDataRating$Attribute[i] = "Shared interests/hobbies"
  }
}



American = ggplot(femaleDataRating) + aes(x="American", y=Rating, fill=Attribute) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(femaleDataRating$Rating) - femaleDataRating$Rating/2

American = American + scale_y_continuous(breaks=y.breaks, labels=femaleDataRating$Attribute) +
  ggtitle("Female Preference") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3","yellow","green","violet"))

American

pie(femaleDataRating$Rating,labels = femaleDataRating$Attribute,col=c("indianred1","deepskyblue","chartreuse3","yellow","green","violet"))

## LEAST : Shared Int,Ambitious

######### DESIRABLE ATTRIBUTE IN FEMALE PARTNER(BY MALE)

## "attr1_1"  "sinc1_1"  "intel1_1" "fun1_1"   "amb1_1"   "shar1_1" 

maleDataRating <- as.data.frame(subset(maleData,select=c(attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1)))

maleDataRating <- apply(maleDataRating,2,convert_count)

convert_count <- function(x) {
  y <- ifelse(is.na(x),0,x)
}

maleDataRating <- apply(maleDataRating,2,mean)
maleDataRating <- round(maleDataRating)

maleDataRating <- as.data.frame(maleDataRating)
maleDataRating$category <- row.names(maleDataRating)
colnames(maleDataRating) <- c("Rating","Attribute")


for (i in 1:nrow(maleDataRating)) {
  
  if (maleDataRating$Attribute[i] == "attr1_1") {
    maleDataRating$Attribute[i] = "Attractive"
  } else if (maleDataRating$Attribute[i] == "sinc1_1") {
    maleDataRating$Attribute[i] = "Sincere"
  } else if (maleDataRating$Attribute[i] == "intel1_1") {
    maleDataRating$Attribute[i] = "Intelligent "
  } else if (maleDataRating$Attribute[i] == "fun1_1") {
    maleDataRating$Attribute[i] = "Fun"
  } else if (maleDataRating$Attribute[i] == "amb1_1") {
    maleDataRating$Attribute[i] = "Ambitious"
  } else if (maleDataRating$Attribute[i] == "shar1_1") {
    maleDataRating$Attribute[i] = "Shared interests/hobbies"
  }
}



American = ggplot(maleDataRating) + aes(x="American", y=Rating, fill=Attribute) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(maleDataRating$Rating) - maleDataRating$Rating/2

American = American + scale_y_continuous(breaks=y.breaks, labels=maleDataRating$Attribute) +
  ggtitle("Male Preference") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3","yellow","green","violet"))

American

pie(maleDataRating$Rating,labels = maleDataRating$Attribute,col=c("indianred1","deepskyblue","chartreuse3","yellow","green","violet"))

## LEAST : Ambitious,Shared Int

##########################
# How important do people think attractiveness is in potential mate selection
#vs. its real impact?

# To use attr1_1 and attr1_7

names(dating)

DataRating <- as.data.frame(subset(dating,select=c(attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1)))
DataRating <- apply(DataRating,2,convert_count)

convert_count <- function(x) {
  y <- ifelse(is.na(x),0,x)
}

DataRating <- apply(DataRating,2,mean)
DataRating <- round(DataRating)
DataRating <- as.data.frame(DataRating)
DataRating$category <- row.names(DataRating)
colnames(DataRating) <- c("Rating","Attribute")


for (i in 1:nrow(DataRating)) {
  
  if (DataRating$Attribute[i] == "attr1_1") {
    DataRating$Attribute[i] = "Attractive"
  } else if (DataRating$Attribute[i] == "sinc1_1") {
    DataRating$Attribute[i] = "Sincere"
  } else if (DataRating$Attribute[i] == "intel1_1") {
    DataRating$Attribute[i] = "Intelligent "
  } else if (DataRating$Attribute[i] == "fun1_1") {
    DataRating$Attribute[i] = "Fun"
  } else if (DataRating$Attribute[i] == "amb1_1") {
    DataRating$Attribute[i] = "Ambitious"
  } else if (DataRating$Attribute[i] == "shar1_1") {
    DataRating$Attribute[i] = "Shared interests/hobbies"
  }
}


pie(DataRating$Rating,labels = DataRating$Attribute,col=c("indianred1","deepskyblue","chartreuse3","yellow","green","violet"))



mean(dating$attr1_1,na.rm = TRUE)
mean(dating$attr7_2,na.rm = TRUE)
mean(dating$attr7_3,na.rm = TRUE)

#Attraciveness is more important in real than expected

##########################
# Are shared interests more important than a shared racial background?

# imprace

DataRating <- as.data.frame(subset(dating,select=c(shar_o,imprace)))
DataRating <- apply(DataRating,2,convert_count)

convert_count <- function(x) {
  y <- ifelse(is.na(x),0,x)
}

DataRating <- apply(DataRating,2,mean)
DataRating <- round(DataRating)
DataRating <- as.data.frame(DataRating)

DataRating$category <- row.names(DataRating)
colnames(DataRating) <- c("Rating","Attribute")

pie(DataRating$Rating,labels = DataRating$Attribute,col=c("indianred1","deepskyblue"))

#Shared int is more important than racial background

##########################
# Can people accurately predict their own perceived value in the dating
# market?

# To use attr3 and attr5

##########################
# In terms of getting a second date, is it better to be someone's first speed date
# of the night or their last?

#####################************************************************************************************


#mean(testDF$v2,na.rm = TRUE)
#rm(list=ls())




