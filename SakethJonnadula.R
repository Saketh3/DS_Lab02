choco <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/data/choco.csv") 
str(choco) 

# 1. Rating distribution
hist(choco$Rating)
boxplot(choco$Rating)
summary(choco$Rating)

# There are a few outliers at the 1.0 end of the spectrum

# 2. Ratings regarding company
types <- factor(choco$Company)
types <- reorder(types, choco$Rating, FUN=mean, na.rm=TRUE)
str(types)
str(choco$Rating)
str(choco$Rating ~ types)
# Can't hist a formula but can boxplot a formula
# By company
#A bargraph wasnt also working so I used a boxplot
boxplot(choco$Rating ~ types)
table(types)

#3
#The highest ratings come from cocoa percents in the range of 70-80%
boxplot(choco$Rating ~ choco$Cocoa_Percent)

#Takes avg of each rating and shows in table. 
#Doesn't matter how high the cocoa percentage is, because you see some of the highest percentages with the lowest ratings
table1 <- matrix(c(choco$Cocoa_Percent, choco$Rating), ncol=2)
aggregate(choco$Cocoa_Percent ~ choco$Rating, table1, mean)


#4
##types1 <- reorder(choco$Characteristics1, choco$Bean_Origin, FUN = mean)
##hist(choco$Bean_Origin ~ types1)
##boxplot(choco$Bean_Origin ~ choco$Characteristics2)
# Reordering the Bean_Origin so its easier to use in code
types <- reorder(choco$Bean_Origin, choco$Rating, FUN=mean)
boxplot(choco$Rating ~ types)
#The better rating came from Haiti, Peru or Belize

# We can just do this since there is one row per rating
barplot(table(choco$Bean_Origin))#it came from everywhere there is not 1 defining winer
barplot(sort(table(choco$Bean_Origin)))#mostly came from PERU
head(sort(table(choco$Bean_Origin), decreasing=TRUE))#Venezuela and Peru are the top 2



#5 
#The first analysis is if the date (year) has an influence on rating. 
boxplot(choco$Rating ~ choco$Date)
#Looking at the boxplot it shows that as the years go buy the rating havent really changed
#it shows that there havent been that good of improvements over the years
table1 <- matrix(c(choco$Date, choco$Rating), ncol=2)
aggregate(choco$Rating ~ choco$Date , table1, mean)
#it shows that either that no improvements have been made over the years or its just that the chocalate has gotten really consistance.


#The Second analysis is how the rating and location are affected
types1 <- reorder(choco$Location, choco$Rating, FUN=mean)
boxplot(choco$Rating ~ types1)#this graph is all over the place and its really hard ot understand but no other graph would work to show a graph between a string and a int
table1 <- matrix(c(choco$Rating, types1), ncol=2)
aggregate(choco$Rating ~ types1 , table1, mean) #it shows all of the rating for the locations

