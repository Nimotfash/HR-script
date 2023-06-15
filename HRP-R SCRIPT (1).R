#Import library 

library(tidyverse)

# Check data types: 

str(hs)

# Check for missing values: 

colSums(is.na(hs))

#Drop missing values 

 hs <-na.omit(hs)

# check to make sure that the rows have been removed 

colSums(is.na(hs))

#Check for duplicates 
#hs <- hs[!distinct(hs), ]
#dim(hs[duplicated(hs$Film),])

hs <- hs[!duplicated(hs$Film), ]
dim(hs)



#round off values to 2 places 

hs$Profitability <- round(hs$Profitability ,digit=2)

hrp$Worldwide.Gross <- round(hrp$Worldwide.Gross ,digit=2)

#View(hs)

dim(hs)

#Check for outliers using a boxplot 

library(ggplot2)

#Create a boxplot that highlights the outliers   
ggplot(hs,aes(x=MSSubClass , y= SalePrice )) +geom_boxplot(outlier.colour= "red",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))



#Remove outliers in 'Profitability' 
Q1 <- quantile(hrp$Profitability, .25)
Q3 <- quantile(hrp$Profitability, .75)
IQR <- IQR(hrp$Profitability)

no_outliers <- subset(hrp, hrp$Profitability> (Q1 - 1.5*IQR) & hrp$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers)

# Remove outliers in 'Worldwide.Gross' 
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)

hrp1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(hrp)

#Summary Statistics/Univariate Analysis: 
summary(hrp)

#bivariate analysis 

#scatterplot 
ggplot(hrp, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#bar chart 
ggplot(hrp, aes(x=Year)) + geom_bar()



#Export clean data 
write.csv(hrp, "clean_hrp.csv")








