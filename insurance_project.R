#load the required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(corrplot)

#load the Dataset
data_ds = read.csv('Insurance_factor_identification.csv')
#check the top 6 rows of dataset
head(data_ds)
#check the bottom 6 rows of dataset
tail(data_ds)
#check the type of of dataset ensure that it is a data.frame
class(data_ds)
#check the shape/dimension of the dataset
dim(data_ds)
#check the structure of the dataset and analyse the columns data type
str(data_ds)
#check allt he column names in there dataset
names_ds = names(data_ds)
names_ds
#check number of rows in the data set
print(nrow(data_ds))
rown=nrow(data_ds)
coln = ncol(data_ds)
#check number of columns in the dataset
print(ncol(data_ds))

#TASK 1
#The committee is interested to know each field of the data collected through 
#descriptive analysis to gain basic insights into the data set and
#to prepare for further analysis

#SOLUTION
#check if the dataset has any null values
print(is.na(data_ds))
total_null_value = sum(is.na(data_ds))
message=paste("number of null values in dataset is ",total_null_value)
cat(message)

summary(data_ds)

#so we have got 2182x7 data set which has information about
#kilometer traveled by the vehicle that year, Area or zone the the vehicle is registered to
#Bonus - how long its been since last claim
# make type of the vehicle, No of policy years insured 
# Claims made by the vehicle owner
# and the insurance paid by the insurance company


#to check a comparison plot between all the columns fo the dataset
pairs(data_ds, col = data_ds$Payment,
      main = "Relational analysis of all the columns in dataset")
#based on the pair plot result we can see that Payment is increasing when Claims and Insured amount is increasing
#Payment does not have much effect based on Kilometers driven, Zone, and bonus
#but another observation is - payment touches its maximum when Make is at maximum that is 9
ggplot(data_ds, aes(x=Make, y=Payment, col=Make))+
  labs(x="Make", y="Total Payment", title = "Correlation of Payment and Vehicle Model")+
  geom_point()
#This tells us that amongst all the make, the one represented by 9, causes a lot of claims and leads to maximum payment
#this gives me an idea that model type - 9 has some faulty component or perhaps poor build which causes more insurance claims
#let's also analyze which model is the best one with maximum number of 0 insurance claims
data_no_claim = data_ds[data_ds$Payment==0,]
data_no_claim$Make <- factor(data_no_claim$Make)
ggplot(data_no_claim, aes(x=Make, fill=Make))+
  geom_bar()+
  labs(x="Make", y="Count")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)+
  ggtitle("Countplot of Make in 0 Payment data subset")

#Let's do the same analysis for Maximum number of insurance claims for each make type
data_max_claims = data_ds[data_ds$Payment >=50000,]
data_max_claims$Make <- factor(data_max_claims$Make)
ggplot(data_max_claims, aes(x=Make, fill=Make))+
  geom_bar()+
  labs(x="Make", y="Count")+
  geom_text(stat = "Count", aes(label = ..count..), vjust=-0.05)+
  ggtitle("Countplot of Make in maxmimum Payment(>50000) Data subset")

#Lets try to visualize it individually

#first i want to check if there are outliers in Kilometer, Zone and Bonus using boxplot
title = "Box plot for kilometer, Zone, Bonus, Make"
boxplot_colors <- c("red", "blue", "green", "purple")
boxplot_labels <- names(data_ds)
boxplot(data_ds$Kilometres, data_ds$Zone, data_ds$Bonus, data_ds$Make, main=title, col = boxplot_colors)
medians <- sapply(list(data_ds$Kilometres, data_ds$Zone, data_ds$Bonus, data_ds$Make),
                  function(x) quantile(x, c(0.25, 0.5, 0.75)))
# Add text labels for median, Q1, and Q3
text(x = 1:4, y = medians["50%", ], labels = paste("Median: ", round(medians["50%", ], 2)), pos = 3)
text(x = 1:4, y = medians["25%", ], labels = paste("Q1: ", round(medians["25%", ], 2)), pos = 1)
text(x = 1:4, y = medians["75%", ], labels = paste("Q3: ", round(medians["75%", ], 2)), pos = 1)

for (i in 1:4) {
  text(x = i, y = par("usr")[3] - 0.15, labels = boxplot_labels[i], srt = 45, pos = 2, xpd = TRUE)
}
#Analysis of data based on box plot
# Observation1 - There are no outliers in all 4 columns Kilometer, Zone, Bonus and Make
# Observation2 - Data is normally distributed because median is at the center of all 4 boxes, 
#its symmetrical, so no skewness
# Observation3 - Kilometers has a shorter spread or IQR spread and Make has largest spread as compared to others


#Lets plot the histogram for numerical data Insured and Claims

hist(data_ds$Insured ,col='red',main="Histogram of Count Insured", breaks=20, xlab= "Insured")
hist(data_ds$Claims,col='blue',breaks=20, main="Histogram of Count Claims", xlab="Claims")
hist(data_ds$Payment, col = 'green',breaks=20, main="Histogram of Payment", xlab = "Payment")

#just to be sure let's plot the density of Insurance, Claims and payment

plot(density(data_ds$Insured), col = 'red',main = "Density Plot of Insured", xlab= "Insured")
plot(density(data_ds$Claims), col = 'blue',main = "Density Plot of Claims", xlab="Claims")
plot(density(data_ds$Payment), col = 'green', main = "Density plot of Payment",xlab = "Payment" )
#as a result we see that the Insured, Claims and Payment have similar Distribution 

#lets study the top 3 and bottom 3 based on Insured, Claims and Payment
#sort the dataset based on Insured amount and check top 3
sort_Insured = data_ds[order(data_ds$Insured),]
tail(sort_Insured, 3)   
head(sort_Insured,3)
#sort the dataset based on Claims and check top 3
sort_Claims = data_ds[order(data_ds$Claims),]
tail(sort_Claims, 3)
head(sort_Claims,3)
#sort the dataset based on Payment  and check top 3
sort_payment = data_ds[order(data_ds$Payment),]
tail(sort_payment, 3)
head(sort_payment, 3)

#Based on the results it is interesting to see that the top 
#3 Payment is also Top 3 Insured and top 3 Claims
#Bottom 3 payments is also bottom 3 claims but interestingly not the same bottom 3 insured

#moving on to our next task - TASK2
#The total value of payment by an insurance company is an important factor to be monitored.
#So the committee has decided to find whether this payment is related to the
#number of claims and the number of insured policy years. 
#They also want to visualize the results for better understanding. 
#to understand if the "Payment" is related to to the "Claims" and insured policy years "Insured" 
# above based on density plot and histogram we saw that our data is skewed for Insured, Claims and Payement

#dim(data_ds)
#data_ds <- data_ds[data_ds$Insured <= 6000, ] #remove all rows which has insured more than 6000 to remove skewness
#dim(data_ds)

#SOLUTION
#we should first calculate the correlation between the dependent and independent variables
#for visualization we can plot it as scatter plot 

sprintf("The correlation of Distance and payment is: %.3f",cor(data_ds$Kilometres, data_ds$Payment))
sprintf("The correlation of Zone and PAyment is: %.3f",cor(data_ds$Zone, data_ds$Payment))
sprintf("The correlation of Bonus with Payment is: %.3f",cor(data_ds$Bonus, data_ds$Payment))
sprintf("The correlation of Make with Payment is: %.3f",cor(data_ds$Make, data_ds$Payment))
sprintf("The correlation of Insured vs Payment is: %.3f",cor(data_ds$Insured, data_ds$Payment))
sprintf("The correlation of Claims Vs Payment is %.3f",cor(data_ds$Claims, data_ds$Payment))

#based on the outcome we can see that Distance and Zone has negative correlation that too is very negligible
#Bonus and Make has positive correlation but that too very feeble
#there is significant positive correlation between Insured policy year and Payment also Claims and Payment

#plot the correlation (scatter plot - using geom_point) between Insured years and Payment
ggplot(data_ds, aes(x=Insured, y=Payment, col = Insured))+
  labs(x="insured years", y="Total Payment", title="correlation of payment and insured years")+
  geom_point()
#plot the correlation between Claims and Payment using Scatter plot
ggplot(data_ds, aes(x=Claims, y=Payment, col=Claims))+
  labs(x="Number of Claims", y="Total Payment", title = "Correlation of Payment and No pf claims")+
  geom_point()

#the scatter plot depicts that we have a very strong dependency of payment
#on Insured years and Number of claims


#moving on to next task - TASK3
#The committee wants to figure out the reasons for insurance payment increase and decrease.
#So they have decided to find whether distance, location, bonus, make, 
#and insured amount or claims are affecting the payment or all or some of these are affecting it. 

#SOLUTION
#to understand the impact of any of these variables on payment we should do a multivariate analysis
#first let's build a model only for KM, Zone, Bonus and MAke, we know this has very less correlation
# just for understanding purpose , let's see how these variables impact payment
regression_payment<-lm(Payment~Kilometres+Zone+Bonus+Make,data=data_ds)
summary(regression_payment)
#output
#Residual standard error: 966700 on 2177 degrees of freedom
#Multiple R-squared:  0.09871,	Adjusted R-squared:  0.09705 
#F-statistic: 59.61 on 4 and 2177 DF,  p-value: < 2.2e-16

#studying the outcome
#A high residual standard error indicates that the model's predictions is not very close to actual values.
#R-square of 9.87 tells us that KM, Zone, Bonus and Make all together only account for 9.87% variation in payment
# Adjusted R-Square of 9.70% tell us that these 4 variables do not give good explanation for the variation in Payment
# F score of 59.61 with 4 and 2177 degree of freedom tells us that the model is statistically significant
# F score is high which tells us that model's prediction ability is not very good 
# very small p-value indicates that at the minimum one of the variable has significant affect on Payment

#now we see that when we built a model excluding the main correlated variable (with Payment), the model's prediction is not very good
#however we can also see that model tells us that there is some unknown factor which is impacting the "payment"
# it is very much possible that "Insured" and "Claims" are those factors

#let's build another model with insured and claims'
reg_model = lm(Payment~Insured+Claims+Kilometres+Zone+Bonus+Make, data= data_ds)
summary(reg_model)

# Studying the outcome
#Residual standard error: 70830 on 2175 degrees of freedom
#Multiple R-squared:  0.9952,	Adjusted R-squared:  0.9952 
#F-statistic: 7.462e+04 on 6 and 2175 DF,  p-value: < 2.2e-16

#with this model we see that we have 99.51% of r-squared and same Adjusted R-Squared value
# so our model is able to explain the variation in Payment very well
# a lower value of F-score with degree of freedom as 6 and 2175 model is statistically significant and 
#model's prediction ability is very good


#moving on to next task - TASK4
#The insurance company is planning to establish a new branch office,
#so they are interested to find at what location, kilometre, and bonus level their 
#insured amount, claims, and payment gets increased. (Hint: Aggregate Dataset) 

#SOLUTION
#To achieve this we need to aggregate our dataset for Insured, claims and Payment
#So we create another dataset which has a column for aggregated/binded value of insured, claims and payment along with other variables

aggregate_data = aggregate(cbind(Insured, Claims, Payment)~Kilometres+Zone+Bonus, data=data_ds, FUN=mean)
ggplot(aggregate_data, aes(x=Kilometres, y=Payment))+
  geom_point(aes(color=Zone, size=Bonus))+
  #geom_text( aes(label = Zone), hjust=2)+
  geom_text(aes(label = Zone), hjust = -1, vjust = 0.5, position = position_nudge(x = -0.1)) +
  geom_text(aes(label = Bonus), hjust = 2, vjust = 0.5, position = position_nudge(x = 0.1)) +
  labs(title="Payment vs. Kilometer by Location and Bonus", x = "Kilometer", y = "Payment")

#Plot Analysis
#with the given outcome we can see that:
# Kilometer - at kilometer value 5 i.e. more than 25000km driven vehicles have lowest payment, followed by 4
# Zone - At zone 7 the payment is lowest followed by 6 and 5, and Zone 4 payment is maximum
# Bonus - At Bonus 7 the payment is lowest followed by 6 and 5, and Bonus 7 payment is maximum

#let's start with next task - Task5
#The committee wants to understand what affects their claim rates
#so as to decide the right premiums for a certain set of situations.
#Hence, they need to find whether the insured amount, zone, kilometre,
#bonus, or make affects the claim rates and to what extent. 

#SOLUTION
#Since Claim is a count type of variable
#for us to determine the whether Kilometre, bonus, Make and insured affects the claim
# we need to use regreesion algorithm
#To my knowledge the most suitable one for count type of data is "Poisson Regression"
#so let's build poisson regression model

pois_model = glm(Claims~Kilometres+Zone+Bonus+Make+Insured, data= data_ds, family=poisson)
summary(pois_model)
#Outcome 
#Coefficients:
#             Estimate Std. Error     z value    Pr(>|z|)    
#(Intercept)  2.656e+00    1.638e-02  162.17   <2e-16 ***
#  Kilometres  -2.692e-01  2.415e-03 -111.47   <2e-16 ***
#  Zone        -2.516e-01  1.744e-03 -144.27   <2e-16 ***
#  Bonus        7.856e-02  1.656e-03   47.46   <2e-16 ***
#  Make         3.765e-01  1.659e-03  226.95   <2e-16 ***
#  Insured      2.837e-05  9.527e-08  297.82   <2e-16 ***

#Outcome Study
# Kilometres: For unit increase in "Kilometres," the log of the expected claims decreases by 0.2692. -> -ve affect
# Zone: for unit increase in Zone, the log of the claims decreased by 0.2516 -> -ve affect
# Bonus:  for unit increase in bonus, the log of claims increases by 0.0786-> +ve but low affect
# Make: For unit increase in make, the log of claims increases by 0.3765 -> +ve and medium affect
# Insured: For unit increase in Insured, the log of claims increased by  0.00002837 -> +ve but extremely low affect

#Conclusion: "Kilometres" has low -ve affect on Claims, 
# "Make" has Medium +ve affect on Claims

plot(pois_model)


 



