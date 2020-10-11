---
title: "Team 6 Project - Web Analytics"
output:
  html_document:
    df_print: paged
  pdf_document: default
---


#1.Import the packages
#Install all the relevant packages to be used in R Analysis in later part of the code
install.packages(readxl)
install.packages(tidyverse)
library(fastDummies)
library(tidyverse)
install.packages("car")
library("car")




#2.Import the data
#Importing the library readxl which helps in excel import

library(readxl)
#Importing excel data using the read_excel function from readxl package 

#Set the directory for the Raw file

setwd("C:/Users/Vaibhav/Desktop/CourseWork/Web Analytics/Web Analytics Project/Report-team-6")
getwd()


FB_DATA <- read_excel("data-team-6.xlsx",sheet =  1)
View(FB_DATA)




#3.Understand the data

#Importing tidyverse library which is used to perform different data analysis such as to use glimpse function
library(tidyverse)
glimpse(FB_DATA)

#attaching the dataset, so that we don't have to call the dataset name, and can use the variables directly
attach(FB_DATA)



#4.Definition of the columns

#1) ad_id: unique ID for each ad
#2) xyz_campaign_id: an ID associated with each ad campaign of Goyano
#3) fb_campaign_id: an ID associated with how Facebook tracks each campaign
#4) age: age of the person to whom the ad is shown
#5) gender: gender of the person to whom the add is shown
#6) interest: a code specifying the category to which the person’s interest belongs
#7) Impressions: the number of times the ad was shown
#8) Clicks: number of clicks on for that ad
#9) Spent: Amount paid by company xyz to Facebook, to show that ad
#10) Total conversion: Total number of people who enquired about the product after seeing the ad
#11) Approved conversion: Total number of people who bought the product after seeing the ad



#5.Analysis on the dataset

#Unique values of the the columns using unique function
unique(FB_DATA$age)
unique(FB_DATA$gender)
unique(FB_DATA$xyz_campaign_id)
unique(FB_DATA$interest)


#Printing Summary of the dataset
print(summary(FB_DATA))

#check if each ad_id corresponds to an unique ad in the website
sum(duplicated(ad_id) == FALSE)

paste("Duplicated ads are" , sum(duplicated(ad_id) == TRUE),sep=" ") #As it is Zero, each row is a unique ad




#6.Create KPIs in the dataset

#We will assess these metrics here, to get an idea about it in R. We will use the KPIs in Pivot table in Excel for detailed analysis

#We have used mutate function to create different metrics
FB_DATA4 <- mutate(FB_DATA, CTR = ifelse(Impressions>0,Clicks/Impressions,0))
FB_DATA4 <- mutate(FB_DATA4, CPC = ifelse(Clicks>0,Spent/Clicks,0))
FB_DATA4 <- mutate(FB_DATA4, CPA = ifelse(Approved_Conversion>0,Spent/Approved_Conversion,0))
FB_DATA4 <- mutate(FB_DATA4, ACR = ifelse(Clicks>0,Approved_Conversion/Clicks,0))
FB_DATA4 <- mutate(FB_DATA4, TCR = ifelse(Clicks>0,Total_Conversion/Clicks,0))
FB_DATA4 <- mutate(FB_DATA4, CPM = ifelse(Impressions>0,Spent/(Impressions*1000),0))
FB_DATA4 <- mutate(FB_DATA4, CPI = ifelse(Impressions>0,Spent/Impressions,0))



#7. Regression Model
#7.1. Model 1: Estimating Total Conversion


#We need to create different model to check for Causation between predictor variables and Outcome variable


#Using lm function to build multiple linear regression model
ml1<-lm(formula =Total_Conversion  ~ as.factor(gender)  + as.factor(xyz_campaign_id) + as.factor(age) +Spent
        +
        as.factor(gender):as.factor(age)+ as.factor(gender):Spent+as.factor(age):Spent+
          as.factor(xyz_campaign_id):as.factor(age) + as.factor(xyz_campaign_id):as.factor(gender) + as.factor(xyz_campaign_id):Spent
        ,data = FB_DATA4)

#See the summary of the model using summary function
summary(ml1)





#7.2. Model 2: Estimating Approved_Conversion
#Using lm function to build multiple linear regression model
ml2<-lm(formula =Approved_Conversion   ~ as.factor(gender) + as.factor(xyz_campaign_id) + as.factor(age) +Spent
           +
        as.factor(gender):as.factor(age)+ as.factor(gender):Spent+as.factor(age):Spent
      
        ,data = FB_DATA4)

#See the summary of the model using summary function
summary(ml2)





#8.Checking correlation among the variables
#Importing heatmaply library 
library(heatmaply)

FB_DATA5 <- FB_DATA4 %>%
  select(CTR, CPC, TCR, Impressions, Spent, Clicks,Approved_Conversion)

#Using heatmap function on the correlation of variables
heatmap(cor(normalize(na.omit(FB_DATA5))))




#9.Linear Hypothesis
#9.1. Linear Hypothesis  on Total Conversions
#Importing car library
library("car")

#Performing linear hypothesis to see whether increasing the amount spent increases the effect of gender (Male/Female) on Total Conversion
linearHypothesis(ml1,"as.factor(gender)M:Spent")




#9.2. Linear Hypothesis  on Approved Conversions
#Importing car library
library("car")

#Performing linear hypothesis to see whether increasing the amount spent increases the effect of gender (Male/Female) on Approved Conversion
linearHypothesis(ml2,"as.factor(gender)M:Spent")
```














