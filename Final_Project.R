#Importing Libraries
library(tidyverse)
library(ggmap)
library(tidyr)
library(caTools)
library(caret)
library(randomForest)
library(plotmo)
library(ggplot2)
library(corrplot)
library(plotly)

#Use Scientific Notation
options(scipen = 999)

#Setting up working directory
setwd('C:/Users/User/Desktop/MS in DAE/Spring 2021/STAT 515/Final Project/')

#Reading the dataset
original <- read.csv('Project Dataset.csv', stringsAsFactors = TRUE, na.strings = "Not Available")

#Preoaring column for Google API
latlong <- paste(original$BUILDING.ADDRESS, original$POSTAL.CODE)

#Appending it to the dataset
original[,1] <- latlong
View(original)

energy <- original[,-c(2,9,10,13,14,17,18,19,27)] #Removing unnecessary columns
row.names(energy) <- NULL # Resetting index

en<- energy[,-c(1,3,7,8,12,19)]
View(en)

#Changing column names
names(en)[1] <- "CO2_emissions"
names(en)[2] <- "percentdiffsourceEUI"
names(en)[3] <- "percentdiffsiteEUI"
names(en)[4] <- "energy_star_score"
names(en)[5] <- "Grossbuildingarea"
names(en)[6] <- "numberofbuildings"
names(en)[7] <- "occupancy"
names(en)[8] <- "siteEUI"
names(en)[9] <- "sourceEUI"
names(en)[10] <- "water_use"
names(en)[11] <- "weather_siteEUI"
names(en)[12] <- "weather_sourceEUI"
names(en)[13] <- "year_built"
View(en)

#Correlation Matrix
corr=cor(en)
corr
corrplot(corr,method = "circle")

#Registering for Google API Services
register_google(key = "AIzaSyBuV2QgiIi6XMjJXWjlnm0qFuajrOukyzo")

#Already ran to generate lat and long, read into csv files (quite a long process, so no need to run again)
#ltlg <- geocode(location = energy$BUILDING.ADDRESS, output = "latlon", source = "google")
#write.csv(ltlg, "latlong.csv")

#reading the latlong dataset
latlg <- read.csv("latlong.csv")

#Combining lat long and energy dataset
energy <- cbind(energy, latlg)
View(energy)

energy <- energy[,-c(1,4,7,8,14,16,17,18)] #removing unnecessary columns

#Histogram - Log Transformation
hist(log(energy$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e.), main="Histogram for Carbon Dioxide Emission",xlab="Log(Co2 Emission)", 
     border="blue", col="#f56f42")

#Filtering outliers
energy <- energy[energy$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e. < 	60000,] #setting limit for Co2
energy <- energy[energy$GROSS.BUILDING.FLOOR.AREA..ft². < 	4000000,] #setting limit for Gross building area

energy <- na.omit(energy)
row.names(energy) <- NULL

#Removing single factor property.type
energy <- energy[!(energy$PROPERTY.TYPE == 'Bureau'| energy$PROPERTY.TYPE == 'Courthouse' | energy$PROPERTY.TYPE == 'Immeuble � logements multiples' | energy$PROPERTY.TYPE == 'Other' | energy$PROPERTY.TYPE == 'Other - Lodging/Residential'), ]
row.names(energy) <- NULL

#To be edited based on the diagnostic plot outliers
#Random forest 1nd iteration outliers
energy <- energy[-c(5319,9573,9344),]
row.names(energy) <- NULL
#Polynomial Regression 1st iteration outliers
energy <- energy[-c(5816,5320,5028,9208),]
row.names(energy) <- NULL
#Linear Regression 1th iteration outliers
energy <- energy[-c(9579,5582,5816),]
row.names(energy) <- NULL


#Map Plot of Los Angeles
ma <- energy[energy$PROPERTY.TYPE == 'Multifamily Housing'| energy$PROPERTY.TYPE == 'Hospital (General Medical & Surgical)' | energy$PROPERTY.TYPE == 'Office' | energy$PROPERTY.TYPE == 'Hotel',]
map <- get_map( location = 'Los Angeles', zoom = 10, maptype = "roadmap" )
co2_map <- ggmap(map) + geom_point(data = ma, aes(x = lon, y = lat, 
                                                      color = PROPERTY.TYPE),alpha = .15, size = 2) + guides(fill=guide_legend(title="Property Type")) + xlab('Longitutde')+
  ylab('Latitude') + ggtitle('Top Four Property Types with Highest CO2 Emissions')

co2_map
co2_map + facet_wrap( ~ PROPERTY.TYPE, ncol = 2)+ ggtitle('Individual Property Type CO2 Emission')


#Setting Seed & Splitting datasets into train and test
set.seed(123)
split = sample.split(energy$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., SplitRatio = 0.7)
training_set = subset(energy, split == TRUE)
test_set = subset(energy, split == FALSE)

#Linear Regression
regres <- lm(formula = CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e. ~ ., data = training_set)
summary(regres)

#Predicting Test set
y_pred = predict(regres, newdata = test_set[-1])

#Joining test value and predicted value
actuals_preds <- data.frame(cbind(actuals=test_set$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., predicteds=y_pred))

#Diagnostic Plot
par(mfrow=c(2,2))
plot(regres)


#K-folds Cross Validation
folds = createFolds(training_set$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., k = 10)
cv = lapply(folds, function(x){
  training_fold = training_set[-x,]
  test_fold = training_set[x,]
  regres <- lm(formula = CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e. ~ ., data = training_fold)
  y_pred = predict(regres, newdata = test_fold[-1])
  actuals_preds <- data.frame(cbind(actuals=test_fold[1], predicteds=y_pred))
  cor_accu  = cor(actuals_preds)
  return(cor_accu)
})

#Finding the mean accuracy of all folds
df <- data.frame(cv)
df <- df[1,c(2,4,6,8,10,12,14,16,18,20)]
mean(as.numeric(df))

#RSME for Linear Regression
RMSE(test_set$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., y_pred)

#Random Forest Regression
rand_regres <- randomForest(x = training_set[-c(1,8)], y = training_set$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., ntree = 40)

y_rand_pred = predict(rand_regres, newdata = test_set[-1])
actuals_rand_preds <- data.frame(cbind(actuals=test_set$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., predicteds=y_rand_pred))
accuracy <- cor(actuals_rand_preds)

#Diagnostic Plot
plotres(rand_regres)

#RMSE for Random Forest
RMSE(test_set$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., y_rand_pred)


#Polynomial Regression
pr <- lm(CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e. ~ polym(X..DIFFERENCE.FROM.NATIONAL.MEDIAN.SITE.EUI,ENERGY.STAR.SCORE,GROSS.BUILDING.FLOOR.AREA..ft².,NUMBER.OF.BUILDINGS,OCCUPANCY,SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft²., degree = 2, raw = TRUE), data = training_set)
summary(pr)
y_pred_pr = pr %>% predict(test_set[-1])
actuals_pr_preds <- data.frame(cbind(actuals=test_set$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., predicteds=y_pred_pr))
accuracy_pr <- cor(actuals_pr_preds)
#RMSE for Polynomial Regression
RMSE(y_pred_pr, test_set$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e.)


