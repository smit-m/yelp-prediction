
########################################

#Author: Smit Mehta
#Project: Yelp Popularity Prediction

#######################################


df <- read.csv("test_wINFO.txt", sep = "\t", , stringsAsFactors = FALSE)
#str(df)

#Scoring the categories based on whether they occur in the mfc and mpc list (1 point for each term)
library(splitstackshape)

#Removing the rows with 'NA' ratings
df <- df[!(df$Rating == 'None'), ]

#Reading in the lists
mfc <- as.character(read.csv("mfc_top25.csv")[, 1])
mpc <- as.character(read.csv("mpc_top25.csv")[, 1])

#Splittin the types column into components
df_split <- cSplit(df, 'Type', sep=",", type.convert=FALSE)

#Calculating the Score
df_split$Category_Score <- ifelse(df_split$Type_1 %in% mfc, 1, 0) + ifelse(df_split$Type_2 %in% mfc, 1, 0) + ifelse(df_split$Type_3 %in% mfc, 1, 0) + ifelse(df_split$Type_4 %in% mfc, 1, 0) + ifelse(df_split$Type_1 %in% mpc, 1, 0) + ifelse(df_split$Type_2 %in% mpc, 1, 0) + ifelse(df_split$Type_3 %in% mpc, 1, 0) + ifelse(df_split$Type_4 %in% mpc, 1, 0) 
raw <- df_split[, c(1:51, 56)]
 

yelp <- data.frame(RestName = raw$Restuarant_name, 
                   Claim=ifelse(raw$Claim =='Claimed', 1, 0),
                   Category_Score = raw$Category_Score, 
                   Price=ifelse(raw$Price=='$', 1, ifelse(raw$Price=='$$', 2, ifelse(raw$Price=='$$$', 3, 4))),
                   Website=ifelse(raw$Website=='None', 0, 1),
                   Phone=ifelse(raw$Tel=='None', 0, 1),
                   TakeReservation=ifelse(raw$Takes.Reservations=='Yes', 1, ifelse(raw$Takes.Reservations=='No', 0, NA)),
                   AcceptCreditCards=ifelse(raw$Accepts.Credit.Cards=='Yes', 1, ifelse(raw$Accepts.Credit.Cards=='No', 0, NA)),
                   BikeParking=ifelse(raw$Bike.Parking=='Yes', 1, ifelse(raw$Bike.Parking=='No', 0, NA)),
                   GoodforGroups=ifelse(raw$Good.for.Groups=='Yes', 1, ifelse(raw$Good.for.Groups=='No', 0, NA)),
                   GoodforKids=ifelse(raw$Good.for.Kids=='Yes', 1, ifelse(raw$Good.for.Kids=='No', 0, NA)),
                   Takeout=ifelse(raw$Take.out=='Yes', 1, ifelse(raw$Take.out=='No', 0, NA)),
                   Delivery=ifelse(raw$Delivery=='Yes', 1, ifelse(raw$Delivery=='No', 0, NA)),
                   Attire=ifelse(raw$Attire=='Casual', 0, ifelse(raw$Attire=='None', NA, 1)),
                   StreetParking=ifelse(raw$Parking=='Street', 1, ifelse(raw$Parking=='None', NA, 0)),
                   HasTV=ifelse(raw$Has.TV=='Yes', 1, ifelse(raw$Has.TV=='No', 0, NA)),
                   OutdoorSeating=ifelse(raw$Outdoor.Seating=='Yes', 1, ifelse(raw$Outdoor.Seating=='No', 0, NA)),
                   FreeWiFi=ifelse(raw$Wi.Fi=='Free', 1, ifelse(raw$Wi.Fi=='None', NA, 0)),
                   Alcohol=ifelse(raw$Alcohol=='No', 0, ifelse(raw$Alcohol=='None', NA, 1)),
                   NoiseLevel=ifelse(raw$Noise.Level=='Quiet', 0, ifelse(raw$Noise.Level=='Average', 1, ifelse(raw$Noise.Level=='Loud', 2, ifelse(raw$Noise.Level=='Very Loud', 3, NA)))),
                   GoodForDancing = ifelse(raw$Good.For.Dancing == 'Yes', 1, 0),
                   GoodForWorking = ifelse(raw$Good.for.Working == 'Yes', 1, 0),
                   PoolTable = ifelse(raw$Has.Pool.Table == 'Yes', 1, 0),
                   GlutenFreeOptions = ifelse(raw$Has.Gluten.free.Options == 'Yes', 1, 0),
                   HappyHour = ifelse(raw$Happy.Hour == 'Yes', 1, 0),
                   WaiterService= ifelse(raw$Waiter.Service == 'Yes', 1, 0),
                   Caters = ifelse(raw$Caters == 'Yes', 1, 0),
                   AcceptsAndroidPay = ifelse(raw$Accepts.Android.Pay == 'Yes', 1, 0),
                   AcceptsApplePay = ifelse(raw$Accepts.Apple.Pay == 'Yes', 1, 0),
                   GenderNeutralRestrooms = ifelse(raw$Gender.Neutral.Restrooms == 'Yes', 1, 0),
                   Ambience = ifelse(raw$Ambience == 'None', 0, 1),
                   CoatCheck = ifelse(raw$Coat.Check == 'Yes', 1, 0),
                   WheelchairAccessible = ifelse(raw$Wheelchair.Accessible == 'Yes', 1, 0),
                   DogsAllowed = ifelse(raw$Dogs.Allowed == 'Yes', 1, 0),
                   Music = ifelse(raw$Music == 'None', 0, 1),
                   Smoking = ifelse(raw$Smoking == 'None' | raw$Smoking == 'No', 0, 1),
                   DriveThru = ifelse(raw$Drive.Thru == 'Yes', 1, 0),
                   VegansLiked = ifelse(raw$Liked.by.Vegans == 'Yes', 1, 0),
                   VegetriansLiked = ifelse(raw$Liked.by.Vegetarians == 'Yes', 1, 0),
                   OffersMilitaryDiscount= ifelse(raw$Offers.Military.Discount == 'Yes', 1, 0),
                   Rating=ifelse(as.numeric(raw$Rating)<4, '0', '1'))



for(i in 7:40){
  yelp[, i][is.na(yelp[, i])] <- 0
}


#split train and test sets
index <- sort(sample(nrow(yelp),round(.20*nrow(yelp))))
training <- yelp[-index,]
test <- yelp[index,]


########################
#Trying out models directly without PCA

#CART
library(rpart)
CART_class <- rpart(formula=Rating~., data=training[,-1])
fit <- predict(CART_class, test[,2:40])
predict <- ifelse(fit[,1] <= 0.5, 1, 0)
table(Actual=test$Rating, Prediction=predict)
CART_wrong_rate <- sum(test$Rating != predict) / length(test$Rating)
CART_wrong_rate

#C5.0
library('C50')
C50_class <- C5.0(formula=Rating~., data=training[,-1])
fit <- predict(CART_class, test[,2:40])
predict <- ifelse(fit[,1] <= 0.5, 1, 0)
table(Actual=test$Rating, Prediction=predict)
C50_wrong_rate <- sum(test$Rating != predict) / length(test$Rating)
C50_wrong_rate


#Random Forest
library(randomForest)
fit <- randomForest(formula=Rating~., data=training[,-1], importance=TRUE, ntree=500)
predict <- predict(fit, test[,2:40])
table(Actual=test$Rating, Prediction=predict)
RF_wrong_rate <- sum(test$Rating != predict) / length(test$Rating)
RF_wrong_rate

#######################################


#PCA
yelp_pca <- prcomp(training[, c(2:40)], center = TRUE, scale. = TRUE) 

#print(yelp_pca)

biplot(yelp_pca, scale = 0, xlabs=rep(".", nrow(training)))

#compute standard deviation of each principal component
std_dev <- yelp_pca$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]


#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")



#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")


### selecting 25 PCs that together explain about 82% of the variability in the data

yelp_train <- data.frame(Rating = training$Rating, yelp_pca$x)
yelp_train <- yelp_train[, 1:26]

#transform test into PCA
yelp_test <- predict(yelp_pca, newdata = test)
yelp_test <- as.data.frame(yelp_test)

#select the first 30 components
yelp_test <- yelp_test[,1:25]


#########
# Decision Tree
library(rpart)
dt <- rpart(Rating ~ .,data = yelp_train, method = "anova")
#dt
prediction <- predict(dt, yelp_test)
table(Actual=test$Rating, Prediction=predict)
DT_wrong_rate <- sum(test$Rating != prediction) / length(test$Rating)
DT_wrong_rate


# Random Forest
library(randomForest)
fit <- randomForest(formula=Rating~., data=yelp_train, importance=TRUE, ntree=500)
predict <- predict(fit, yelp_test)
table(Actual=test$Rating, Prediction=predict)
RF_wrong_rate <- sum(test$Rating != predict) / length(test$Rating)
RF_wrong_rate



