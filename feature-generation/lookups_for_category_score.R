#Creating the lookup tables for most frequent and most popular Category

library(splitstackshape)
library(reshape2)
library(stringr)
library(dplyr)

#Reading in the raw data
df <- read.csv("output1.csv")
#Removing the rows with 'NA' ratings
df <- df[!is.na(df$rating), ]

#Subsetting for the required columns
sub <- df[, c(4,9)]
sub_split <- cSplit(sub, 'types', sep=";", type.convert=FALSE)

#Transforming the dataset to required form
d1 <- sub_split[, c(1,2)]
names(d1) <- c("rating", "type")
d2 <- sub_split[!is.na(sub_split$types_2), c(1,3)]
names(d2) <- c("rating", "type")
d3 <- sub_split[!is.na(sub_split$types_3), c(1,4)]
names(d3) <- c("rating", "type")
d4 <- sub_split[!is.na(sub_split$types_4), c(1,5)]
names(d4) <- c("rating", "type")

d <- rbind(d1, d2, d3, d4)

#Aggregating to get the most frequently used category
mfc <- d %>% group_by(type) %>% summarise(AvgRating = mean(rating, na.rm = TRUE), TotCount = length(rating)) %>% arrange(-TotCount)
#Aggregating to get the most popular category
mpc <- d %>% group_by(type) %>% summarise(AvgRating = mean(rating, na.rm = TRUE), TotCount = length(rating)) %>% arrange(-AvgRating)
write.csv(head(mfc, 25), file = "mfc_top25.csv", row.names = FALSE)
write.csv(head(mpc, 25), file = "mpc_top25.csv", row.names = FALSE)
