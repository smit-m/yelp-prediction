#Scoring the categories based on whether they occur in the mfc and mpc list (1 point for each term)
library(splitstackshape)

#Reading in the raw data
df <- read.csv("output1.csv")
#Removing the rows with 'NA' ratings
df <- df[!is.na(df$rating), ]

#Reading in the lists
mfc <- as.character(read.csv("mfc_top25.csv")[, 1])
mpc <- as.character(read.csv("mpc_top25.csv")[, 1])

#Splittin the types column into components
df_split <- cSplit(df, 'types', sep=";", type.convert=FALSE)

#Calculating the Score
df_split$Category_Score <- ifelse(df_split$types_1 %in% mfc, 1, 0) + ifelse(df_split$types_2 %in% mfc, 1, 0) + ifelse(df_split$types_3 %in% mfc, 1, 0) + ifelse(df_split$types_4 %in% mfc, 1, 0) + ifelse(df_split$types_1 %in% mpc, 1, 0) + ifelse(df_split$types_2 %in% mpc, 1, 0) + ifelse(df_split$types_3 %in% mpc, 1, 0) + ifelse(df_split$types_4 %in% mpc, 1, 0) 
df <- df_split[, c(1:3, 13, 4:8)]

