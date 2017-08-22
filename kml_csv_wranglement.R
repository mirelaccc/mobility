
# some re-wrangling of the 'csv-ed' kml files
setwd("/a/b/csvs/")
library(stringr)

# read in files
df0 <- read.csv("kcp1.csv", sep = ',', header=FALSE)
df1 <- read.csv("kcp2.csv", sep = ',', header=FALSE)
# name columns
colnames(df0) <- c("A","B","C","D","E","F","G","H")
colnames(df1) <- c("A","B","C","D","E","F","G","H")

# only keep relevan columns
df00 <- df0[,-c(2,3,5,7,8)]
df11 <- df1[,-c(2,3,5,7,8)]
# make one dataframe
df2 <- rbind(df00, df11)
# rename columns
colnames(df2)[1] <- "city"
colnames(df2)[2] <- "level"
colnames(df2)[3] <- "lonlats"

# removing the ',' at the start
lonlats <- as.data.frame(df2$lonlats)
colnames(lonlats) <- "ll"

library(tidyverse)
# making smaller dataframes to enable pausing to safe fan-spinning
# can be larger chunks of course, depends on the machine used
lonlats_s1 <- as.data.frame(lonlats[1:200000,])
colnames(lonlats_s1) <- "ll"
s1 <- read.csv(text = sub("^,", "", lonlats_s1$ll), header = FALSE)

lonlats_s2 <- as.data.frame(lonlats[200001:400000,])
colnames(lonlats_s2) <- "ll"
s2 <- read.csv(text = sub("^,", "", lonlats_s2$ll), header = FALSE)

lonlats_s3 <- as.data.frame(lonlats[400001:600000,])
colnames(lonlats_s3) <- "ll"
s3 <- read.csv(text = sub("^,", "", lonlats_s3$ll), header = FALSE)

lonlats_s4 <- as.data.frame(lonlats[600001:872640,])
colnames(lonlats_s4) <- "ll"
s4 <- read.csv(text = sub("^,", "", lonlats_s4$ll), header = FALSE)
# merge into one, rename cols
lls <- do.call("rbind", list(s1, s2, s3, s4))
colnames(lls) <- c("lon_start","lat_start","lon_end","lat_end")

# remove old lonlat col, and have one new, correct dataframe
df2 <- df2[,-c(3)]
df3 <- data.frame(df2, lls)

# separate the cogn and acc measurements
df_acc <- df3[grep("accDelay", df3$city), ]
df_con <- df3[grep("congLevel", df3$city), ]

