
# some re-wrangling of the 'csv-ed' kml files
setwd("/home/x/projs/mob/y/csvs/")
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

## removing the ',' at the start
lonlats <- as.data.frame(df2$lonlats)
colnames(lonlats) <- "ll"
# second try
lonlats2 <- as.data.frame(as.character(df2$lonlats))
colnames(lonlats2) <- "ll"

library(tidyverse)

mydata2 <- read.csv(text = sub("^,", "", lonlats2$ll), header = FALSE)

lonlats2_s <- lonlats2[1:20,]


======

separate(lonlats, ll, into = c("V0", "V1", "V2", "V3", "V4"), sep = ",") %>% select(-V0)

ly <- read.csv(lonlats =sub("^,", "", as.character(lonlats$ll)), header = TRUE) 

write.csv(lonlats, "lls2.csv")
lx <- read.csv("lls2.csv" =sub("^,", "", as.character(lonlats$ll)), header = TRUE) 

lonlats$ll4 <- sub(',*\\,', '', lonlats)

ll_c <- as.character(df2$lonlats)

ll <- read.table(ll_c, sep = ",")

ll3 <- as.data.frame(sub(',*\\,', '', lonlats))

lonlats$a <- str_split(lonlats, ",")[[1]]
lonlats$b <- str_split_fixed(lonlats, ",", 2)

str_split_fixed(",", fixed("..."), 2)
b <- strsplit(as.character(lonlats),',') 
 

df2$llx <- sub('.*\\,', '', df2$ll4)


df2$ll3 <- sapply(strsplit(df2$ll, ","), "[", 2)

df2$ll <- sub(',*\\,', '', df2$lonlats)

df2$ll1 <- sub('*\\,', '', df2$ll)
df2$ll2 <- sub(',*\\,', '', df2$lonlats)
df2$ll3 <- sub(',*\\,', '', df2$lonlats)
df2$ll4 <- sub(',*\\,', '', df2$lonlats)


df3_ll <- data.frame(strsplit(as.character(df2$ll), ','))
df3_ll2 <- data.frame(t(df3_ll), stringsAsFactors = TRUE)

colnames(df3_ll2) <- c("lon_start","lat_start","lon_end","lat_end")

df3a <- df3_ll2$lon_start
df3b <- df3_ll2$lat_start
df3c <- df3_ll2$lon_end
df3d <- df3_ll2$lat_end 

dft1 <- data.frame(df3a,df3b)
dft2 <- data.frame(dft1,df3c)
dft3 <- data.frame(dft2,df3d)
 

colnames(df3_ll2)[1] <- 
df3_ll3 <- df3_ll2[,c(2,3,4,5)]


df_acc <- df3[grep("accDelay", rownames(df3)), ]
dfcon <- df3[grep("congLevel", rownames(df3)), ]

df_acc <- iris[grep("accDelay", df2$city), ]
dfcon <- iris[grep("congLevel", df2$city), ]

library("ggmap")
m <- get_map(location=c(lon=median(test$longitude), lat=median(test$latitude)), zoom=8)
ggmap(m) + geom_point(aes(x=longitude, y=latitude, color=mode), data=test) + 
  geom_line(aes(x=longitude, y=latitude, color=mode), data=test)


