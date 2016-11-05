#Data Set 1 - Load raw data for wholesale distribution 
wholesale <- read.csv("ny_county_wholesale.csv", header = TRUE)

#Examine the imported dataset
head(wholesale)
summary(wholesale)

#Display structure of the data frame
str(wholesale)

#Retrieve only those records with NAICS Codes 42448 and 42459
wholesale_dist <- subset(wholesale, NAICS_Code %in% c(42448,42459))

#Aggregate codes by county to calculate the total number of wholesale trade establishments by FIPS
wholesale_est <- aggregate (Total_Establishments~County_FIPS, sum,data=wholesale_dist)

#plot histogram of total establishment distribution among counties
hist(wholesale_est$Total_Establishments, breaks =50)

summary(wholesale_est)

#Analysis findings, more urban counties closer to NYC have a higher number of wholesale trade establishments
#Since the median number of establishments is 3, and the average is 13, score weights will need to be applied appropriately.









