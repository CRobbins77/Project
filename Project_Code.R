#CIS512 Final Project R Code - Curtis Robbins
#<<<<<<Dataset 1 - Load raw data for wholesale distribution by County FIPS>>>>>>
wholesale <- read.csv("ny_county_wholesale.csv", header = TRUE)

#------Examine the imported dataset------

#Display the first six records
head(wholesale)

#Descriptive stats - median and mean for each column
summary(wholesale)

#Display structure of the data frame (individual data types)
str(wholesale)

#Retrieve only those records with NAICS Codes 42448 and 42459
wholesale_dist <- subset(wholesale, NAICS_Code %in% c(42448,42459))

#Aggregate codes by county to calculate the total number of wholesale trade establishments by FIPS
wholesale_est <- aggregate (Total_Establishments~County_FIPS, sum,data=wholesale_dist)

#Use a Kernal Density Plot to view the distribution of wholesale establishments among counties
est_plot <-density(wholesale_est$Total_Establishments)
plot(est_plot)

#Run descriptive stats again on new dataset
summary(wholesale_est)

#FINDINGS - urban counties closer to NYC have a higher number of wholesale trade establishments
#Since the median number of establishments is 3, and the average is 13, score weights will need to be applied appropriately.

#Wholesale Establishment Dataset - Cleaned


#<<<<<<Dataset 2 - Load raw data for poverty data by County FIPS>>>>>>
poverty <- read.csv("US_county_poverty.csv", header = TRUE)

#------Examine the imported dataset------

#Display the first six records
head(poverty)

#Descriptive stats - median and mean for each column
summary(poverty)

#Display structure of the data frame (individual data types)
str(poverty)

#Retrieve only poverty data for counties in New York State
ny_poverty <- subset(poverty, State %in% c('NY'))

#Remove New York State total record (row 1), creating file with only 62 counties
ny_poverty <- subset(ny_poverty, County_FIPS >0)

#Add leading zeros to County_FIPS to allow for combining of State and County FIPS codes
ny_poverty$County_FIPS <- formatC(ny_poverty$County_FIPS, width = 3, format = "d", flag = "0")

#Combine State_FIPS and County_FIPS to form FIPS field in table
ny_poverty$FIPS <- paste0(ny_poverty$State_FIPS,ny_poverty$County_FIPS)

#Change the order of columns so FIPS is first
ny_poverty <- ny_poverty[,c(12,1:11)]

#List rows of data that have missing values
ny_poverty[!complete.cases(ny_poverty),]

#No rows in the dataset have missing values, next identify any outliers in the dataset by graphing

#First begin by displaying structure of the data frame (individual data types)
str(ny_poverty)

#Since poverty percentage per all ages is a factor, it must be converted to a numeric value before graphing
ny_poverty$Poverty_Per_All_Ages <- as.numeric(as.character(ny_poverty$Poverty_Per_All_Ages))

#Verify dataframe changes
str(ny_poverty)

#Use a Kernal Density Plot to view the distribution of poverty percentage all ages 
pov_plot <-density(ny_poverty$Poverty_Per_All_Ages)
plot(pov_plot)

#Based on graph, look specifically at counties with poverty percentages less than 10 and greater than 20

low_pov <- subset(ny_poverty, Poverty_Per_All_Ages <10)
#With Medium Household Income no less than $70K for these four counties, the low poverty rate is in line

high_pov <- subset(ny_poverty, Poverty_Per_All_Ages >20)
#With Medium Household Income of $34K and $48K respectively for Bronx and Kings County, poverty percentages remain very high because 
#of the concentration of poverty in this urban area;counties represent the Bronx and Brooklyn in the five boroughs of NYC. 

#Poverty by County Dataset - Cleaned


#<<<<<<Dataset 3 - Drought data by County FIPS>>>>>>
#See metadata file; both datasets are small enough that all pre-processing steps can be done in MS Excel.

#Files ny_county_D2 and ny_county_D3 - Cleaned


#<<<<<<Dataset 4 - Load raw data for farm data by County FIPS>>>>>>
farms <- read.csv("ny_county_farms.csv", header = TRUE)

#------Examine the imported dataset------

#Display the first six records
head(farms)

#Descriptive stats - median and mean for each column
summary(farms)

#Display structure of the data frame (individual data types)
str(farms)

#Check if there are any duplicate county records
n_occur <- data.frame(table(farms$County))
n_occur[n_occur$Freq > 1,]
farms[farms$County %in% n_occur$Var1[n_occur$Freq > 1],]

#Load raw data for County FIPS codes
FIPS <- read.csv("ny_county_FIPS.csv", header = TRUE)

#Display structure of the data frame (individual data types)
str(FIPS)

#Change the order of columns so County is first for appending
FIPS <- FIPS[,c("County","FIPS")]

#Change county field from lowercase to uppercase prior to merge
FIPS <- as.data.frame(sapply(FIPS, toupper))

#Append County FIPS to farms dataset
farms_FIPS <- merge(FIPS,farms, by=c("County"))

#List rows of data that have missing values
farms_FIPS[!complete.cases(farms_FIPS),]

#Results of analysis indicate that Putnum, Seneca and Westchester have missing data 

#Load the VIM and mice packages in R (used for imputing missing values with plausible data values)

#Use the mice function called "md.pattern" to get a better understanding of the pattern of missing data
md.pattern(farms_FIPS)

#Use Multivariate Imputation by Chained Equations (mice) to calculate missing values in dataset.
#Include Random Forest function as a regression and classification method to accommodate interactions and non-linearities.
#Data Source: http://r-statistics.co/Missing-Value-Treatment-With-R.html (Section 4.3 mice)

miceMod <- mice(farms_FIPS[, !names(farms_FIPS) %in% "medv"], method="rf")  # perform mice imputation, based on random forests.
farms_alldata <- complete(miceMod)  # generate the completed data
anyNA(farms_alldata)

#Missing Values Populated

#Create new field titled "Avg_CA_Per_Farm" = Average Cropped Acres Per Farm 
farms_alldata$Avg_CA_Per_Farm <- with(farms_alldata, Cropped_Acres/Total_Farms)

#Next display descriptive stats and structure of the new data frame
summary(farms_alldata)
str(farms_alldata)

#Use a Kernal Density Plot to view the distribution of small farms <= 100 cropped acres
sf_plot <-density(farms_alldata$Avg_CA_Per_Farm)
plot(sf_plot)

small_farms <- subset(farms_alldata, Avg_CA_Per_Farm <=100)
#26 counties have been identified as having average cropped acres per farm <100 (concentration of small farms)

#Small Farms Dataset - Cleaned

#All coding complete - All 5 Datasets ready for analysis (November 7th, 2016)






