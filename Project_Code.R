#CIS512 Final Project R Code - Curtis Robbins

#%%%%%% LOADED PACKAGES %%%%%%

#Load the VIM and mice packages in R (used for inputing missing values in Dataset #4)>>>>>

#%%%%%% PHASE 1 - DATA CLEANING %%%%%%

#<<<<<<DATASET 1 - Load raw data for wholesale distribution by County FIPS>>>>>>
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

#FINDINGS - urban counties closer to NYC have a higher number of wholesale trade establishments

#Dataset 1 (wholesale_est) - Wholesale Establishments by County - Cleaned


#<<<<<<DATASET 2 - Load raw data for poverty data by County FIPS>>>>>>
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

#Use a Kernal Density Plot to view the distribution of poverty percentage all ages 
pov_plot <-density(ny_poverty$Poverty_Per_All_Ages)
plot(pov_plot)

#Based on graph, look specifically at counties with poverty percentages less than 10 and greater than 20

low_pov <- subset(ny_poverty, Poverty_Per_All_Ages <10)
#With Medium Household Income no less than $70K for these four counties, the low poverty rate is in line

high_pov <- subset(ny_poverty, Poverty_Per_All_Ages >20)
#With Medium Household Income of $34K and $48K respectively for Bronx and Kings County, poverty percentages remain very high because 
#of the concentration of poverty in this urban area;counties represent the Bronx and Brooklyn in the five boroughs of NYC. 

#FINDINGS - There are a total of 62 Counties in NYS

#Dataset 2 (ny_poverty) - Poverty by NYS County - Cleaned


#<<<<<<DATASET 3 - Load Drought data (2 files) by County FIPS>>>>>>
#See metadata file; both datasets are small enough that all pre-processing steps can be done in MS Excel.

severe_drought <- read.csv("ny_county_D2.csv", header = TRUE)
extreme_drought <- read.csv("ny_county_D3.csv", header = TRUE)

#Append extreme_drought classfication to severe_drought classification by county.
ny_drought_con <- merge(severe_drought,extreme_drought, by=c("FIPS", "State", "County"), all=TRUE)

#Replace remaining NAs under D3_Class with 0
ny_drought_con[is.na(ny_drought_con)] <- 0

#Dataset 3 (ny_drought_con) Drought Conditions by County - Cleaned


#<<<<<<DATASET 4 - Load raw data for farm data by County FIPS>>>>>>
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

#Use the mice function called "md.pattern" to get a better understanding of the pattern of missing data
md.pattern(farms_FIPS)

#OPTION 1 - Try using a random sampling with replacement method to fill in the missing fields.
#One option is to use the R package: Multivariate Imputation by Chained Equations (mice) to calculate missing values in the dataset.
#Include Random Forest function as a regression and classification method to accommodate interactions and non-linearities.
#Data Source: http://r-statistics.co/Missing-Value-Treatment-With-R.html (Section 4.3 mice)

#miceMod <- mice(farms_FIPS[, !names(farms_FIPS) %in% "medv"], method="rf")  # perform mice imputation, based on random forests.
#farms_alldata <- complete(miceMod)  # generate the completed data
#anyNA(farms_alldata)

#The missing values are now populated.
#Look at the populated fields, do the numbers make sense?  Can cropped acres be more than farmed acres?
#After further review, the cropped acres field does not appear to be an independent variable.
#In fact there exists a distinct relationship between farmed acres and cropped acres.

#Therefore, peforming a bootstrapping analysis with mice (random forests) does not provide accurate results.

#OPTION 2 - Perform a linear regression analysis to determine an equation for calculating the missing fields.
with (farms_FIPS,plot (Farmed_Acres,Cropped_Acres))
lm(Cropped_Acres~Farmed_Acres,data=farms_FIPS)
#Coefficients:
#(Intercept)  Farmed_Acres  
#1617.9660        0.4748 
#Equation of a line: y=mx+b
#Cropped_Acres=(.4748)*(Farmed_Acres) + 1617.97

#Check R2 value - This model accounts for 56% of the variance, a more accurate method for estimating the missing values. 
farms.lm <- lm(Cropped_Acres~Farmed_Acres,data=farms_FIPS)
summary(farms.lm)$r.squared
#0.5641398

#Populate missing Cropped_Acres fields by County using the regression equation.
#Putnum County
farms_FIPS[35,5]=3490
#Seneca County
farms_FIPS[41,5]=63211
#Westchester County
farms_FIPS[51,5]=4682

#Replace remaining NAs under Acres_Rented fields with 0
farms_FIPS[is.na(farms_FIPS)] <- 0

#Create new field titled "Avg_CA_Per_Farm" = Average Cropped Acres Per Farm rounded to zero decimal places 
farms_FIPS$Avg_CA_Per_Farm <- with(farms_FIPS, round(Cropped_Acres/Total_Farms),0)

#Next display descriptive stats and structure of the new data frame
summary(farms_FIPS)
str(farms_FIPS)

#Use a Kernal Density Plot to view the distribution of small farms <= 100 cropped acres
sf_plot <-density(farms_FIPS$Avg_CA_Per_Farm)
plot(sf_plot)

small_farms <- subset(farms_FIPS, Avg_CA_Per_Farm <=100)
#27 out of 53 counties with an agricultural district in NYS have been identified as having 
#average cropped acres per farm <100 (concentration of small farms)
#Note: Using the bootstrapping method for missing values only 26 counties were identified.

#Dataset 4 (small_farms) - NYS Counties with a High Concentration of Small Farms - Cleaned

#<<<<< All coding complete - All 5 Datasets ready for analysis (11-07-16)>>>>>
#<<<<< Dataset #4 revised after further review of bootstrapping analysis (11-22-16)>>>>>


#%%%%%% PHASE 2 - ANALYSIS STAGE %%%%%%

#To prepare for the first stage of the analysis begin by merging the following datasets.

#Perform a left outer join of the small_farms dataset and the remaining 3 data frames.
farms_alldata <- merge(small_farms,ny_drought_con, by=c("FIPS"), all.x=TRUE)
farms_alldata <- merge(farms_alldata,wholesale_est, by.x=c("FIPS"), by.y=c("County_FIPS"), all.x=TRUE)
farms_alldata <- merge(farms_alldata,ny_poverty, by=c("FIPS"), all.x=TRUE)

#Create a function to remove all of the unwanted columns after merging and rename columns accordingly.
col.dont.want <- c("State.x", "County.y", "State_FIPS", "County_FIPS", "State.y", "County", "Acres_Owned", "Acres_Rented", "Poverty_Est_All_Ages", 
  "Poverty_Est_Age_0.17","Poverty_Per_Age_.0.17", "Poverty_Est_Age_5.17", "Poverty_Per_Age_5.17", "Med_HH_Inc")
analysis_dataset <- farms_alldata[,!names(farms_alldata) %in% col.dont.want,drop=F]
names(analysis_dataset)[names(analysis_dataset)=="County.x"]<-"County"

#Replace remaining NAs under farms_alldata fields with 0
analysis_dataset[is.na(analysis_dataset)] <- 0

#Display structure of the data frame (individual data types)
str(analysis_dataset)
summary(analysis_dataset)

#Convert all integers (Total_Acres, Farmed_Acres and Total_Farms) to numeric values before categorizing.
analysis_dataset$Total_Acres <- as.numeric(as.character(analysis_dataset$Total_Acres))
analysis_dataset$Farmed_Acres <- as.numeric(as.character(analysis_dataset$Farmed_Acres))
analysis_dataset$Total_Farms <- as.numeric(as.character(analysis_dataset$Total_Farms))

#Because the dataset has a unique field for each county (27 different FIPS codes), a decision tree in R could not be used.
#Instead, a scoring matrix will be utilized to rank each county based on the predetermined categorical variables.

#Begin by assigning scores (low = 1 /average = 3 / high = 5) across the 4 categorical variables for each county.
#Based on the counting principle, this will produce 81 different combinations (3x3x3x3).

#Determine distribution ranges for data by calculating the mean and standard deviation for each field to be used in the analysis:
#Total_Farms, Total_Establishments and Poverty_Per_All_Ages (Note: Drought is already classified as extreme and severe)
sd(analysis_dataset$Poverty_Per_All_Ages,na.rm=FALSE) #SD 3.858 and Mean is 14.07 (Class: Low<10.2 / Average 10.2-17.9 / High>17.9)
sd(analysis_dataset$Total_Establishments,na.rm=FALSE) #SD 7.111 and Mean is 3.481 (Class: Low<1 / Average 1-10.6 / High>10.6)
sd(analysis_dataset$Total_Farms,na.rm=FALSE) #SD 350.175 and Mean is 544.9 (Class: Low<194.7 / Average 194.7-895.1 / High>895.1)

#Next classify the above fields based on the calculated distribution ranges.
analysis_dataset$farms.type<-ordered(cut(analysis_dataset$Total_Farms,c(0,195,895,1600), labels=c(1,3,5)))
analysis_dataset$poverty.type<-ordered(cut(analysis_dataset$Poverty_Per_All_Ages,c(0,10.2,17.9,20), labels=c(1,3,5)))
analysis_dataset$establishments.type<-ordered(cut(analysis_dataset$Total_Establishments,c(-1,.95,11,35), labels=c(1,3,5)))

#Create a function to convert the rankings to numeric values, retaining the original values (1,3,5)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} 
analysis_dataset$farms.score <- as.numeric.factor (analysis_dataset$farms.type)
analysis_dataset$poverty.score <- as.numeric.factor (analysis_dataset$poverty.type)
analysis_dataset$establishments.score <- as.numeric.factor (analysis_dataset$establishments.type)

#Classify the Drought field as Normal = 1 / Severe = 3 / Extreme = 5
analysis_dataset$drought.score<-ifelse(analysis_dataset$D3_Class>0,5, ifelse(analysis_dataset$D2_Class>0,3,1))

#Create a new total.score field by aggregating all 4 scores in the dataset.
analysis_dataset$total.score <-analysis_dataset$farms.score + analysis_dataset$poverty.score + analysis_dataset$establishments.score + analysis_dataset$drought.score

#Determine the capacity for expansion of cropped acres among the three counties.
#Begin by determining the additional capacity for crop acres using the previous regression equation for cropped acres vs. farmed acres (Dataset 4)
#Cropped_Acres=(.4748)*(Farmed_Acres) + 1617.97

#Create a function to automate the calculation. 
regression.eq <- function(x) {as.numeric((.4748)*(x)) + 1617.97} 
analysis_dataset$capacity <- regression.eq (analysis_dataset$Farmed_Acres)

#Determine the percentage that cropped acres can proportionally increase before increasing farmed acres.
analysis_dataset$per.increase <- ((analysis_dataset$capacity - analysis_dataset$Cropped_Acres)/analysis_dataset$Cropped_Acres) * 100

#Prioritize counties based on their opportunities and challenges.

#Step 1: Identify counties with total scores >= 16 and drought.score >1 (severe and extreme drought conditions) 
inv_counties <- subset(analysis_dataset, total.score >= 16 & drought.score >1 )
#The analysis identified three prospective counties as potential candidates for investment.
#Chautauqua County (FIPS Code = 36013), Chemung County (FIPS Code = 36015) and Wayne County (FIPS Code = 36117)

#Step 2: Examine the three identified counties based on their capacity for increasing cropped acres.
#Eliminate Wayne County even though it has the greatest capacity for cropland; the reason why their cropped acres
#are not proportional to the rest of the NYS counties (outlier) is because they are the top apple producing county. 
inv_counties <- inv_counties[-c(3),]

#Step 3: From the remaining counties, identify a single county for investment based on highest per.increase.
target_co <- head(inv_counties[order(inv_counties$per.increase, decreasing=T),],n = 1)

#Final Results - Chautauqua County will be targeted for investment because they can increase their cropped acres by 36%,
#prior to investing in additional farmed acres, offering the most potential for both short-term (drought) and long-term investment (increased production).
#%%%%%%%%%% Code and Analysis Complete %%%%%%%%%%%
















