#unzipping crime data
zipfile <- "NI Crime Data.zip"
unzip(zipfile)


getwd()

#setting wd to NI Crime Data to work with csv files
setwd("NI Crime Data/")

#creating a variable pointing to the csv files contained in each subfolder of NI Crime Data

csv_files <- list.files(pattern=".csv"
                        ,recursive = TRUE, full.names = FALSE)
csv_files

#creating a dataframe consisting of each all the csv files

AllNICrimeData <- do.call(rbind.data.frame, lapply(csv_files, 
                                           FUN=function(amalgamate){ 
                                             
                                             read.csv(amalgamate)
                                             
                                             }))


#changing working directory back to project

setwd("C:/Users/Richard/Documents/CA2_Crime_Data/")

write.csv(AllNICrimeData, "AllNICrimeData.csv")

#AllNICrimeData[AllNICrimeData == ""] <- NA

#number of rows in dataset
nrow(AllNICrimeData)


#filtering unwanted columns

AllNICrimeData <- subset(AllNICrimeData, select = -c(Crime.ID, LSOA.code, LSOA.name,
                                                             Last.outcome.category, Context)) 

# factorising Crime type attribute
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type)
str(AllNICrimeData$Crime.type)


#celaning Location attribute
AllNICrimeData$Location <- gsub("On or near", "", AllNICrimeData$Location)
AllNICrimeData$Location[AllNICrimeData$Location == " "] <- NA

#removing all NA values from crime data
AllNICrimeData <- na.omit(AllNICrimeData)
colSums(is.na(AllNICrimeData))

#random sample of 1000
random_crime_sample <- AllNICrimeData[sample(1:nrow(AllNICrimeData), 1000, replace= FALSE),]

#reading postcode csv file
NI_postcode <- read.csv("C://Users/Richard/Documents/CA2/CleanNIPostcodeData.csv", stringsAsFactors = FALSE)


#primary thorfare is assumed to be the main road for each postcode
#therefore these two attributes will be filtered into a new df tbl 
#can do more with data table data frame

NI_Thor_PCode <- subset(NI_postcode, select = c(Primary.Thorfare, Postcode))
NI_Thor_PCode <- na.omit(NI_Thor_PCode) #removing any na values from primary thorfare
NI_Thor_PCode <- tibble::as.tibble(NI_Thor_PCode)

#Primary.thorfare is in uppercase; msetting Location to uppercase in random_sample 
head(NI_Thor_PCode$Primary.Thorfare, 3)
random_crime_sample$Location <- toupper(random_crime_sample$Location)

head(random_crime_sample$Location, 3)
head(NI_Thor_PCode$Primary.Thorfare, 3)

#there is leading whitespace in random_sample(Location). This trims leading and trailing whitespace from key columns
random_crime_sample$Location <- gsub('^\\s+|\\s+$',"", random_crime_sample$Location)
NI_Thor_PCode$Primary.Thorfare <- gsub('^\\s+|\\s+$',"", NI_Thor_PCode$Primary.Thorfare)



#find a postcode function; filters the rows of NI_Thor_Pcode data for which Primary.Thorfare is a match for Location
#finds the most occurences of each postcode for each of the matched locations in random_crim_sample 

library(dplyr)
find_a_postcode <- function(Location) {
  
  matched_location <- filter(NI_Thor_PCode, Primary.Thorfare == Location)
  
  pcodes<- names(which.max(table(matched_location$Postcode)))
  
  
  return(pcodes)
  }

rm(pcodes)

pcodes <- lapply(random_crime_sample$Location, find_a_postcode)

# converting pcodes to character vector so it can be properly added to rand_crime_sample dataframe
pcodes <- as.character(pcodes)
random_crime_sample$Postcodes <- pcodes
str(random_crime_sample)

write.csv(random_crime_sample, file = "random_crime_sample.csv", row.names = FALSE)

#creating updated_random_sample as a subset of random_crime_sample but with select colums
updated_random_sample <- subset(random_crime_sample, select = c(Month, Longitude,Latitude, Location,
                                                                Crime.type, Postcodes))

#covnerting to data table dta frame and reordering                                  
chart_data <- tibble::as.tibble(updated_random_sample)
chart_data <- chart_data[order(chart_data$Postcodes, chart_data$Crime.type),]
chart_data_pcodes <- chart_data[order(chart_data$Postcodes),]
chart_data_crimetype <- chart_data[order(chart_data$Crime.type),]

#summary stats for crime type

#occurence of each crime type in ech postcode.
tapply(chart_data$Crime.type, chart_data$Postcodes,  summary)

#total occurence of each crime
table(chart_data$Crime.type)
str(chart_data$Crime.type)

#barchart of crime data. par(mar=) allows the margins of the page to be altered to fit x labels.
par(mar=c(11,4,4,2))
barplot(crime_type, main = "Crime Type", ylab = "Freq",
        las =2, ylim = c(0, 400), cex.names = 0.9, 
        legend.text = "x-axis = CrimeType")

        