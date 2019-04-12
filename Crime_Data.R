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

#tidying dataset; seeing empty cols and getting rid of empty colus

x<- colSums(is.na(AllNICrimeData)) #total na per col

y <- nrow(AllNICrimeData) #total row per col

y-x  # result: any that equal zero are columns containing nothing bu NA and so can be deleted
     #crime id is a string of 

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

random_crime_sample$Location <- toupper(random_crime_sample$Location)

head(random_crime_sample$Location, 3)
head(NI_Thor_PCode$Location, 3)

#trimmign leading and trailing whitespace
random_crime_sample$Location <- gsub('^\\s+|\\s+$',"", random_crime_sample$Location)
NI_Thor_PCode$Primary.Thorfare <- gsub('^\\s+|\\s+$',"", NI_Thor_PCode$Primary.Thorfare)

#reading postcode csv file
NI_postcode <- read.csv("C:/Users/Richard/Documents/CA2/CleanNIPostcodeData.csv", stringsAsFactors = FALSE)
NI_postcode <-unique(NI_postcode)

#primary thorfare is assumed to be the main road for each postcode
#therefore these two attributes will be filtered into a new df tbl

NI_Thor_PCode <- subset(NI_postcode, select = c(Primary.Thorfare, Postcode))
NI_Thor_PCode <- na.omit(NI_Thor_PCode) #removing any na values from primary thorfare
NI_Thor_PCode <- tbl_df(NI_Thor_PCode)


#find a postcode function; filters the rows of NI_Thor_Pcode data for which Primary.Thorfare is a match for Location
#finds the most occurences of each postcode for each of the locations in random_crim_sample 
find_a_postcode <- lapply(random_crime_sample$Location, function(Location) {
  
  matched_location <- filter(NI_Thor_PCode, Primary.Thorfare == Location)
  
  pcodes<- names(which.max(table(matched_location$Postcode)))
  
  
  return(pcodes)
})

# converting pcodes to character vector so it can be added to rand_crime_sample dataframe
pcodes <- as.character(find_a_postcode)
random_crime_sample$Postcodes <- pcodes
str(random_crime_sample)

#test_data <- rowr::cbind.fill(random_crime_sample, NI_Thor_PCode, fill = NA)



#want rows in random to have postcode; using postcode dataset give each value in location(random) a postcode based on location(postcode)


