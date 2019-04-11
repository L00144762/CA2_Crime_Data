#unzipping crime data
zipfile <- "NI Crime Data.zip"
unzip(zipfile)


getwd()

#setting wd to NI Crime Data to work with csv files
setwd("NI Crime Data/")

#creating a variable pointing to the csv files contained in each subfolder of NI Crime Data

csv_files <- list.files(path = "C:\\Users\\Richard\\Documents\\CA2_Crime_Data\\NI Crime Data", pattern=".csv"
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
random_crime_sample <- AllNICrimeData[sample(1:nrow(AllNICrimeData)
                                                    , 1000, replace= FALSE),]

NI_postcode <- read.csv("C:/Users/Richard/Documents/CA2/CleanNIPostcodeData.csv", stringsAsFactors = FALSE)

sum(is.na(NI_postcode$Postcode))
