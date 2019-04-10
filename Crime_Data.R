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

AllNICrimeData <- do.call(rbind.data.frame,lapply(csv_files, 
                                           FUN=function(amalgamate){ 
                                             
                                             read.csv(amalgamate)
                                             
                                             }))

#changing working directory back to project

setwd("C:/Users/Richard/Documents/CA2_Crime_Data/")

write.csv(AllNICrimeData, "AllNICrimeData.csv")

#number of rows in dataset
nrow(AllNICrimeData)
