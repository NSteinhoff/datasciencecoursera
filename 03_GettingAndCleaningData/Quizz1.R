setwd("~/Documents/Coursera/CourseraDataScience/03_GettingAndCleaningData")

#----------
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

download.file(fileUrl, "./Data/housing.csv", method = "curl")

data <- read.csv("./Data/housing.csv")

table(data$VAL)

#----------
library(xlsx)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

download.file(fileUrl, "./Data/gas.xlsx", method = "curl")

dat <- read.xlsx("./Data/gas.xlsx", sheetIndex = 1, 
	rowIndex = 18:23, colIndex = 7:15)

sum(dat$Zip * dat$Ext, na.rm = T)

#----------
library(XML)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

download.file(fileUrl, "./Data/restaurants.xml", method = "curl")

doc <- xmlTreeParse("./Data/restaurants.xml", useInternal = TRUE)

rootNode <- xmlRoot(doc)

zipCodes <- xpathSApply(doc, "//zipcode", xmlValue)

table(zipCodes)

#----------
library(data.table)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

download.file(fileUrl, "./Data/comHouse.csv", method = "curl")

DT <- fread("./Data/comHouse.csv", sep = ",")
system.time(rowMeans(DT)[DT$SEX==1]) 
system.time(rowMeans(DT)[DT$SEX==2])
system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(mean(DT[DT$SEX==1,]$pwgtp15)) 
system.time(mean(DT[DT$SEX==2,]$pwgtp15))
system.time(DT[,mean(pwgtp15),by=SEX])