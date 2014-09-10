importData <- function() {
	## Set working directory
	setwd("~/Documents/Coursera/CourseraDataScience/02_R-Programming")

	## Download and unzip file
	# Store URL
	fileUrl <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"

	# Create (virtual) temporary file to store zip archive
	temp <- tempfile()

	# Download file into temporary file
	download.file(fileUrl, destfile = temp, method = "curl")

	# Unzip temporary file int data directory
	unzip(temp, exdir = "./Data")

	# Remove the temporary file
	unlink(temp)

	# Read data
	read.csv("./Data/outcome-of-care-measures.csv", 
		na.strings = "Not Available")
}

## Function to find hostpitals for state and outcome by rank
rankhospital <- function(state, outcome, num) {
	## Read outcome data
	data <- importData()
	
	## Check that state and outcome are valid
	# Check state
	if(!(tolower(state) %in% tolower(data$State))){
		stop("invalid state")
	}
	
	# Check outcome and set search string
	regex <- if(!is.na(pmatch("heart attack", tolower(outcome)))){
		"^(Hospital.30.Day.Death).*(Heart.Attack)$"
	} else if(!is.na(pmatch("heart failure", tolower(outcome)))){
		"^(Hospital.30.Day.Death).*(Heart.Failure)$"
	} else if(!is.na(pmatch("pneumonia", tolower(outcome)))){
		"^(Hospital.30.Day.Death).*(Pneumonia)$"
	} else {
		stop("invalid outcome")
	}
	
	# Find column number
	columnNumber <- grep(regex, names(data), ignore.case = TRUE)
	
	## Return hospital name in that state with 'num'th lowest 30-day death rate
	dataState <- subset(data, data$State == toupper(state))
	
	sortOrder <- order(dataState[ , columnNumber], dataState$Hospital.Name, na.last = TRUE, decreasing = FALSE)

	if(num == "best"){
		num <- 1
	} else if(num == "worst"){
		num <- nrow(dataState[sortOrder, ][!is.na(dataState[ , columnNumber]), ])
	}
	
	as.character(dataState$Hospital.Name[sortOrder][num])	
}

