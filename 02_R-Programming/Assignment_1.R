pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	pollutant <- tolower(pollutant)
	pollutants <- c("sulfate", "nitrate")
	if(!(pollutant %in% pollutants)){
		stop(paste("Invalid pollutant.", "Valid pollutants are", paste(pollutants, collapse = ", "), sep = " "))
	}
	
	# init data
	data <- NULL

	# get all file names in the directory
	directory <- paste("Data", directory, sep = "/")
	files <- list.files(directory)
	
	# read data from selected files
	for (file in files) {
		
		# check if file should be included
		fileNumber <- as.numeric(substring(file, 1, 3))
		if (!(fileNumber %in% id)) {
			next
		}
		# read from file
		fileData <- read.csv(paste(directory, file, sep = "/"))
		
		# populate main data frame	
		if(is.null(data)){
			data <- fileData
		} else {
			data <- rbind(data, fileData)
		}
	}

	# calculate mean
	mean(data[[pollutant]], na.rm = TRUE)
}

complete <- function(directory, id = 1:332){
	# init data
	data <- NULL

	# get all file names in the directory
	directory <- paste("Data", directory, sep = "/")
	files <- list.files(directory)
	
	# read data from selected files
	for (file in files) {
		
		# check if file should be included
		fileNumber <- as.numeric(substring(file, 1, 3))
		if (!(fileNumber %in% id)) {
			next
		}
		
		# read from file
		fileData <- read.csv(paste(directory, file, sep = "/"))

		# populate main data frame	
		if(is.null(data)){
			data <- data.frame(ID = fileNumber, nobs = sum(complete.cases(fileData)))
		} else {
			data <- rbind(data, data.frame(ID = fileNumber, nobs = sum(complete.cases(fileData))))
		}
	}

	data[match(id, data$ID), ]
}

corr <- function(directory, threshold = 0){
	directory <- paste("Data", directory, sep = "/")
	files <- list.files(directory)
	
	correlations <- numeric()
	for(file in files){
		fileData <- read.csv(paste(directory, file, sep = "/"))
		
		if(sum(complete.cases(fileData)) <= threshold){
			next
		} else {
			correlation <- cor(fileData$nitrate, fileData$sulfate, use = "complete.obs")
			correlations <- c(correlations, correlation)
		}
	}
	
	correlations
}