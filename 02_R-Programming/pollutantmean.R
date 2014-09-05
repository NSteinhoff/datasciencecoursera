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
		fileNumber <- as.numeric(substring(file, 1, 3))
		if (!(fileNumber %in% id)) {
			next
		}
		
		fileData <- read.csv(paste(directory, file, sep = "/"))
			
		if(is.null(data)){
			data <- fileData
		} else {
			data <- rbind(data, fileData)
		}
	}


	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	
	mean(data[[pollutant]], na.rm = TRUE)
}
