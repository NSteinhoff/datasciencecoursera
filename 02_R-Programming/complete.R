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
