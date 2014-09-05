
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