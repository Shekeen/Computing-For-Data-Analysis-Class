complete <- function(directory, id = 1:332) {
	completecases <- vector()
	for (i in id) {
		data <- getmonitor(i, directory)
		completecases <- c(completecases, length(data$s[complete.cases(data$n, data$s)]))
	}
	return(data.frame(id = id, nobs = completecases))
}