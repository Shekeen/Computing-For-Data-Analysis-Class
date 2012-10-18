corr <- function(directory, threshold = 0) {
	correlations <- vector()
	completecases <- complete(directory)
	for (id in completecases[completecases$n > threshold, ]$i) {
		data <- getmonitor(id, directory)
		correlations <- c(correlations, cor(data$s, data$n, use = "na.or.complete"))
	}
	return(correlations)
}