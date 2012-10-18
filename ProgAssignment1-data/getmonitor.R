getmonitor <- function(id, directory, summarize = FALSE) {
	## load file named %id%.csv in %directory%
	## %summarize% indicates whether the summary should be printed
	if (id < 10) prefix <- "00"
	else if (id < 100) prefix <- "0"
	else prefix <- ""
	data <- read.csv(paste0(directory, "/", prefix, id, ".csv"));
	if (summarize) summary(data);
	return(data)
}