rankall <- function(outcome, num = "best") {
	full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	column <- if (outcome == "heart attack") {
		full_data[, 11] <- as.numeric(full_data[, 11])
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	} else if (outcome == "heart failure") {
		full_data[, 17] <- as.numeric(full_data[, 17])
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	} else if (outcome == "pneumonia") {
		full_data[, 23] <- as.numeric(full_data[, 23])
		"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	} else {
		stop("invalid outcome")
	}

	data_by_state <- split(full_data[, c("Hospital.Name", "State", column)], full_data$State)

	rank_hospital <- function(state_data, num) {
		ordered_state_data <- order(state_data[3], state_data$Hospital.Name, na.last=NA)
		
		if (num == "best") {
     	   state_data$Hospital.Name[ordered_state_data[1]]
	    } else if (num == "worst") {
    	   state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]]
	    } else if (is.numeric(num)) {
    	   state_data$Hospital.Name[ordered_state_data[num]]
	    } else {
    	    stop("invalid num")
	    }
	}

	pre_result <- lapply(data_by_state, rank_hospital, num)
	
	data.frame(hospital = unlist(pre_result), state = names(pre_result), row.names = names(pre_result))
}