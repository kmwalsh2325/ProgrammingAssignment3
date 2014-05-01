rankall <- function(outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

	# read in DF
	master.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	# check for outcome validity
	if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
		stop("invalid outcome")
	}
	if(num == "best"){num <- 1}

	# get the true column name for the outcome
	outcome.column <- switch(outcome,
							 "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
							 "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
							 "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")

	master.df[, outcome.column] <- as.numeric(master.df[, outcome.column])
	outcome.df <- na.omit(master.df[, c("Hospital.Name", "State", outcome.column)])


	outcome.df <- outcome.df[with(outcome.df, order(outcome.df$State, outcome.df[, outcome.column], outcome.df$Hospital.Name)), ]

	split.states <- split(outcome.df, outcome.df$State)

	final.df <- data.frame()


	for(state in unique(sort(master.df$State))){

		hosp.list <- split.states[[state]][[1]]

		if(num == "worst"){index <- length(hosp.list)}

		hosp.name <- hosp.list[index]
		new.row <- data.frame("hospital" = hosp.name, "state" = state)

		final.df <- rbind(final.df, new.row)
	}

	return(final.df)

} ## end rankall
