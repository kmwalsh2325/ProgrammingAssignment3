best <- function(state, outcome) {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate
	
	# read in DF
	outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	# check for valid arguments
	if(!(state %in% unique(outcome.df$State))) {
		stop("invalid state")
	}
	if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
		stop("invalid outcome")
	}

	# only use specified state data
	outcome.df <- outcome.df[outcome.df$State == state, ]

	# get the true column name for the outcome
	outcome.column <- switch(outcome,
							 "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
							 "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
							 "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")

	# reassign class to numeric and take min of the outcome column
	outcome.df[, outcome.column] <- as.numeric(outcome.df[, outcome.column])
	min.mortality <- min(outcome.df[, outcome.column], na.rm=TRUE)

	# create and sort a name list with this mortality rate
	names.list <- as.list(unique(sort(outcome.df[outcome.df[, outcome.column] == min.mortality, "Hospital.Name"])))

	# return first entry
	names.list[[1]]

} ## end best
