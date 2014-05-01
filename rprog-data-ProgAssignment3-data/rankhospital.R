rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate

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
	outcome.df <- na.omit(outcome.df[, c("Hospital.Name", outcome.column)])
	outcome.df <- outcome.df[with(outcome.df, order(outcome.df[, outcome.column], outcome.df[, "Hospital.Name"])), ]

	# establish rank
	# rename middle column
	outcome.df$Rank <- 1:nrow(outcome.df)
	colnames(outcome.df)[2] <- "Rate"

	if(num == "best"){num <- 1}
	else if(num == "worst"){num <- nrow(outcome.df)}

	return(outcome.df[num, "Hospital.Name"])

	## establishes weird structure with na.omit. the rownames tag along and so do not reset correctly




} ## end rankhospital