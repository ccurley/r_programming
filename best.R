# best.R
# Coursera - Introduction to R
# Assignment 3 - Week 4 - Part 1
#
# This is less an exercise in finding the minimum as it is getting the data into the
# correct type to perform an operation. The data comes in as characters. To perfom the 
# data operations, the data needs to be treated as numeric vectors.
#
# Don't use the states.abb internal list to build the logical validation for the state:
# it will return a vector with a length that doesn't match the data in the outcome file.
# So, it's possible using ALL the states to get a pass on the valid state but not have 
# hosp data for that state. Instead, you have to validate from col 7
#
# Assignment instructions say to remove the "Not Available" data from the data by state when
# determining the min function. I didn't find this impacted the which.min, since it only threw a 
# warning. But, I pulled it out anyway.

best <- function(state, outcome) {
        
        # Get the outcome data from file
        # Gotcha! Must add the na.strings arg or there will be an error the function tries to find the min,
        # since it can't perform min against a char string.        
        my.file <- paste(getwd(), "/hospdata/outcome-of-care-measures.csv", sep="")
        data <- read.csv(my.file, colClasses = "character", na.strings="Not Available")
        
        ## Check if the State and outcomes are valid. Repeat for outcomes.
        # stackexchange recommends using %in% e.g. if (!state %in% ValidState)
        # validState <- state.abb                               # Can't use. Outcome doesn't include all states
        validState <- sort(unique(data[,7]))                    # Put states in a vect
        check.state <- grep(state, validState)                  # If state is a validState, store 1 in log vect
        if (!length(check.state)) {                             # If check_state is empty, stop
                 stop("invalid state")
        }
        
        validOutcome = c("heart attack", "heart failure", "pneumonia")
        check.outcome <- grep(outcome, validOutcome)
        if (!length(check.outcome)) { 
                stop("invalid outcome")
        }
        
        # Match the outcome arg to a column name in the data frame and store it for use later.
        # grep returns logical. Use match on the fullColName to return the char str. ?match
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,validOutcome)]
        
        # Return hospital name in that state with lowest 30-day death rate
        # stackoverflow says to use the data.table lib. Got the table data easily, but not hosp name.
        # R Bloggers says to use which.min. That worked better.
        # http://www.r-bloggers.com/r-tip-finding-the-location-of-minimum-and-maximums/ 
        
        data.state <- subset(data, data$State == state)         # get that state rows from data
        data.state <- subset(data.state, data.state[, colName] != "Not Available") 
                                                                # remove the outcome "not availables"
        tmp <- which.min(as.double(data.state[,colName]))       # which row of THAT state has the min outcome
        
        data.state[tmp,"Hospital.Name"]                         # Return the name of THAT hospital 
}

# tested against
# best("SC", "heart attack") - "MUSC MEDICAL CENTER"
# best("NY", "pneumonia") - "MAIMONIDES MEDICAL CENTER"
# best("NN", "pneumonia") - Error in best("NN", "pneumonia") : invalid state

# end of best.R