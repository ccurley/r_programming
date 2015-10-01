# rankhospital.R
# Coursera - Introduction to R
# Assignment 3 - Week 4 - Part 2
#
# The hardest part of this assignment is reading the instructions. 
# 
# The first part of the exercise is taken directly from 'best', which grabs the data and puts it into a
# subset by state. Find commented code in best.R in this repo.
#
# I know cases operations aren't very R-ish, according to the purists, but this is what you get when you
# started with perl. The only realy R-ish thing I did in 'best.R' was grab the states.abb list... and that didn't
# work because the .csv didn't have data for all 50 states plus DC.
#
# So, 1 is alwasy the start, and nrow() will give me the last. Otherwise, the num passed in as an arg will
# be the row ID. And, all I need to do is dump the name, as in best. The bit with the ties means that the state
# set needs to be sorted alpha on name.

rankhospital <- function(state, outcome, num = "best") {
               
        my.file <- paste(getwd(), "/hospdata/outcome-of-care-measures.csv", sep="")
        df <- read.csv(my.file, colClasses = "character", na.strings="Not Available")
        
        valid.state <- sort(unique(df[,7])) 
        check.state <- grep(state, valid.state) 
        if (!length(check.state)) {    
                stop("invalid state")
        }
        
        valid.outcome = c("heart attack", "heart failure", "pneumonia")
        check.outcome <- grep(outcome, valid.outcome)
        if (!length(check.outcome)) { 
                stop("invalid outcome")
        }
        
        f.col.name <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        col.name <- f.col.name[match(outcome,valid.outcome)]
        
        df.state <- df[df$State == state,]
        df.state <- subset(df.state, df.state[, col.name] != "Not Available")
        
        # okay. Now for the new stuff. We need to sort the data     
        s.df.state <- df.state[order(as.numeric(df.state[[col.name]]),
                                          df.state[["Hospital.Name"]],
                                          decreasing = FALSE), ]

        if ( num == "best") {                                     # if best, select the first row
                s.df.state[1,"Hospital.Name"]
        } else if ( num == "worst") {                             # if worst, select the last row
                s.df.state[nrow(s.df.state),"Hospital.Name"]
        } else {                                                  # if a rank, select the rank
                s.df.state[num,"Hospital.Name"]
        }
        
}

# Tested for 
# rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# rankhospital("MN", "heart attack", "worst")
# [1] "HEALTHEAST ST JOHN'S HOSPITAL"
# rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# rankhospital("MN", "heart attack", 5000)
# [1] NA

# end of rank hospital
