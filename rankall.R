# rankall.R
# Coursera - Introduction to R
# Assignment 3 - Week 4 - Part 3
#
# This is bascially the same script as the rankhosptial, with the operation in the if/else if/else state-
# ment slightly altered to build a data frame. As always, I overcomplicated that step with an rbind call in
# my first pass -- which almost worked, except that I was getting NA in the state name where hospital was NA
# I switched approaces based on the cachematrix.R exercise -- where I build the data frame from two separate
# lists -- a lost of hospitals corresponding to the row called and the list of states.
#
# 

rankall <- function(outcome, num = "best"){
        
        # get the file... yeah, I don't need the paste statement, but I was going build some logic into it
        my.file <- paste(getwd(), "/hospdata/outcome-of-care-measures.csv", sep="")
        
        # seed the list for hospital data and the data frame for ranking by state
        my.hospital<-character()
        df.state.rank <- data.frame()
        
        # read it in
        df <- read.csv(my.file, colClasses = "character", na.strings="Not Available")
        
        # get the list of states
        my.states <- sort(unique(df[, "State"]))
        
        # validate the outcome
        valid.outcome = c("heart attack", "heart failure", "pneumonia")
        check.outcome <- grep(outcome, valid.outcome)
        if (!length(check.outcome)) { 
                stop("invalid outcome")
        }
        
        # match the column names in the df to the outcome arg
        f.col.name <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        col.name <- f.col.name[match(outcome,valid.outcome)]
        
        # now... for each of the states, subset THAT state, order THAT subset, and depending on the 
        # num passed in as an arg, push the hosptial name to an indexed list by i in my.states
        for (i in seq_along(my.states)) {
                df.state <- subset(df, df$State == my.states[i])
                s.df.state <- df.state[order(as.numeric(df.state[[col.name]]),
                                                       df.state[["Hospital.Name"]],
                                                       decreasing = FALSE,
                                                       na.last = NA), ]
                if ( num == "best") {                                     
                        my.hospital[i] <- s.df.state[1,"Hospital.Name"]
                } else if ( num == "worst") {                            
                        my.hospital[i] <- s.df.state[nrow(s.df.state),"Hospital.Name"] 
                } else {                                                  
                        my.hospital[i] <- s.df.state[num,"Hospital.Name"] 
                }
                
        } # end of for
        
        # build a data frame and spit it out
        df.out <- data.frame(hospital=my.hospital,state=my.states,row.names=my.states)
        df.out
        
} # end of function

# tested with 
#
# > head(rankall("heart attack", 20), 10)
#                               hospital state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                <NA>    DE
# FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL
# > tail(rankall("pneumonia", "worst"), 3)
#                                       hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# WV                     PLATEAU MEDICAL CENTER    WV
# WY           NORTH BIG HORN HOSPITAL DISTRICT    WY
# > head(rankall("heart failure"), 10)
# hospital state
# AK                     SOUTH PENINSULA HOSPITAL    AK
# AL           GEORGE H. LANIER MEMORIAL HOSPITAL    AL
# AR VA CENTRAL AR. VETERANS HEALTHCARE SYSTEM LR    AR
# AZ         BANNER GOOD SAMARITAN MEDICAL CENTER    AZ
# CA            CENTINELA HOSPITAL MEDICAL CENTER    CA
# CO                    PARKER ADVENTIST HOSPITAL    CO
# CT                      YALE-NEW HAVEN HOSPITAL    CT
# DC                          PROVIDENCE HOSPITAL    DC
# DE            BAYHEALTH - KENT GENERAL HOSPITAL    DE
# FL    FLORIDA HOSPITAL HEARTLAND MEDICAL CENTER    FL
# > tail(rankall("heart failure"), 10)
#                                                             hospital state
# TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# TX                                        FORT DUNCAN MEDICAL CENTER    TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# VA                                          SENTARA POTOMAC HOSPITAL    VA
# VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# VT                                              SPRINGFIELD HOSPITAL    VT
# WA                                         HARBORVIEW MEDICAL CENTER    WA
# WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# WV                                         FAIRMONT GENERAL HOSPITAL    WV
# WY                                        CHEYENNE VA MEDICAL CENTER    WY