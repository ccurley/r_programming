corr <- function(directory, threshold = 0) {
        
        # Assign 3. Hint in the answer set points to calling complete.R (urgh!) and then using 
        # directory and threshold as args, calculate the correlation between complete nitrate and
        # sulfate values. Use ?cor to figure out what's going on there. If no cases match the arg
        # for threshold return a 0.
        #
        # Note - I made a real pig's ear of this on the first couple of passes.
        
        # Get all the files. Carried over from ex 1.
        wk2_files <- list.files(directory, full.name=TRUE)
        
        # Get the complete cases
        source("complete.R")                                            # MUST fix complete.R ... PAINFUL!
        wk1_complete_cases <- complete(directory)                       # Get the complete cases                     
        
        # Get the first column (ID) for complete cases with threshold over in put
        # Cribbed rep(NA,length(wk1_over_threshold )) from stack overflow. Seeding sting with NA values
        # fixing the error in object not foudn
        wk1_over_threshold <- wk1_complete_cases[wk1_complete_cases$nobs > threshold, 1]
        wk1_correlations <- rep(NA,length(wk1_over_threshold ))
        
        # Okay, this is take three to get it down to one loop. It started with four loops....
        # Again, thank GOOGLE.
        for (i in wk1_over_threshold) {
                wk1_data_in <- (read.csv(wk2_files[i]))                 # if > t, read in THAT file i
                
                wk1_complete <- complete.cases(wk1_data_in)             # id the complete cases
                
                wk1_sulfate <- wk1_data_in[wk1_complete, 2]
                wk1_nitrate <- wk1_data_in[wk1_complete, 3]
                wk1_id <- wk1_data_in[wk1_complete, 4]                  # don't need. Dump.
                
                wk1_correlations[i] <- cor(x = wk1_sulfate, y = wk1_nitrate) 
                                                                        # lose the quotes, fool        
        }
        
        wk1_correlations <- wk1_correlations[complete.cases(wk1_correlations)]
                                                                        # this part killed me.
        
        # print(wk1_correlations)
}