# Week 2, Assignment 2
#
# I bet my life there is a better way to do this than loops, but you go with what you know
# I use two data frames, I bet it can be done in one. Borrowing from pollutantmean.R, I grab
# the files out of the specdata drive based on the range of inputs taken as args
#
# That's the first for loop, and I bet you can do this in one line.
# 
# The second loop then subsets the data in the dataframe to IDs that match the i of the ID.
# The subset is pushed to newdata vector. The vector is processed for complete cases. I nrow
# the newdata subsetted for good, and then I spit it out into a new data frame.
#
# I'm sure this is a total kluge, but all I know are setsets, data frames, and conditionals.
# So, here it is.

complete <- function(directory, id = 1:332) {
        
       # set up the data frame and get the file names
        spititout <- data.frame()                               # I should renamed this 'suck it in'
        wk2_df    <- data.frame()                               # initialize the data frame
        wk2_files <- list.files(directory, full.name=TRUE)      # get the files in the directory arg
        
        # get a row at a time per id in i and build a dataset, find the complete cases, get a count
        # of the good rows, and then pass the ID for the row and the nrow count into a new data frame
        #
        # I bet there's a more efficient way to get and count the complete cases.
        for (i in seq_along(id)) {
                wk2_df <- rbind(wk2_df, read.csv(wk2_files[i])) # read in just ID sequence to d.f.
                # good <- wk2_df(!is.na[wk2_df$sulfate] & wk2_df(!is.na[wk2_df$nitrate]))
                good <-complete.cases(wk2_df)                   # which are complete, e.g. 'good'
                # out <- nrow(good)
                out <- nrow(wk2_df[good, ])                     # now count the rows that are 'good'
                spititout = rbind(spititout, data.frame(id = i, nobs = out))
        }
        
        # It's all in the name.
        spititout
}