# Week 2, Assignment 2
#
# Better and less of a kluge than previous versions, but still not ideal.
# 
# First not ideal item is that I can't get he sapply to work inside loop to only read in the file pertaining
# to the ID. 
#
# Second, I'm still using a for statement, which is not very "r-ish"
# As a result, I calculate for all files in the directory run the rbind for each i in ID.
#
# could be better, but it's a hell of a lot better than my first hideous attempt.

# So, here it is.

complete <- function(directory, id = 1:332) {
        
        directory = "specdata"
        id = 17:23

        # set up the data frame and get the file names
        spititout <- data.frame()                               # I should renamed this 'suck it in'
        wk2_files <- list.files(directory, full.name=TRUE)      # get the files in the directory arg
        wk2_rows <- c(1:length(id))                             # create the row labels
        
        # This will give us a count of the complete cases in the complete cases in the dir
        # into a vector containing named ints.
        out <- sapply(wk2_files, function(f) sum(complete.cases(read.csv(f))))
        
        # create a df with row names, id, and number of observatons corresponding to the input range
        for (i in id) {
                spititout <- rbind(spititout, data.frame(id = i, nobs = out[i], row.names = wk2_rows[1]))
        }
        
        spititout
}