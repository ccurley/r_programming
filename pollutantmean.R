# Week 2, Assignment 1
#
# This function takes three arguments from the command line and uses them to calculate a mean, based
# on the range of data files specified by the users. 
# The args are:
#       directory - the directory from the workdir where the source data is found
#       pollutant - the variable measured, in this case "sulfate" or "nitrate"
#       id        - the range of file id in csv filename correstponds with the id in the row data. The user
#                   can specify 70:72, 1:10 or default to 1:332]
# The fuction pushes to wk2_files pushs the files names to a vectoy, using the directory arg to specify the
# diretory where the files are located
# 
# The function then iterates through the id range to push specific elements of wk2_files in a csv read that
# stuffs the contents into the data frame.
#
# The function then reads that data frame to perform a median operation on a subset of the dataframe determined
# either by the column header nitrate or sulfate
#
# WARNING: not a robust function. Does not validate argument. Throws standard errors

pollutantmean <- function(directory, pollutant, id = 1:332) {
        # set up the data frame and get the file names
        wk2_df    <- data.frame()                           # initialize the data frame
        wk2_files <- list.files(directory, full.name=TRUE)  # get the files in the directory passed in as an arg
        
        # build the data frame with contents from each file in wk2.files
        for (i in id) {
                wk2_df <- rbind(wk2_df, read.csv(wk2_files[i]))              
        }
        
        # build a conditional to understand the input, calculate the mean, throw an error if neither
        if (pollutant == "sulfate") {
                mean(wk2_df$sulfate, na.rm=TRUE)
        } else if (pollutant == "nitrate") {
               mean(wk2_df$nitrate, na.rm=TRUE)  
        } else {
               message("WARNING: Pollutant Not Recognized.")
        }
}