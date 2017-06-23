# Function to clean NAs from ARS data

testdata <- may_data

# writing a function to remove rows if a certain # of NAs in each block
# I think I can either make child ID and each timepoint for ARS as separate data frames, do the function
# on each, then combine by child ID and only keep the ones that have all data, 
# or I need to write a function that analyzes each set of columns within the same data frame. 

# steps: 
# Set threshold for rejection. to start, 5 NAs means reject. n_reject <- 5  
# For each block, 12:19, 20:27, 28:35, tally up # of NAs. 
# if(na_sum >= 5) # then... add row # to a vector, perhaps? Then use that vector to eliminate rows. 


# one set of cols
na_cleaning <- function(x, na.limit, col1.start, col1.end){
  temp <- x
  for(i in 1:(nrow(x))){
    if(length(x[i,(which(is.na(x[i,col1.start:col1.end])))]) > na.limit){
      temp[i,col1.start:col1.end] <- NA
    } 
  }
  test_data <<- temp
}


# all 3 sets of cols
na_cleaning_all <- function(x, na.limit, col1.start, col1.end, col2.start, col2.end, col3.start, col3.end){
  temp <- x
  for(i in 1:(nrow(x))){
    if(length(x[i,(which(is.na(x[i,col1.start:col1.end])))]) > na.limit&
       length(x[i,(which(is.na(x[i,col2.start:col2.end])))]) > na.limit&
       length(x[i,(which(is.na(x[i,col3.start:col3.end])))]) > na.limit){
      temp[i,c(col1.start:col1.end,col2.start:col2.end,col3.start:col3.end)] <- NA
    } 
  }
  test_data <<- temp
}


# This will assign date to the name. 
assign(paste0(Sys.Date(), ".data"), value = my_data)

# testing area
may_six_test <- na_cleaning_all(may_data, 4, col1.start = 12, col1.end = 19)


#####################################################################################################
