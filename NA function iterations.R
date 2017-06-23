# most recent version, 3 cols: 
na_cleaning_all <- function(x, na.limit, col1.start, col1.end){
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

# This one does just one span of columns
na_cleaning <- function(x, na.limit, col1.start, col1.end){
  temp <- x
  for(i in 1:(nrow(x))){
    if(length(x[i,(which(is.na(x[i,col1.start:col1.end])))]) > na.limit){
      temp[i,col1.start:col1.end] <- NA
    } 
  }
  test_data <<- temp
}


# This will assign date to the name. 
assign(paste0(Sys.Date(), ".data"), value = my_data)







# NA cleaning function that sorta works

function(x, na.limit, col1.start, col1.end, col2.start, col2.end, col3.start, col3.end){
  temp <- x
  for(i in 1:(nrow(x))){
    if(length(x[i,(which(is.na(x[i,col1.start:col1.end])))]) <= na.limit&
       length(x[i,(which(is.na(x[i,col2.start:col2.end])))]) <= na.limit&
       length(x[i,(which(is.na(x[i,col3.start:col3.end])))]) <= na.limit){
      temp[i,(which(is.na(temp[i,])))] <- 999
    } 
  }
  test_data <<- temp[complete.cases(temp),]
}

# this one returns data frame with proper ARS #s removed, then complete cases, but still has 999
data_cleaning <- function(x, na.limit, col1.start, col1.end, col2.start, col2.end, col3.start, col3.end){
  temp <- x
  for(i in 1:(nrow(x))){
    if(length(x[i,(which(is.na(x[i,col1.start:col1.end])))]) <= na.limit&
       length(x[i,(which(is.na(x[i,col2.start:col2.end])))]) <= na.limit&
       length(x[i,(which(is.na(x[i,col3.start:col3.end])))]) <= na.limit){
      temp[i,(which(is.na(temp[i,])))] <- 999
    } 
  }
  test_data <<- temp[complete.cases(temp[col1.start:col3.end]),]
}


############# THIS ONE TAKES THE CAKE ################################################################

data_cleaning <- function(x, na.limit, col1.start, col1.end, col2.start, col2.end, col3.start, col3.end){
  temp <- x
  for(i in 1:(nrow(x))){
    if(length(x[i,(which(is.na(x[i,col1.start:col1.end])))]) <= na.limit&
       length(x[i,(which(is.na(x[i,col2.start:col2.end])))]) <= na.limit&
       length(x[i,(which(is.na(x[i,col3.start:col3.end])))]) <= na.limit){
      temp[i,(which(is.na(temp[i,])))] <- 999
    } 
  }
  temp1 <<- temp[complete.cases(temp),]
  temp1[temp1==999] <- NA
  test_data <<- temp1
}

# Older pieces



length(airquality[5,(which(is.na(airquality[5,3:6])))])

which(is.na(airquality[5,]))
vector_of_NA_sums(airquality)   
vector_of_NA_sums(testdata)
table(vector.a)
hist(vector.a)
sum(vector.a <= 7)/length(vector.a)
cbind_vector_to_data(airquality)
rows_to_NAs(airquality,2)
rows_to_NAs(testdata, 7)
huzzah(airquality, 1)
huzzah(testdata, 7)


###### THESE ONES WORK ###################
vector_of_NA_sums <- function(x){
  vector.a <<- as.numeric(rowSums(is.na(x)))
}

cbind_vector_to_data <- function(x){
  vector.b <- as.numeric(rowSums(is.na(x)))
  test_cbind <<- as.data.frame(cbind(x, vector.b))
  View(test_cbind)
}
##########################################

rows_to_NAs <- function(x, l){
  vector.c <- as.numeric(rowSums(is.na(x)))
  test_cbind <- as.data.frame(cbind(x, vector.c))
  for(i in 1:(nrow(x))){
    if(test_cbind$vector[i] >=l){
      test_cbind[i,] <- NA
    } 
  }
  test_cbind <<- test_cbind
}
##########################################

rows_to_NAs <- function(x, l){
  vector.c <- as.numeric(rowSums(is.na(x)))
  test_data <- as.data.frame(cbind(x, vector.c))
  for(i in 1:(nrow(x))){
    if(test_data$vector[i] <= l){
      test_data[i,(which(is.na(test_data[i,])))] <- 999
    } 
    else 
      test_data[i,] <- NA
  }
  test_data <<- test_data
}
##########################################

huzzah <- function(x, l){
  vector.c <- as.numeric(rowSums(is.na(x)))
  test_data <- as.data.frame(cbind(x, vector.c))
  for(i in 1:(nrow(x))){
    if(test_data$vector[i] <= l){
      test_data[i,(which(is.na(test_data[i,])))] <- 999
    } 
    else 
      test_data[i,] <- NA
  }
  test_data <<- test_data[complete.cases(test_data),]
}
##########################################

srs_stuff <- function(x, na.limit, col.start, col.end){
  temp <- x
  for(i in 1:(nrow(x))){
    if(length(x[i,(which(is.na(x[i,col.start:col.end])))]) <= na.limit){
      temp[i,(which(is.na(temp[i,])))] <- 999
    } 
  }
  test_data <<- temp[complete.cases(temp),]
}
##########################################




