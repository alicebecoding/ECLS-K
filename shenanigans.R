# other shenanigans

# Other diagnostics and shenanigans

check.predictorMatrix<-function(predictorMatrix, nmis, nvar)
{
  #	checks the predictorMatrix
  #  it can change the predictormatrix
  #  stops the program if an error is found
  #
  if(!is.matrix(predictorMatrix)) stop("Argument predictorMatrix should be a square matrix.")
  if(nvar != nrow(predictorMatrix) | nvar != ncol(predictorMatrix)) stop
  ("Argument predictorMatrix has improper dimensions.")
  if(sum(diag(predictorMatrix) != 0)) stop("The diagonal of predictorMatrix may contain only zeroes.")
  for(j in 1:nvar) {
    if(!any(predictorMatrix[j,]) & any(predictorMatrix[,j]) & nmis[j]>0) stop
    (paste("Column ", j, " is used, has missing values, but is not imputed"))
    if(nmis[j]==0 & any(predictorMatrix[j,])) {
      # warning(paste("Row ",j," of predictorMatrix is set to zero"))
      predictorMatrix[j,] <- rep(0, nvar)
    }
  }
  return(predictorMatrix)
}
nmis <- apply(is.na(may_data),2,sum)
check.predictorMatrix(pred, nmis, 108)
eclsk
str(pred)
pred_test <- blank_imp_form$predictorMatrix
str(pred_test)

?mice.mids
is.mids(eclsk.imp1)



######################################################################################################
######################################################################################################
######################################################################################################



# Notes on imputed variables: 

# Household income: 2397 imputed, 81 missing. 
# Education P1 Y1:  316 imputed, 83 missing
# Education P2 Y1:  483 imputed, 88 missing
# Prestige P1 Y1:   257 imputed, 40 missing
# Prestige P2 Y1:   417 imputed, 22 missing

# How many would complete case analysis (of just the above variables) remove?
ses_cols
ses_comp <- ses_cols[c(complete.cases(ses_cols)),]
# Leaves n=13399



######################################################################################################
######################################################################################################
######################################################################################################



# not helpful: md.pat <- md.pattern(testdata)

# Correlates *whether or not* a variable is NA with the value of another variable. 
a <- round(cor(y=test_data_2_z, x=!is.na(test_data_2_z), use="pair"), 3)
which(a>0.1)
# not really sure what to do with this


