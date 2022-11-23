# sd() cannot calculate matrix
# calculate Mean and SE is littile trouble..]

# so function Mean.SE() here is to calcualte Mean and SE of a matrix

# The matrix is a usual site-variable table:
    # variables are the colname of the matrix 
    # Site name are the rowname of the matrix

Mean.SE <- function(data){
  # Mean
  nrow <- nrow(data)
  mean <- colSums(data)/nrow
  
  # SD
  ncol <- ncol(data)
  i=0;result.F <- NULL
  repeat{
    i=i+1
    result <- sd(data[,i])
    result.F <- c(result.F,result)
    if (i == ncol) break
  }
  
  # SE
  SE <- result.F/(nrow^(1/2))
  
  result <- cbind(mean,SE)
  return(result)
}
