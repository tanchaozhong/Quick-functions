# when calculate the Mean and SE, we often need to separate the data to different groups.
# The function Mean.SE.grp is based on function Mean.SE() and Element.grp(),
# to calculate mean se among different groups

# Matrix is site-variable matrix, with row names are site, col names are variables
# grp is a vector

# Result, row names are varirables, col names are the groups
Mean.SE.grp <- function(matrix,grp){
  title <- Element.grp(grp)
  n <- length(title)
  i=0;result.F <- NULL
  repeat{
    i=i+1
    data <- matrix[grp==title[i],]
    result <- Mean.SE(data)
    result.F <- cbind(result.F,result)
    if(i==n)break
  }
  return(result.F)
}

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

Element.grp <- function(data){
  
  element.F <- NULL
  repeat{
    
    element <- data[1]
    
    # create a index from 1 to n
    n <- length(data);vector <- 1:n
    
    # get the index of the elements that are same as the first one
    index <- vector[data==element]
    
    # remove the same elements, and formed a new data list
    data <- data[-index]
    
    # record the elements
    element.F <- c(element.F,element)
    
    if (length(data)==0)break
  }
  return(element.F)
}