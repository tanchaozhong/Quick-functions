#Merge()
# A vector have only serveral kind of strings or numbers, but every letter occurred number of times
# We need to count how many of each string or number
# by the way to order the result by the parameter 'order'

Merge <- function(vector,order){
  level <- Element.grp(vector)
  result <- NULL
  i=0
  repeat{
    i=i+1
    count <- length(vector[vector==order[i]])
    #if(isTRUE(level==order[i])==FALSE) count=0
    result <- c(result,count)
    if(i==length(order))break
  }
  result <- data.frame(level=order,count=result)
  n1 = length(order); n2 = length(level)
  if(isTRUE(n1==n2)==FALSE) print('the length of the order you gave is not equal to the length of the level')
  
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
