#Order.number

# When the vector are numbers, 
# We sometimes need to order the vector from small to large, or otherwise

Order.number <- function(vector, order='decrease'){
  n=length(vector)
  if(is.numeric(vector)==TRUE){
    new.vector <- NULL
    i=0
    repeat{
      i=i+1
      # the highest value
      max <- max(vector)
      # create a vector to find the index
      array = 1:length(vector)
      # find out the index of the highest value
      index <- array[vector==max]
      # put the highest value in the new vector
      if(order=='decrease') new.vector <- c(new.vector,vector[index])
      if(order=='increase') new.vector <- c(number,new.vector)
      # remove the highest value from the vector
      vector<-vector[vector!=max]
      if(length(vector)==0)break
    }
    return(new.vector)
  } 
  if(is.numeric(vector)==FALSE) return('the vector is not numeric')
}

# Example
vector <- c(4,3,5,6,3,7,5,6,3,5)
Order.number(vector)
