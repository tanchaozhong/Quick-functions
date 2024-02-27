Remove.duplicate.vector <- function(vector){
  # find one duplicate, remove one, until all the elements in the list are unique
  vector.new <- vector
  i=0
  repeat{
    i=i+1
    index<- 1:length(vector.new)
    
    # check the entry one by one
    x = vector.new[i]
    
    # find the duplicate index.indi
    index.indi <- index[vector.new == x]
    
    # remain the first index we found
    if(length(index.indi)>1) {
      index.indi.rm <- index.indi[-1]
      # remove the rest index
      vector.new <- vector.new[-index.indi.rm]
    } 
    
    if(i==length(vector.new))break
  }
  return(vector.new)
}
