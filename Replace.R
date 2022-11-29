# Sometimes we need to replace the strings within a group factor
# The function Replace() is 
# to replace a vector with another vector with the same number

# 'original' is a vector of data 
# 'replace' is a vector of level factor
# the change you want to make has to be associated.
# For example, Element.grp(original)[1] --> replace[1]

Replace <- function(original,replace){
  old <- Element.grp(original)
  new <- NULL
  # replace the grp var to color 
  i=0
  repeat{
    i=i+1
    
    # For each element in original
    # Check which they are
    # And then replace then accordingly
    j=0
    repeat{
      j=j+1
      if(original[i] == old[j]) new[i] <- replace[j]
      if(j==length(old))break
    }
    
    if(i == length(original) ) break
  }
  
  # record which replaced which
  i=0; alter.F <- NULL
  repeat{
    i=i+1
    alteration <- c(replace[i],'-->',old[i])
    alter.F <- rbind(alter.F,alteration)
    if(i==length(replace))break
  }
  alter.F
  
  result <- list(alter.F,new)
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