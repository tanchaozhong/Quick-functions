# We often does not know how many elements in a group
# therefore this function element.grp() is to find out how many elements are there

# data is a vector


# Define how many elements in a group
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

data <- c(1,2,3,4,5,6,7)
Element.grp(list)
