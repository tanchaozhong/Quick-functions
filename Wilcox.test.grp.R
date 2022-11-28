# The function Wilcox.test() can only compute two groups per time
# the function Wilcox.test.grp() is to compute wilcox.test() among over 2 groups.

# the data is a vector
# the grp is a vector

Wilcox.test.grp <- function(data,grp){

  # See how many levels of the grp
  level <- Element.grp(grp) 
  
  n <- length(level)
  # built two matrix to store the statistics of 'W' and 'P value'

  i=0;result.F.F <- NULL
  repeat{
    i=i+1
    
    j=i;result.F <- NULL
    repeat{
      j=j+1
      
      test <- wilcox.test(data[grp==level[i]],data[grp==level[j]])
      t.statistic <- test$statistic
      p.value <- test$p.value
      result <- c(t.statistic,p.value)
      result.F <- c(result.F,result)
      if(j==n)break
    }
    result.F <- c(rep(0,(i-1)*2),result.F)
    result.F.F <- rbind(result.F.F,result.F)
    if(i==n-1)break
  }
  
  rownames(result.F.F) <- level[-n]
  result.F.F <- rbind(
    (rep(c('w','p'),n-1))
    ,result.F.F)
  
  i=0;colname <- NULL
  repeat{
    i=i+1
    result <- c(level[i+1],'')
    colname <- c(colname,result)
    if(i+1 == n) break
  }
  
  colnames(result.F.F) <- colname
  
  return(result.F.F)
}

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

data <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)
grp <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)
Wilcox.test.grp(data,grp)
