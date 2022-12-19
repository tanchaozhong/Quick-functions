# Function boxplot() has to have grp factors. it is a little annoying every time we have to create a grp factor.
# Function Boxplot.grp() here you just need to input the data list and grp factor
# such as grp = c('lake 1', 'lake 2') that corrresponds with list.of.data

# data in the list.of.data need to be vector or distance


Boxplot.grp <- function(list.of.data,grp){
  n = length(list.of.data)
  
  i=0;group <- NULL;data.F <- NULL
  repeat{
    i=i+1
    data <- list.of.data[[i]]
    
    # transform the data into vactors
    data.F <- c(data.F,as.vector(data))

    # enlarge the grp factor
    group <- c(group,rep(grp[i],length(data)))
    if(i==n)break
  }
  
  # sort the group based on what you want
  group <- factor(group,levels = grp)
  boxplot(data.F~group)
  
}