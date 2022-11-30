# The function Left() is mimic the function left() in excel
# To take the number of letters from the left of the string.

# spe.name is a vector
# n is a number

Left <- function(spe.name,n)
{
  j=0;initial <- NULL
  repeat{
    j=j+1
    name <- spe.name[j]
    new.name <- strsplit(name,"")[[1]]
    new.name <- paste(new.name[1:n],collapse='')
    initial <- c(initial,new.name)
    if(j==length(spe.name))break
  }
  return(initial) # initial of the species
}