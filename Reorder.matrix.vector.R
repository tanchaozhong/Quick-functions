# Reorder the vector / matrix by a reference order

# type can be "matrix" or "vector"
# Against.vector is a column NAME in the matrix


Reorder <- function(to.be.ordered.vector.matrix,type='matrix',against.vector.name=NULL,reference.vector){
  
  # if it is a matrix, but no name of the against vector
  if(type == 'matrix' && is.null(against.vector.name)) print('this is a matrix, where is the NAME of the against vector? what do you want me to do?')

  # Define the vector
  vector <- names(to.be.ordered.vector.matrix)  # using the name of this vector
  if(type == 'matrix') vector <- to.be.ordered.vector.matrix[[against.vector.name]] # using one column of the matrix
  
  # Re-order the vector
  
  ordered.matrix.vector <- NULL

  reference.vector <- Element.grp(reference.vector)
  
  for (name in reference.vector){
    
    # the matrix, and there is a match
    if(type == 'matrix' && sum(vector == name)>0) {
      new.matrix <- to.be.ordered.vector.matrix[vector == name,]
      ordered.matrix.vector <- rbind(ordered.matrix.vector,new.matrix)
 
    }
    
    # the vector, and there is a match
    if(type == 'vector' && sum(vector == name)>0) {
      new.vector <- to.be.ordered.vector.matrix[vector == name]
      ordered.matrix.vector <- c(ordered.matrix.vector,new.vector)
    }
  }
  return(ordered.matrix.vector)

}
