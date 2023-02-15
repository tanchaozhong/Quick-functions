# When perform CCA, the order of the column matters
# The function Reorder(matrix) here is to complete reorder the matrix by column

Reorder <- function(matrix){
  # how many column
  num <- ncol(matrix)
  tag <- colnames(matrix)
  # a random permutation
  ordered <- sample(1:num)
  i=0; matrix.ordered <- NULL; tag.ordered <- NULL
  repeat{
    i=i+1
    matrix.ordered <- cbind(matrix.ordered,matrix[,ordered[i]])
    tag.ordered <- c(tag.ordered,tag[ordered[i]])
    if (i==num) break
  }
  colnames(matrix.ordered) <- tag.ordered
  rownames(matrix.ordered) <- rownames(matrix)
  return(matrix.ordered)
}
