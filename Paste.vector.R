Paste.vector <- function(vector){
  # paste all the elements within a vector
  
  i=0;former<-vector[i]
  repeat{
    i=i+1
    former <- paste(former,vector[i+1], sep='; ')
    result <- former
    if(i==length(vector)-1)break
  }
return(result)
}
