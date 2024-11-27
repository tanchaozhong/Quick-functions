# Steps of the reduction
#1. Define the strings that you want to reduce
#2. Using loop to calculate the importance of each term.
  #2.1 pick out one "term", and rest term as "background term".
  #2.2 Because "TRUE==1", "FALSE==0", we therefore and directly perform calculation with TRUE and FALSE
  #2.3 add up all the results from the "background term"
  #2.4 compare added results with the result of "term"
  #2.5 the smaller parts are the unique ones that the "term" holds


terms.to.be.reduce = 'climate OR environment OR fish OR green OR health OR landscape OR  pollution OR risk OR systems OR vegetation OR water quality OR wetland OR benefits OR conservation OR culture OR design OR education OR framework OR future OR information OR indicators OR infrastructure OR management OR mode OR planning OR policy OR program OR recreation OR restoration OR services OR strategies OR support'

# the target database
library(litsearchr)
secondary_results <- import_results(file='XXX.ris')
nrow(secondary_results)
papers <- paste(secondary_results[,'title'],secondary_results[,'abstract'])


Term.importance <- function(terms.to.be.reduce,papers){
  or_parts <- unlist(strsplit(terms.to.be.reduce, " OR "))
  
  term.importance <- NULL
  for (word.target in or_parts) {
    
    # calculate the current 
    part_result_target <- grepl(word.target,papers)
    
    # calculate the rest
    part_result_bg <- rep(0,length(papers))
    for(word.bg in Rest.vector(or_parts,word.target)){
      part_result <- grepl(word.bg,papers)
      # simply add the TRUE, and later convert them from '>1' to '1'
      part_result_bg <- part_result_bg + part_result
    }
    part_result_bg[part_result_bg>1]=1
    
    # # which papers are unique
    # index <- c(1:nrow(secondary_results))[part_result_target>part_result_bg]
    # index <- paste(index,collapse = ', ')
    # number of unique papers
    num <- sum(part_result_target>part_result_bg)
    num
    
    term.importance <- c(term.importance,num)
  }
  names(term.importance) <- or_parts
  return(term.importance)
}

# output the importance of each terms
term.importance(terms.to.be.reduce,papers)