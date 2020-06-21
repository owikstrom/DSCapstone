predictWord <- function(phrase, db){
    
    query <- as.character(tokens(phrase,
                                 remove_punct =TRUE, 
                                 remove_symbols=TRUE, 
                                 remove_url=TRUE)) 
  
    for (i in length(db):1) {
      subquery <- tail(query,i) %>% paste(collapse="_")

      result <- db[[i]] %>% filter(input == subquery) %>% arrange(desc(prob))
      
      if(dim(result)[1] >0) break
             
    }   
    result[1,3]
    
# Maybe recalculate for unobserved n-grams
# sample(resultn$type, 1, F, resultsn$prob)       
        
        
} 

queryWord <- function(phrase, answers, db){
  
  query <- as.character(tokens(phrase,
                               remove_punct =TRUE, 
                               remove_symbols=TRUE, 
                               remove_url=TRUE)) 
  
  for (i in length(db):1) {
    subquery <- tail(query,i) %>% paste(collapse="_")
   
    result <- db[[i]] %>% filter(input == subquery) %>% arrange(desc(prob))
    result <- result[which(result$prediction %in% unlist(answers)),]
 
  if(dim(result)[1] >0) break
     
  }   
  result[1,3]
  # Maybe recalculate for unobserved n-grams
  # sample(resultn$type, 1, F, resultsn$prob)       
  
  
} 