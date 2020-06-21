predictWord <- function(phrase, db){
    require(quanteda)
    require(dplyr)
  
    query <- tolower(as.character(tokens(phrase,
                                 remove_punct =TRUE, 
                                 remove_symbols=TRUE, 
                                 remove_url=TRUE))) 

    for (i in length(db):2) {
      subquery <- tail(query,i-1) %>% paste(collapse="_")
      result <- db[[i]] %>% filter(input == subquery) %>% arrange(desc(prob))

      if(dim(result)[1] >0){
        break
      } else if(i==2) {
        result <- db[[1]] %>% filter(input == "") %>% arrange(desc(prob))
        result <- result[sample(nrow(result), 1, replace=TRUE, prob = result$prob),]
      }
    }
    result[1,2]
} 


predictWordDetailed <- function(phrase, db){

  require(quanteda)
  require(dplyr)
  
  query <- tolower(as.character(tokens(phrase,
                                       remove_punct =TRUE, 
                                       remove_symbols=TRUE, 
                                       remove_url=TRUE))) 

    for (i in length(db):2) {
    subquery <- tail(query,i-1) %>% paste(collapse="_")
    result <- db[[i]] %>% filter(input == subquery) %>% arrange(desc(prob))
    
    if(dim(result)[1] >0){
      break
    } else if(i==2) {
      result <- db[[1]] %>% filter(input == "") %>% arrange(desc(prob))
      result <- result[sample(nrow(result), 1, replace=TRUE, prob = result$prob),]
    }
  }
  result
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
