predictWord <- function(phrase, ngramdb){

library(tidyverse)
         
        
        
for (i in 5:1) {

snippet <- phrase %>% word(-4:1,-1)
        
        
resultn <- ngramdb[[i]][snippet]

if(len(result >0)) break
        
}              
    
# Maybe recalculate for unobserved n-grams
# sample(resultn$type, 1, F, resultsn$prob)       
        
        
} 