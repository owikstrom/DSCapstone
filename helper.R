batchngramdfm <- function(corpus, batches=100, group = 'text', ntype=2){ 
 
        my_clock <- Sys.time()
        tot_clock <-Sys.time()
        
        segment <- length(corpus)/batches
        start <- 1
        
        #Batch
        for (i in 1:batches){
                finish=round(segment*i)
                
                ngram_segment <- corpus[start:finish] %>% 
                        tokens(remove_punct =TRUE, 
                               remove_symbols=TRUE, 
                               remove_url=TRUE) %>% 
                        tokens_ngrams(n=ntype) %>% 
                        dfm(groups = rep(group,finish-start+1))
                
                start =finish+1

                if(i==1){
                        ngramdfm <- ngram_segment
                }else{
                        ngramdfm <- rbind(ngramdfm, ngram_segment)
                }
                
                print(paste('Batch: ', i, ' ', Sys.time()-my_clock))
                my_clock <- Sys.time()
                               
        
                if (i %% 10 == 0) ngramdfm <- dfm_compress(ngramdfm, margin = "documents")        
        }
        ngramdfm <- dfm_compress(ngramdfm, margin = "documents")
        Sys.time()-tot_clock
        rm(i,j, my_clock, batches)
        return(ngramdfm) 
}