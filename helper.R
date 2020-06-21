batchedDFM <- function(corpus, batches=5){ 
        
        segment <- length(corpus)/batches

        start <- 1
        files <- paste(rep('temp',batches), 1:batches, rep('.rds',batches), sep="")

        #Batch
        for (i in 1:batches){
                finish=round(segment*i)
                dfm_segment <- dfm(corpus[start:finish], 
                                   tolower = TRUE, 
                                   remove_punct =TRUE, 
                                   remove_symbols=TRUE, 
                                   remove_url=TRUE,
                                   remove_numbers=TRUE, 
                                   include_docvars=FALSE)
                saveRDS(dfm_segment,files[i] )
                start =finish+1
        }

        # Knit 
        my_dfm <- readRDS(files[1])
        
        for (i in 2:batches){
                my_dfm <- rbind(my_dfm,readRDS(files[i]))
        }
        
        file.remove(files)
        return(my_dfm) 
        

}


batchTokens <- function(corpus, batches=20, prefix = 'b'){ 
        my_clock <- Sys.time()
        tot_clock <-Sys.time()
        
        segment <- length(corpus)/batches
        
        start <- 1
        files <- paste(rep(prefix, batches), rep('temp',batches), 1:batches, rep('.rds',batches), sep="")
        
        #Batch
        for (i in 1:batches){
                finish=round(segment*i)
                ngram_segment <- corpus[start:finish] %>% 
                        char_tolower(keep_acronyms = TRUE) %>% 
                        tokens(remove_punct =TRUE, remove_symbols=TRUE, remove_url=TRUE) %>% 
                        tokens_ngrams(n=1:3)
                saveRDS(ngram_segment,files[i] )
                start =finish+1
                print(paste('Save: ', i, ' ', Sys.time()-my_clock))
                my_clock <- Sys.time()
        }
        
        # Knit 
        my_ngrams <- readRDS(files[1])
        
        for (i in 2:batches){
                my_ngrams <-my_ngrams+readRDS(files[i])
                print(paste('Knit: ', i, ' ', Sys.time()-my_clock))
                my_clock <- Sys.time()
                
        }
        
        print(Sys.time()-tot_clock)
        # file.remove(files)
        return(my_ngrams) 
        
        
}

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
                               remove_url=TRUE,
                               split_hyphens = TRUE) %>% 
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