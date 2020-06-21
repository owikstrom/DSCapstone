library(quanteda);library(dplyr)
library(data.table);library(tidyr);library(tidyverse)

loadDB <- function(){
        sources <- c("twitter","news","blogs")  
        db <- list()
  
        for(n in 2:4){
                ###TODO : change names of list entries
                db <- append(db, list(readRDS(paste(n,"grams.rds", sep=""))))
        } 
        db
}

ngramjoin <- function (){
        sources <- c("twitter","news","blogs")

        for(n in 2:3){
                fullngrams <- data.table(feature=c("TEMP"), frequency=0)
                
                for(fileid in 1:3){
                        for(part in 1:3){
                                ngrams <- readRDS(paste(sources[fileid],n,part,'gramsfixed.rds',sep = ""))%>% 
                                        select(feature, frequency)
                                
                                fullngrams <- full_join(fullngrams, ngrams, by=c("feature" = "feature")) %>%
                                         transmute(feature, frequency.x = replace_na(frequency.x, 0),
                                                frequency.y = replace_na(frequency.y, 0),
                                                frequency = frequency.x+frequency.y) %>%
                                         select(feature, frequency)
                        }
                }
                
                fullngrams <- fullngrams[-1,] %>%
                        separate(feature, c(paste(rep('w', n),1:n, sep="")),sep="_", remove=F) %>%
                        unite(input,2:n, sep="_", remove=TRUE)%>%
                        rename(prediction =paste('w',n, sep = ""))%>%
                        mutate(prob = frequency/sum(frequency))
                setkey(fullngrams, feature)
                saveRDS(fullngrams, paste(n,"grams.rds", sep=""))
        }

}

# Process n-gram dfms to data.tables
ngram2dt <- function (){
        sources <- c("twitter","news","blogs")
        
        for(fileid in 2:3){
                for(part in 1:3){
                        
                        first = TRUE
                        for(n in 4:2){
                                if (first){
                                        # N-1 gram
                                        ngrams <- readRDS(paste(sources[fileid],n,1,'grams.rds',sep = ""))
                                        ngrams <- data.table(feature = featnames(ngrams), frequency = featfreq(ngrams), row.names = NULL)
                                        first=FALSE
                                }else{
                                        ngrams <- nlessgrams
                                }
        
                                nlessgrams <- readRDS(paste(sources[fileid],n-1,1,'grams.rds',sep = ""))
                                nlessgrams <- data.table(feature=featnames(nlessgrams), frequency=featfreq(nlessgrams), row.names = NULL)
        
                                ngrams <- ngrams %>%
                                        separate(feature, c(paste(rep('w', n),1:n, sep="")),sep="_", remove=F) %>%
                                        unite(nless, 3:(n+1),sep="_", remove=FALSE) %>% 
                                        left_join(nlessgrams, by=c("nless" = "feature"), suffix=c("", ".nless"))%>%
                                        select(-nless)%>%
                                        unite(input,2:(n), sep="_", remove=TRUE)%>%
                                        rename(prediction =paste('w',n, sep = ""))%>%
                                        filter(frequency !=frequency.nless & frequency!=1)
        
                                saveRDS(ngrams, paste(sources[fileid],n,part,'gramsfixed.rds',sep = ""))
                        }
                        for(n in 1){
                                #####include processing of unigrams
                        }
                }        
        }
        rm(nlessgrams, ngrams, part, first, n, fileid)
        gc()
}

# Loop for generating all n-grams in pieces.
genngrams <- function(){
        sources <- c("twitter","news","blogs")        
        for(fileid in 1:3){
                my_corpus <- readLines(files[10+fileid,1], encoding='UTF-8', skipNul=TRUE)
                len = length(my_corpus)
                my_corpus <- corpus(my_corpus, docnames = paste(sources[fileid],1:len, sep = ""), docvars = data.frame(src = sources[fileid]))
                
                for(n in 1:4){
                        start=1   
                        for(part in 1:3){
                                finish=round(len*part/3)
                                dfmShard <- batchngramdfm(my_corpus[start:finish], group=sources[fileid], batches = 25, ntype = n)
                                saveRDS(dfmShard, paste(sources[fileid],n,part,'grams.rds',sep = ""))
                                start = finish+1
                        }
                }
        }
        rm(fileid, len, my_corpus, start, finish, dfmShard,n,part)
}
