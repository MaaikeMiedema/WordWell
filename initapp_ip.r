# Reads / prepares data and defines functions used in WordWell

#### PACKAGES ####

library(quanteda)
library(dplyr)
library(stringr)

#### DATA ####

# substitute for NA values when returned by prediction function 
naSub <- readRDS("data/na_sub.rds") # values
naSub <- data.frame(w6=naSub, prob=0) #assign probability 0 to NA replacement
naSub$w6 <- as.character(naSub$w6)

ngramProbs<- readRDS("data/ngram_probs.rds")

vocabulary <- unique(ngramProbs$w5)

# words/emoticons to be removed from n grams:
rmWords <- readRDS("data/badwords.rds")

#### FUNCTIONS ####

cleanTokens_Pred<-function (data, words)
{
      # input: data: a character object, words: vector with words to be removed
      # output: tokens object of data (per sentence), cleaned and words removed
      #
      tmp <- tokens(data, what="sentence")
      tmp <- as.character(tmp) # vector with sentences
      tmp <- tmp<-str_to_lower(tmp) %>%
            str_replace_all("\\s*\\S*www\\S+\\s*", " WWW ") %>%
            str_replace_all("\\s*\\S*http\\S+\\s*", " WWW ") %>%
            str_replace_all("[_#@:~]", " ")  
      tmp <-tokens(tmp, what="word",
                   remove_numbers = TRUE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_hyphens = FALSE, remove_twitter=FALSE
                   )
      tmp <- tokens_remove(tmp,"([0-9]*[\\.,-][0-9]){1,}", valuetype = "regex", verbose=FALSE)
      tmp
}

predictIP <- function(inputWords, ngramProbs,rmWords, vocabulary, naSub,m){
      toks <- cleanTokens_Pred(inputWords, rmWords)
      nrSentences <- length(toks)
      wordsTemp <- unlist(toks[nrSentences])
      keep <- !(wordsTemp %in% rmWords)
      wordsTemp <- wordsTemp[keep]
      unknown <-!(wordsTemp %in% vocabulary) 
      wordsTemp[unknown] <- "UNKNOWN"
      nrWords <- length(wordsTemp)
      v <- rep(NA, 5)
      if (nrWords>=5) v[1:5] <- wordsTemp[(nrWords-4):nrWords] else
            v[5:(6-nrWords)] <- wordsTemp[nrWords:1]                              
      # handle UNKNOWN values
      if ((is.na(v[5]) | v[5]=="UNKNOWN") & is.na(v[4])) {  #dit is niet helemaal goed want dat betekent als je twee unknown waarden achter elkaar hebt aan eind dat voorgaande woorden niets meer doen
            prediction <- naSub[1:m,]
            prediction[,2] <- 0 
            } 
      else {
            prediction <- filter(ngramProbs, w6!="UNKNOWN", w5==v[5],            # unknown values should be removed beforehand
                                  w4==v[4] | is.na(w4),w3==v[3] | is.na(w3), 
                                  w2==v[2] | is.na(w2), w1==v[1] | is.na(w1) ) %>% 
                  transmute(w6, prob=pIP6) %>% 
                  arrange(desc(prob))
            notInPred <- !(naSub$w6 %in% prediction$w6) # to prevent double suggestions
            naSub <- naSub[notInPred,]
            prediction <- rbind(prediction[1:5,], naSub)      
            prediction[,2] <- prediction[,2]/prediction[1,2] # relative to first prediction probability, not in app yet
            notNA <- !is.na(prediction$w6)
            prediction <- unique(prediction[notNA,])[1:m,]
      
      }
     prediction
}
