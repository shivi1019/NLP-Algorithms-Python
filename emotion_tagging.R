#### Loading library ####

require(tidyverse)
require(tidytext)
require(RColorBrewer)
require(gplots)
theme_set(theme_bw(12))
col_name
library(textdata)

# Reading dataset 
setwd("~/Project1-WFM/Project5 - Topic Modelling/Emotion Tagging/")

data <- read.csv("emotion_data.csv", stringsAsFactors = F) 




covid_terms <- c("serve","shortly","inform","assist","unable","assured","avoid","shopping","love")

# email 1 
  emotion_words_count <- data %>% 
      unnest_tokens( word,email_new_1_POS_filt, drop = FALSE, collapse = T) %>%                           
      anti_join(stop_words, by = "word") %>%                  
      filter(!grepl('[0-9]', word))  %>%
      left_join(get_sentiments("nrc"), by = "word") %>%
      filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
               !(word %in% colors()) & !(word %in% covid_terms )) %>%
      group_by(incident_id,email_1,word,sentiment) %>%
      summarize(total= n()) %>%
      ungroup()

  
  
  e1 <- emotion_words_count %>% select(-total) %>% 
    rename(word1 = word,sentiment1 = sentiment) %>% 
    group_by(incident_id, email_1) %>% summarize( word1 = paste(sort(unique(word1)),collapse=", "), 
                                                  sentiment1 = paste(sort(unique(sentiment1)),collapse=", "))
  
  
  
# email 2 
  emotion_words_count2 <- data %>% 
    unnest_tokens( word,email_new_2_POS_filt, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    left_join(get_sentiments("nrc"), by = "word") %>%
    filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
             !(word %in% colors()) & !(word %in% covid_terms )) %>%
    group_by(incident_id,email_2,word,sentiment) %>%
    summarize(total= n()) %>%
    ungroup()
  
  
  e2 <- emotion_words_count2 %>% select(-total) %>% 
    rename(word2 = word,sentiment2 = sentiment) %>% 
    group_by(incident_id, email_2)  %>% summarize( word2 = paste(sort(unique(word2)),collapse=", "), 
                                                   sentiment2 = paste(sort(unique(sentiment2)),collapse=", "))
  
  
  
# email 3
  emotion_words_count3 <- data %>% 
    unnest_tokens( word,email_new_3_POS_filt, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    left_join(get_sentiments("nrc"), by = "word") %>%
    filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
             !(word %in% colors()) & !(word %in% covid_terms )) %>%
    group_by(incident_id,email_3,word,sentiment) %>%
    summarize(total= n()) %>%
    ungroup()
  
  e3 <- emotion_words_count3 %>% select(-total) %>% 
    rename(word3 = word,sentiment3 = sentiment) %>% 
    group_by(incident_id, email_3)  %>% summarize( word3 = paste(sort(unique(word3)),collapse=", "), 
                                                   sentiment3 = paste(sort(unique(sentiment3)),collapse=", "))
  
  
  
 # email 4 
  emotion_words_count4 <- data %>% 
    unnest_tokens( word,email_new_4_POS_filt, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    left_join(get_sentiments("nrc"), by = "word") %>%
    filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
             !(word %in% colors()) & !(word %in% covid_terms )) %>%
    group_by(incident_id,email_4,word,sentiment) %>%
    summarize(total= n()) %>%
    ungroup()
  
  e4 <- emotion_words_count4 %>% select(-total) %>% 
    rename(word4 = word,sentiment4 = sentiment) %>% 
    group_by(incident_id, email_4)  %>% summarize( word4 = paste(sort(unique(word4)),collapse=", "), 
                                                   sentiment4 = paste(sort(unique(sentiment4)),collapse=", "))
  
  
  
  # email 5
  emotion_words_count5 <- data %>% 
    unnest_tokens( word,email_new_5_POS_filt, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    left_join(get_sentiments("nrc"), by = "word") %>%
    filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
             !(word %in% colors()) & !(word %in% covid_terms )) %>%
    group_by(incident_id,email_5,word,sentiment) %>%
    summarize(total= n()) %>%
    ungroup()

  
  e5 <- emotion_words_count5 %>% select(-total) %>% 
    rename(word5 = word,sentiment5 = sentiment) %>% 
    group_by(incident_id, email_5)  %>% summarize( word5 = paste(sort(unique(word5)),collapse=", "), 
                                                   sentiment5 = paste(sort(unique(sentiment5)),collapse=", "))
  
  
  
  
  
  
  e12 <- merge(e1, e2, by = "incident_id", all.x = T)
  
  e123 <- merge(e12, e3, by = "incident_id", all.x = T)
  
  e1234 <- merge(e123, e4, by = "incident_id", all.x = T)
  
  e12345 <- merge(e1234, e5, by = "incident_id", all.x = T)
  
  
  rm(e12,e123,e1234,e1,e2,e3,e4,e5)
  
  rm(emotion_words_count, emotion_words_count2, emotion_words_count3, emotion_words_count4, emotion_words_count5)
  
  
  
#### Polarity of emails ####
  
  # email 1 
  email_1_sense <- data %>% 
    unnest_tokens( word,email_1, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(incident_id,email_1) %>% 
    summarise( affinity_e1 = sum(value))
  
  neg_word1 <- data %>% 
    unnest_tokens( word,email_1, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    select(incident_id,email_1,word, value) %>% 
    filter(value < 0) %>% 
    group_by(incident_id, email_1) %>% 
    summarise(neg_word_1 = paste(sort(unique(word)), collapse = ","))

  
  #email 2 
  email_2_sense <- data %>% 
    unnest_tokens( word,email_2, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(incident_id,email_2) %>% 
    summarise( affinity_e2 = sum(value))
  
  
  
  neg_word2 <- data %>% 
    unnest_tokens( word,email_2, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    select(incident_id,email_2,word, value) %>% 
    filter(value < 0) %>% 
    group_by(incident_id, email_2) %>% 
    summarise(neg_word_2 = paste(sort(unique(word)), collapse = ","))
  
  
  
  
  #email 3 
  email_3_sense <- data %>% 
    unnest_tokens( word,email_3, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(incident_id,email_3) %>% 
    summarise( affinity_e3 = sum(value))
  
  
  
  
  neg_word3 <- data %>% 
    unnest_tokens( word,email_3, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    select(incident_id,email_3,word, value) %>% 
    filter(value < 0) %>% 
    group_by(incident_id, email_3) %>% 
    summarise(neg_word_3 = paste(sort(unique(word)), collapse = ","))
  
  
  
  
  
  #email4
  email_4_sense <- data %>% 
    unnest_tokens( word,email_4, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(incident_id,email_4) %>% 
    summarise( affinity_e4 = sum(value))
  
  
  
  
  
  neg_word4 <- data %>% 
    unnest_tokens( word,email_4, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    select(incident_id,email_4,word, value) %>% 
    filter(value < 0) %>% 
    group_by(incident_id, email_4) %>% 
    summarise(neg_word_4 = paste(sort(unique(word)), collapse = ","))
  
  
  
  #email_5
  
  email_5_sense <- data %>% 
    unnest_tokens( word,email_5, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(incident_id,email_5) %>% 
    summarise( affinity_e5 = sum(value))
  
  
  neg_word5 <- data %>% 
    unnest_tokens( word,email_5, drop = FALSE, collapse = T) %>%                           
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word))  %>%
    filter(!(word %in% colors()) & !(word %in% covid_terms )) %>%
    inner_join(get_sentiments("afinn")) %>%
    select(incident_id,email_5,word, value) %>% 
    filter(value < 0) %>% 
    group_by(incident_id, email_5) %>% 
    summarise(neg_word_5 = paste(sort(unique(word)), collapse = ","))
  


# Merging with the main data set 
  
  e_all_1 <- merge(e12345, email_1_sense, by = c("incident_id","email_1"), all.x = T)
  e_all_12 <- merge(e_all_1, email_2_sense, by = c("incident_id","email_2"), all.x = T)
  e_all_123 <- merge(e_all_12, email_3_sense, by = c("incident_id","email_3"), all.x = T)
  e_all_1234 <- merge(e_all_123, email_4_sense, by = c("incident_id","email_4"), all.x = T)
  e_all_12345 <- merge(e_all_1234, email_5_sense, by = c("incident_id","email_5"), all.x = T)
  
  
  rm(email_1_sense,email_2_sense,email_3_sense,email_4_sense, email_5_sense)
  rm(e_all_1,e_all_12,e_all_123,e_all_1234)

# Merging negative words with the main data set 
  neg_1 <- merge(e_all_12345, neg_word1, by = c("incident_id","email_1"), all.x = T)
  neg_12 <- merge(neg_1, neg_word2, by = c("incident_id","email_2"), all.x = T)
  neg_123 <- merge(neg_12, neg_word3, by = c("incident_id","email_3"), all.x = T)
  neg_1234 <- merge(neg_123, neg_word4, by = c("incident_id","email_4"), all.x = T)
  neg_12345 <- merge(neg_1234, neg_word5, by = c("incident_id","email_5"), all.x = T)
  
  
  
# writing to csv 
write.csv(neg_12345, file = "email_sentiment_affinity_negative_words.csv", row.names = F)
