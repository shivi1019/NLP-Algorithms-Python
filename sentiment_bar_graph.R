#### Loading library ####

require(tidyverse)
require(tidytext)
require(RColorBrewer)
require(gplots)
theme_set(theme_bw(12))
library(textdata)
library(reshape2)

# Reading dataset 
setwd("~/Project1-WFM/Project5 - NLP/Emotion Tagging/")

data <- read.csv("emotion_data.csv", stringsAsFactors = F) 

#### Step 1 - emotion tagging #### 
#counting total number of words 
# total_words_count <- data %>%
#   unnest_tokens(word, email_new_1_POS_filt) %>%
#   anti_join(stop_words, by = "word") %>%
#   filter(!grepl('[0-9]', word)) %>%
#   left_join(get_sentiments("nrc"), by = "word") %>%
#   filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
#            !(word %in% colors()) & !(word == "shopping" )) %>% 
#   group_by() %>%
#   summarize(total= n()) %>%
#   ungroup()
# 
#  
# #counting total number of words 
# total_words_count2 <- data %>%
#   unnest_tokens(word, email_new_2_POS_filt) %>%
#   anti_join(stop_words, by = "word") %>%
#   filter(!grepl('[0-9]', word)) %>%
#   left_join(get_sentiments("nrc"), by = "word") %>%
#   filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
#            !(word %in% colors()) & !(word == "shopping" )) %>% 
#   group_by() %>%
#   summarize(total= n()) %>%
#   ungroup()
# 
# 
# #counting total number of words
# total_words_count3 <- data %>%
#   unnest_tokens(word, email_new_3_POS_filt) %>%
#   anti_join(stop_words, by = "word") %>%
#   filter(!grepl('[0-9]', word)) %>%
#   left_join(get_sentiments("nrc"), by = "word") %>%
#   filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
#            !(word %in% colors()) & !(word == "shopping" )) %>% 
#   group_by() %>%
#   summarize(total= n()) %>%
#   ungroup()
# 
# 
# #counting total number of words
# total_words_count4 <- data %>%
#   unnest_tokens(word, email_new_4_POS_filt) %>%
#   anti_join(stop_words, by = "word") %>%
#   filter(!grepl('[0-9]', word)) %>%
#   left_join(get_sentiments("nrc"), by = "word") %>%
#   filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
#            !(word %in% colors()) & !(word == "shopping" )) %>% 
#   group_by() %>%
#   summarize(total= n()) %>%
#   ungroup()
# 
# #counting total number of words
# total_words_count5 <- data %>%
#   unnest_tokens(word, email_new_5_POS_filt) %>%
#   anti_join(stop_words, by = "word") %>%
#   filter(!grepl('[0-9]', word)) %>%
#   left_join(get_sentiments("nrc"), by = "word") %>%
#   filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
#            !(word %in% colors()) & !(word == "shopping" )) %>% 
#   group_by() %>%
#   summarize(total= n()) %>%
#   ungroup()


################## PROMOTER ###################################

promoter <- data %>% filter(csat_flag == 0)

covid_terms <- c("serve","shortly","inform","assist","unable","assured","avoid","shopping","love")


# email 1 
emotion_words_count <- promoter %>% 
  unnest_tokens( word,email_new_1_POS_filt, drop = FALSE, collapse = T) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word))  %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
           !(word %in% colors()) & !(word %in% covid_terms )) %>%
  group_by(sentiment) %>%
  summarize(total= n()) %>%
  ungroup()



# email 2 
emotion_words_count2 <- promoter %>% 
  unnest_tokens( word,email_new_2_POS_filt, drop = FALSE, collapse = T) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word))  %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
           !(word %in% colors()) & !(word %in% covid_terms )) %>%
  group_by(sentiment) %>%
  summarize(total= n()) %>%
  ungroup()




# email 3
emotion_words_count3 <- promoter %>% 
  unnest_tokens( word,email_new_3_POS_filt, drop = FALSE, collapse = T) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word))  %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
           !(word %in% colors()) & !(word %in% covid_terms )) %>%
  group_by(sentiment) %>%
  summarize(total= n()) %>%
  ungroup()



# email 4 
emotion_words_count4 <- promoter %>% 
  unnest_tokens( word,email_new_4_POS_filt, drop = FALSE, collapse = T) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word))  %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
           !(word %in% colors()) & !(word %in% covid_terms )) %>%
  group_by(sentiment) %>%
  summarize(total= n()) %>%
  ungroup()



# email 5
emotion_words_count5 <- promoter %>% 
  unnest_tokens( word,email_new_5_POS_filt, drop = FALSE, collapse = T) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word))  %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA") &
           !(word %in% colors()) & !(word %in% covid_terms )) %>%
  group_by(sentiment) %>%
  summarize(total= n()) %>%
  ungroup()




emotion_words_count <- emotion_words_count %>% rename( email_1 = total)
emotion_words_count2 <- emotion_words_count2 %>% rename( email_2 = total)
emotion_words_count3 <- emotion_words_count3 %>% rename( email_3 = total)
emotion_words_count4 <- emotion_words_count4 %>% rename( email_4 = total)
emotion_words_count5 <- emotion_words_count5 %>% rename( email_5 = total)


e12 <- merge(emotion_words_count, emotion_words_count2, by = "sentiment")

e123 <- merge(e12, emotion_words_count3, by = "sentiment")

e1234 <- merge(e123, emotion_words_count4, by = "sentiment")

e12345 <- merge(e1234, emotion_words_count5, by = "sentiment")


rm(e12,e123,e1234)

rm(emotion_words_count, emotion_words_count2, emotion_words_count3, emotion_words_count4, emotion_words_count5)


graph_promoter <- e12345


mydf.molten <- melt(graph_promoter[,c("sentiment","email_4","email_5")], value.name="Count", variable.name="Variable", na.rm=TRUE)


ggplot(mydf.molten, aes(x=Variable,y = Count)) +geom_bar(stat = "identity") + facet_wrap( "sentiment" )




write.csv(mydf.molten, file = "after removing covid words/Demoter_distribution.csv", row.names = F)

