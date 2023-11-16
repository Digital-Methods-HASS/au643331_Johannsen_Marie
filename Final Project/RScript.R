library(tidytext)
library(tidyverse)
library(here)
library(pdftools)
library(textdata)
library(ggwordcloud)

SD_path <- here("Data/Social-Demokraten_(1907-1959).pdf")
SD_text <- pdf_text(SD_path)

NT_path <- here("Data/Nationaltidende_(1876-1931).pdf")
NT_text <- pdf_text(NT_path)

JP_path <- here("Data/Jyllandsposten_(1871-1937).pdf")
JP_text <- pdf_text(JP_path)

SD_df <- data.frame(SD_text) %>% 
  mutate(text_full = str_split(SD_text, pattern = '\n')) %>% 
  unnest(text_full) %>% 
  mutate(text_full = str_trim(text_full)) 

NT_df <- data.frame(NT_text) %>% 
  mutate(text_full = str_split(NT_text, pattern = '\n')) %>% 
  unnest(text_full) %>% 
  mutate(text_full = str_trim(text_full)) 

JP_df <- data.frame(JP_text) %>% 
  mutate(text_full = str_split(JP_text, pattern = '\n')) %>% 
  unnest(text_full) %>% 
  mutate(text_full = str_trim(text_full)) 

#Removing the frontpage

SD_df <- SD_df[-c(1:22), ]
NT_df <- NT_df[-c(1:22), ]
JP_df <- JP_df[-c(1:22), ]

#Dividing into words

SD_words <- SD_df %>% 
  unnest_tokens(word, text_full)

NT_words <- NT_df %>% 
  unnest_tokens(word, text_full)

JP_words <- JP_df %>% 
  unnest_tokens(word, text_full)

#Removing stopwords

StopW <- read_csv("Data/Stopwordlist_danish.txt")
colnames(StopW)[1] <- "word" 

SD_no_stop <- SD_words %>% 
  anti_join(StopW) %>% 
  select(-SD_text)

NT_no_stop <- NT_words %>% 
  anti_join(StopW) %>% 
  select(-NT_text)

JP_no_stop <- JP_words %>% 
  anti_join(StopW) %>% 
  select(-JP_text)

#Counting to find the most frequent words

SD_wc <- SD_no_stop %>% 
  count(word) %>% 
  arrange(-n)

NT_wc <- NT_no_stop %>% 
  count(word) %>% 
  arrange(-n)

JP_wc <- JP_no_stop %>% 
  count(word) %>% 
  arrange(-n)

#Filtering away the numbers

SD_no_num <- SD_wc %>% 
  filter(is.na(as.numeric(word)))

NT_no_num <- NT_wc %>% 
  filter(is.na(as.numeric(word)))

JP_no_num <- JP_wc %>% 
  filter(is.na(as.numeric(word)))

#Finding the top 100 words

SD_100 <- SD_no_num %>% 
  head(100)

NT_100 <- NT_no_num %>% 
  head(100)

JP_100 <- JP_no_num %>% 
  head(100)

#Creating wordclouds

SD_cloud <- ggplot(data = SD_100, aes(label = word, size = n)) +
  geom_text_wordcloud_area(aes(color = n), shape = "cloud") +
  scale_size_area(max_size = 10) +
  scale_color_gradientn(colors = c("darkgreen","blue", "red")) +
  theme_classic()

NT_cloud <- ggplot(data = NT_100, aes(label = word, size = n)) +
  geom_text_wordcloud_area(aes(color = n), shape = "cloud") +
  scale_size_area(max_size = 10) +
  scale_color_gradientn(colors = c("darkgreen","blue","red")) +
  theme_classic()

JP_cloud <- ggplot(data = JP_100, aes(label = word, size = n)) +
  geom_text_wordcloud_area(aes(color = n), shape = "cloud") +
  scale_size_area(max_size = 10) +
  scale_color_gradientn(colors = c("darkgreen","blue","red")) +
  theme_classic()

#Searching for specific words

SD_war <- SD_100 %>% 
  filter(word == "krig" | word == "tyskland" | word == "ostrig" | word == "frankrig" | word == "fred" | word == "hær" | word == "neutralitet" | word == "angreb" | word == "krigen" | word == "mobilisering")

NT_war <- NT_100 %>% 
  filter(word == "krig" | word == "tyskland" | word == "ostrig" | word == "frankrig" | word == "fred" | word == "hær" | word == "neutralitet" | word == "angreb" | word == "krigen" | word == "mobilisering")

JP_war <- JP_100 %>% 
  filter(word == "krig" | word == "tyskland" | word == "ostrig" | word == "frankrig" | word == "fred" | word == "hær" | word == "neutralitet" | word == "angreb" | word == "krigen" | word == "mobilisering")


