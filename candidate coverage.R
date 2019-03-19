coverage_data <- read.csv('candidate_coverage.csv', stringsAsFactors = FALSE, row.names = NULL)

library(quanteda)
library(readr)
library(stringr)
library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining library(tidyr)
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(yarrr)  #Pirate plot
library(igraph) #ngram network diagrams
library(ggraph)
library(tidyverse)
library(topicmodels)

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")
storybench.colors <- c("azure4", "chocolate2", "cornflowerblue", "firebrick3")

theme_plots <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none",
                         family = "Helvetica")

undesirable_words <- c("harris", "harris’s", "harris's", "booker", "booker’s", "booker's", "warren",
                       "warren’s", "warren's", "sanders", "sanders’", "sanders’s", "gillibrand",
                       "gillibrand’s", "gillibrand's", "warren's", "klobuchar", "klobuchar’s",
                       "klobuchar's", "des", "moines", "president", "campaign", "san", "mccain",
                       "kamala", "bernie", "kirsten", "elizabeth", "amy", "cory", "democratic",
                       "senator", "democrats", "candidates", "political", "presidential", "voters",
                       "sen", "rep", "beto", "o’rourke", "o'rourke", "o'rourke's", "o’rourke’s",
                       "rourke", "rourke's", "rourke’s") 

tidy_coverage <- coverage_data %>%
  unnest_tokens(word, text) %>% #Break the text into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Short words
  anti_join(stop_words) #Data provided by the tidytext package

#MOST USED WORDS
harris_words <- tidy_coverage %>%
  filter(candidate == "Harris") %>%
  count(word, sort = TRUE) 

harris_words %>% print(n = 50)

#UNIQUE WORDS
tf_idf_words <- tidy_coverage %>% 
  count(word, candidate, sort = TRUE) %>%
  bind_tf_idf(word, candidate, n) %>%
  arrange(desc(tf_idf)) 

top_tf_idf_words <- tf_idf_words %>% 
  group_by(candidate) %>%
  top_n(10) %>%
  ungroup()

ggplot(top_tf_idf_words, aes(x = reorder(word, n), y = n, fill = candidate)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~candidate, scales = "free") +
  xlab(NULL) + ylab(NULL) +
  coord_flip() + 
  ggtitle("Unique Words By Candidate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text=element_text(family = "Helvetica"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

harris_idf_words <- tf_idf_words %>% 
  group_by(candidate) %>%
  filter(candidate == "Harris") %>%
  top_n(10) %>%
  ungroup()

sanders_idf_words <- tf_idf_words %>% 
  group_by(candidate) %>%
  filter(candidate == "Sanders") %>%
  top_n(10) %>%
  ungroup()

warren_idf_words <- tf_idf_words %>% 
  group_by(candidate) %>%
  filter(candidate == "Warren") %>%
  top_n(10) %>%
  ungroup()

klobuchar_idf_words <- tf_idf_words %>% 
  group_by(candidate) %>%
  filter(candidate == "Klobuchar") %>%
  top_n(10) %>%
  ungroup()

gillibrand_idf_words <- tf_idf_words %>% 
  group_by(candidate) %>%
  filter(candidate == "Gillibrand") %>%
  top_n(10) %>%
  ungroup()

booker_idf_words <- tf_idf_words %>% 
  group_by(candidate) %>%
  filter(candidate == "Booker") %>%
  top_n(10) %>%
  ungroup()

orourke_idf_words <- tf_idf_words %>% 
  group_by(candidate) %>%
  filter(candidate == "O'Rourke") %>%
  top_n(10) %>%
  ungroup()
  
ggplot(orourke_idf_words, aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE, fill = storybench.colors[3]) +
  xlab(NULL) + ylab(NULL) +
  coord_flip() + 
  ggtitle("Beto O'Rourke") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text=element_text(family = "Helvetica"),
        plot.title = element_text(hjust = 0.5, face = "bold"))  


#SENTIMENT ANALYSIS
tidy_coverage %>%
  filter(candidate == "Gillibrand") %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative)

coverage_bing <- tidy_coverage %>%
  inner_join(get_sentiments("bing"))

coverage_sentiment_candidates <- coverage_bing %>%
  count(sentiment, candidate) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)

coverage_sentiment_candidates %>%
  ggplot(aes(reorder(candidate, polarity), polarity, fill = ifelse(polarity >= 0,my_colors[5],my_colors[4]))) +
  geom_col(show.legend = FALSE) +
  xlab(NULL) + ylab("Net sentiment of words used") +
  coord_flip() +
  ggtitle("Media Sentiment by Candidate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text=element_text(family = "Helvetica"), 
        plot.title = element_text(hjust = 0.5, face = "bold"))

coverage_sentiment_candidates %>%
  ggplot(aes(reorder(candidate, percent_positive), percent_positive)) +
  geom_col(show.legend = FALSE, fill = storybench.colors[3]) +
  xlab(NULL) + ylab("Percentage of positive words used") +
  coord_flip() +
  ggtitle("Media Sentiment by Candidate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text=element_text(family = "Helvetica"), 
        plot.title = element_text(hjust = 0.5, face = "bold"))

#LABMT SENTIMENT
labmt_sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")

labMT <- labmt_sentiments %>%
  select(word, happs)

all_labmt_sentiment <- tidy_coverage %>%  
  inner_join(labMT, by = "word") %>%
  group_by(candidate) %>% 
  summarize(sentiment = mean(happs)) %>%
  arrange(desc(sentiment)) %>%
  mutate("score" = sentiment-5.372) 

all_labmt_sentiment %>%
  ggplot(aes(reorder(candidate, score), score)) +
  geom_col(show.legend = FALSE, fill = "lightgreen") +
  xlab(NULL) + ylab("Mean sentiment of words used") +
  coord_flip() +
  ggtitle("Media Sentiment by Candidate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text=element_text(family = "Helvetica"), 
        plot.title = element_text(hjust = 0.5, face = "bold"))

#TOPIC MODELING
coverage_dtm <- coverage_data %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Short words
  anti_join(stop_words) %>% #Data provided by the tidytext package
  count(candidate, word) %>%
  cast_dtm(candidate, word, n)

coverage_lda <- LDA(coverage_dtm, k = 10, control = list(seed = 1234))

coverage_topics <- tidy(coverage_lda, matrix = "beta")

coverage_top_terms <- coverage_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

coverage_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
