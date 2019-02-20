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

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

theme_plots <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none",
                         family = "Helvetica")

undesirable_words <- c("harris", "harris’s", "harris's", "booker", "booker’s", "booker's", "warren",
                       "warren’s", "warren's", "sanders", "sanders’", "sanders’s", "gillibrand",
                       "gillibrand’s", "gillibrand's", "warren's", "klobuchar", "klobuchar’s",
                       "klobuchar's", "des", "moines", "president", "campaign", "san", "mccain") 

tidy_coverage <- coverage_data %>%
  unnest_tokens(word, text) %>% #Break the text into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Short words
  anti_join(stop_words) #Data provided by the tidytext package

harris_words <- tidy_coverage %>%
  filter(candidate == "Harris") %>%
  count(word, sort = TRUE) 

harris_words %>% print(n = 40)

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
  coord_flip() + 
  ggtitle("Unique Words By Candidate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text=element_text(family = "Helvetica"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

tidy_coverage %>%
  filter(candidate == "Gillibrand") %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative)

coverage_bing <- tidy_coverage %>%
  inner_join(get_sentiments("bing"))

coverage_polarity_candidate <- coverage_bing %>%
  count(sentiment, candidate) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)

coverage_polarity_candidate %>%
  ggplot(aes(candidate, polarity, fill = ifelse(polarity >= 0,my_colors[5],my_colors[4]))) +
  geom_col(show.legend = FALSE) +
  xlab("Candidates") + ylab(NULL) +
  ggtitle("Sentiment by Candidate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text=element_text(family = "Helvetica"), 
        plot.title = element_text(hjust = 0.5, face = "bold"))

