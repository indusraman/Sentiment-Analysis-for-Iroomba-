
reviews1<-read.csv("C:/Users/indupravs/Downloads/Reviewroomba.csv",header=TRUE)
# Verified_Purchase should be TRUE
#reviews1<-filter(reviews1, Verified_Purchase==TRUE)
data("stop_words")
reviews1.text <-reviews1 %>%
  select(text)
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(reviews1.text$text)
reviews1.text$text<-sapply(reviews1.text$text,function(row)iconv(row,"latin1","ASCII",sub=""))
# Lowercase all text
reviews1.text$text <- tolower(reviews1.text$text)
# make wasn't=was not, can't=can not, etc..
reviews1.text$text<- gsub("wasn[\u2019']t", "was not", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t", "will not", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t", "can not", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t", "did not", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t", "do not", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t", "I am", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t", " have", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t",  "", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t", " are", reviews1.text$text)
reviews1.text$text<- gsub("wasn[\u2019']t", " will", reviews1.text$text)
# Remove numbers in the text
reviews1.text$text <- removeNumbers(reviews1.text$text)
# Remove punctuations in the text
reviews1.text$text <- removePunctuation(reviews1.text$text)
# Create a list to identify the sentiment shifters in the text
negation.words <- c("not",
                    "no",
                    "without",
                    "never",
                    "bad",
                    "none",
                    "never",
                    "nobody",
                    "nowhere",
                    "neither",
                    "nothing"
)

# Run the following to view Shifted sentiments sorted by polarity point
shifted.words <- reviews1.text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(word1 %in% negation.words & !word2 %in% stop_words$word)%>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word"))%>%
  mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
  mutate(score = sentiment * n) %>%
  mutate(word2 = reorder(word2, score))

#shifted.words

# Pick the most effective sentiment shifters
negated.phrases <- c("not worth", 
                     "not noise",
                     "no issues",
                     "no complaints",
                     "not disappoint",
                     "not disappointed",
                     "not cheap",
                     "no regrets"
                     
)
# Find synonyms for the phrases above to replace
synonyms <- c("expensive",
              "functional",
              "cool",
              "satisfied",
              "satisfied",
              "satisfied",
              "expensive",
              "satisfied"
)
# Replace the negations with their synonyms.
 reviews1.text <- mgsub(reviews1.text$text, negated.phrases, synonyms) %>%
        dplyr::as_data_frame() %>%
       rename(text = value)

# if you want to ignore words that are frequent but doesn't help, add them to this list.
ignore.words <- data_frame(word = c("irobot","roomba","im","one","get","doesnt","even","now","two",
                                     "vacuum","just","back","'s","vaccum","let","said","vacuuming",
                                     "robot","got","left","make","etc","say","thought","ive","'S",
                                    "also","every"))
# create the words freq table
 word.freq.table<- reviews1.text %>% 
       unnest_tokens(word, text) %>%
       anti_join(stop_words) %>%
       anti_join(ignore.words) %>%
      count(word, sort = TRUE)
 
 
 # Plotting a Wordcloud
 word.freq.table %>% 
      filter(n>100) %>%
     with(wordcloud(word, n,scale = c(5,0.3),colors = brewer.pal(8, "Dark2")))
 
 # Most Common Bigrams
 reviews1.text %>%
   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
   count(bigram, sort = TRUE) %>%
   separate(bigram, c("word1", "word2"), sep = " ") %>%
   filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
   filter(n>7) %>%
   unite(word, word1:word2, sep = " ") %>%
   with(wordcloud(word, n,
                  scale = c(3,0.5),
                  colors = brewer.pal(8, "Dark2")))
 
 
 # Most common Positive and Negative words using Bing
 reviews1.text %>% 
   unnest_tokens(word, text) %>%
   anti_join(stop_words) %>%
   anti_join(ignore.words) %>%
   inner_join(get_sentiments("bing")) %>%
   count(word, sentiment, sort = TRUE) %>%
   filter(n > 20) %>%
   mutate(word = reorder(word, n)) %>%
   mutate(percent = round(n/sum(n), 3)) %>%
   ggplot(aes(x = word, y = percent, fill = sentiment, label = percent)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~sentiment, scales = "free_y") +
   geom_text(aes(y = 0.7*percent)) +
   labs(title = "Iroomba Word Polarity (bing)") +
   coord_flip() + 
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))
 
 # Bing
 bing.mean.score <- word.freq.table %>% 
   inner_join(get_sentiments("bing")) %>%
   mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
   summarise(mean = mean(sentiment))
 # Correlation Terms
 # The correlation of appearing together in a review
roomba.correlation.terms <- reviews1.text %>%
   mutate(review = row_number()) %>%
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words$word) %>%
   group_by(word) %>%
   filter(n() >= 5)%>%
   pairwise_cor(word, review, sort = TRUE)
roomba.correlation.terms
 
library(ggraph)
library(igraph)

roomba.correlation.terms %>%
  filter(correlation >= 0.50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "igraph", algorithm = "kk") +
  geom_edge_link(aes(alpha = correlation), 
                 show.legend = FALSE)+
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


bigrams.network.df<-reviews1.text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 5)

bigrams.network <- graph_from_data_frame(bigrams.network.df)

# now we find the centrality measures of the network
# degree:the number of its adjacent edges (measure of direct influence)
deg <- degree(bigrams.network, mode = "all")

#K-core decomposition allows us to identify the core and the periphery of the network. A k-core is a maximal subnet of a network such that all nodes have at least degree K.
core <- coreness(bigrams.network, mode = "all")

# betweenness measures brokerage or gatekeeping potential. It is (approximately) the number of shortest paths between nodes that pass through a particular node.
betw <- betweenness(bigrams.network)

#Eigenvector centrality is a measure of being well-connected connected to the well-connected. First eigenvector of the graph adjacency matrix. Only works with undirected networks.
eigen <- eigen_centrality(bigrams.network, directed = TRUE)
members <- cluster_walktrap(bigrams.network)

bigrams.network <- simplify(bigrams.network, 
                            remove.multiple = FALSE,
                            remove.loops = TRUE)
V(bigrams.network)$color <- members$membership+1

# Using "Coreness" as size
# Coreness -> mean (average distance to all the other nodes, diffusion of information)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.9,
     vertex.label.dist = 0,
     vertex.frame.color = 0,
     vertex.size = core*10, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "gray",
     main = "Bigram Communities (Iroomba)"
)
mtext("Coreness")

# Using "Degree" as size
# degree=mode (number of edges of the node, in-degree:prestige

plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.9,
     vertex.label.dist = 0,
     vertex.frame.color = 0,
     vertex.size = deg, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "gray",
     main = "Bigram Communities (Bose Headphones)"
)
mtext("Degree")

# Using "Eigenvector Centrality" as size
# centrality (the most connected words)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.label.dist = 0,
     vertex.size = eigen$vector*20, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "black",
     main = "Bigram Communities (Iroomba)"
)
mtext("Eigenvector Centrality")

# Using "Betweenness" as size
# Betweenness -> median (weighted # of paths going through the node)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.label.dist = 0,
     vertex.size = betw, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "lightgrey",
     main = "Bigram Communities (Iroomba)"
)
mtext("Betweenness")













