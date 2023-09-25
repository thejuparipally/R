# Ch. 17 Topic Modeling
#
# 17.3 Text Segmentation and Preparation
#
rm(list = ls())

setwd("/Users/theju/Desktop/R")
#setwd("/Users/theju/Desktop/R")
#setwd("/Users/theju/Desktop/R")

source("code/corpus_functions_2.R")
input_dir <- "data/XMLAuthorCorpus"
(files_v <- dir(path = input_dir, pattern = ".*xml"))

library(xml2)

i <- 1
xml_doc <- read_xml(file.path(input_dir, files_v[i]))
para_text <- get_node_text(xml_doc,
                           xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                           ns = c(tei = "http://www.tei-c.org/ns/1.0"))
word_v <- tokenize(para_text)

chunk_size <- 1000 # number of words per chunk
(num_breaks <- length(word_v) / chunk_size) # not a whole number

# The values in x above are a sequence of word v indices. 
#  using it with ceiling (rounding up to the next greater integer)
#  corrects the mismatch caused by the decimal
# As an example, use a short sequence of 100 values
some_vector <- 1:100
x <- seq_along(some_vector)
some_chunk_size <- 9
x / some_chunk_size
ceiling(x / some_chunk_size)
(chunks_l <- split(some_vector, ceiling(x / some_chunk_size)))

# The last chunk will not be the same size as
#  the other chunks.
chunk_size <- 1000 # recall: the number of words per chunk
x <- seq_along(word_v)
chunks_l <- split(word_v, ceiling(x / chunk_size))
head(ceiling(x / chunk_size))
tail(ceiling(x / chunk_size))
chunks_l[[1]][1:8]
length(chunks_l[[1]])
head(lapply(chunks_l, length), 4)
tail(lapply(chunks_l, length), 4)

# to ensure that the last chunk is at least
#  half the size of the others
if(length(chunks_l[[length(chunks_l)]]) <= chunk_size / 2){
  chunks_l[[length(chunks_l) - 1]] <- c(
    chunks_l[[length(chunks_l) - 1]],
    chunks_l[[length(chunks_l)]]
  )
  chunks_l[[length(chunks_l)]] <- NULL
}

# put all the code together
i <- 1
xml_doc <- read_xml(file.path(input_dir, files_v[i]))
para_text <- get_node_text(xml_doc,
                           xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                           ns = c(tei = "http://www.tei-c.org/ns/1.0"))
word_v <- tokenize(para_text)

chunk_size <- 1000 # number of words per chunk
x <- seq_along(word_v)
chunks_l <- split(word_v, ceiling(x / chunk_size))
if(length(chunks_l[[length(chunks_l)]]) <= chunk_size / 2){
  chunks_l[[length(chunks_l) - 1]] <- c(
    chunks_l[[length(chunks_l) - 1]],
    chunks_l[[length(chunks_l)]]
  )
  chunks_l[[length(chunks_l)]] <- NULL
}
chunk_strings_l <- lapply(chunks_l, paste, collapse = " ")
chunks_df <- do.call(rbind, chunk_strings_l)

# need the metadata, so capture them from the files v
#  Strip the suffix using a regular expression
textname_v <- gsub("\\..*", "", files_v[i])
chunk_ids_v <- 1:nrow(chunks_df)
chunk_names_v <- paste(textname_v, chunk_ids_v, sep = "_")
file_df <- data.frame(
  id = chunk_names_v,
  text = chunks_df,
  stringsAsFactors = FALSE # want strings not factors
)
head(file_df, 1)
tail(file_df, 1)


# Construct the final form of the text-reading code
library(xml2)
input_dir <- "data/XMLAuthorCorpus"
files_v <- dir(path = input_dir, pattern = ".*xml")

documents_df <- NULL
chunk_size <- 1000

for(i in seq_along(files_v)){
  xml_doc <- read_xml(file.path(input_dir, files_v[i]))
  para_text <- get_node_text(xml_doc,
                             xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                             ns = c(tei = "http://www.tei-c.org/ns/1.0"))
  word_v <- tokenize(para_text)
  
  chunk_size <- 1000 # number of words per chunk
  x <- seq_along(word_v)
  chunks_l <- split(word_v, ceiling(x / chunk_size))
  if(length(chunks_l[[length(chunks_l)]]) <= chunk_size / 2){
    chunks_l[[length(chunks_l) - 1]] <- c(
      chunks_l[[length(chunks_l) - 1]],
      chunks_l[[length(chunks_l)]]
    )
    chunks_l[[length(chunks_l)]] <- NULL
  }
  chunk_strings_l <- lapply(chunks_l, paste, collapse = " ")
  chunks_df <- do.call(rbind, chunk_strings_l)
  
  # need the metadata, so capture them from the files v
  #  Strip the suffix using a regular expression
  textname_v <- gsub("\\..*", "", files_v[i])
  chunk_ids_v <- 1:nrow(chunks_df)
  chunk_names_v <- paste(textname_v, chunk_ids_v, sep = "_")
  file_df <- data.frame(
    id = chunk_names_v,
    text = chunks_df,
    stringsAsFactors = FALSE # want strings not factors
  )
  documents_df <- rbind(documents_df, file_df)
}
names(documents_df)
dim(documents_df)

# 17.4 The R Mallet Package
if(!require(mallet)) install.packages("mallet")
library(mallet)

# 17.5 Simple Topic Modeling with a Standard Stop List
#  In the stylistic analysis of prior chapters, high
#  frequency words were retained and used as markers of
#  authorial style. But in topic modeling the functional
#  words such as "the", "of", "and", "a", "an", as
#  well as others. Topic modeling removes those words
#  using a stop list (Jockers calls them "stop-out" words)
# If those words are not removed, the weighted word
#  distribution will be dominated by syntatical conventions
#  instead of topics or themes.
#
# mallet has an import function with five arguments (parameters)
#  1. id.array      - vector of document IDs
#  2. text.array    - vector of text strings, 1st col of documents_df
#  3. stoplist.file - stoplist.csv
#  4. preserve.case - perform case folding?
#  5. token.regexp  - regular expression used for tokenization
#                   -  [\\p{L}']+
#
# the mallet instances object is a Java object called a
#  mallet instances list. It is not an R object so it
#  must be accessed using Java methods.
mallet_instances <- mallet.import(
  documents_df$id,
  documents_df$text,
  "data/stoplist.csv",
  FALSE,
  token.regexp = "[\\p{L}']+"
)

# The next step is to create a "topic model trainer object"
#  to hold the data for the next steps. This is where we
#  choose the topic count. Here the author chooses
#  the number of topics equal to the number of novels
topic_model <- MalletLDA(num.topics = 43)

class(topic_model)
# [1] "jobjRef"    - pointer to a Java object
# attr(,"package")
# [1] "rJava"

# load the documents
topic_model$loadDocuments(mallet_instances)

# the corpus vocabulary is available as a character vector
vocabulary <- topic_model$getVocabulary()
class(vocabulary)
length(vocabulary)
head(vocabulary)
vocabulary[1:50]

# The word frequencies are also available
#  as a data frame with a row for each unique
#  word type in the corpus
word_freqs <- mallet.word.freqs(topic_model)
names(word_freqs) # "word" "word.freq" "doc.freq"
head(word_freqs)

# We can 'tweak' the topic model's optimization
#  hyperparameters. 
# defaults are 200 burn-in iterations and
#  50 iterations in each optimization interval 
#
topic_model$setAlphaOptimization(40, 80)

# The number of sampling iterations must be set 
#  Increasing the number of iterations improves
#  the results up to a plateau point, after which
#  improvement is minimal or non-existent
topic_model$train(400)

topic_words_m <- mallet.topic.words(
  topic_model,
  smoothed = TRUE,
  normalized = TRUE
)

# When normalization is set to TRUE, the values in
#  each topic (row) are converted to proportions
#  that sum to one
rowSums(topic_words_m)

# if normalization is FALSE, the contents are
#  counts of occurrences (integers) of that word type
topic_words_m[1:3, 1:3]

# use the vocabulary to name the columns
vocabulary <- topic_model$getVocabulary()
colnames(topic_words_m) <- vocabulary
topic_words_m[1:3, 1:3]

# investigate key words
keywords <- c("california", "ireland")
topic_words_m[, keywords]

# We can calculate which word has the greatest
#  concentration of key terms using rowSums and max
#  
imp_row <- which(
  rowSums(topic_words_m[, keywords]) == 
    max(rowSums(topic_words_m[, keywords]))
)
imp_row

# For a ranked sorting of topic words use
#  mallet.top.words
#   1. topic_model   - model itself
#   2. word.weights  - row from matrix of word weights
#   3. num.top.words - how many words to display
#
mallet.top.words(topic_model,
                 topic_words_m[imp_row, ], 10)

# word clouds may be more revealing than just the top ten
if(!require(wordcloud)) install.packages("wordcloud")
library(wordcloud)

topic_top_words <- mallet.top.words(topic_model,
                                    topic_words_m[imp_row, ], 100)
wordcloud(topic_top_words$term,
          topic_top_words$weight,
          c(2, 0.4), rot.per = 0, random.order = FALSE)

#
doc_topics_m <- mallet.doc.topics(  topic_model,
                                    smoothed = TRUE,
                                    normalized = TRUE
)

file_ids_v <- documents_df[, 1]
head(file_ids_v)

# convert the columns to a two-column matrix
file_id_l <- strsplit(file_ids_v, "_")
file_chunk_id_l <- lapply(file_id_l, rbind)
file_chunk_id_m <- do.call(rbind, file_chunk_id_l)
head(file_chunk_id_m)

doc_topics_df <- as.data.frame(doc_topics_m)

# need a data frame with details
doc_topics_df <- data.frame(
  file = file_chunk_id_m[, 1],
  doc_topics_df,
  stringsAsFactors = FALSE
)

library(dplyr)
doc_topics_means_df <- group_by(doc_topics_df, file) %>%
  summarize_all(mean)

head(doc_topics_means_df)
barplot(doc_topics_means_df$V18,
        names.arg = c(1:43),
        main = paste("Topic", keywords),
        xlab = "Books",
        ylab = "Topic Mean")
doc_topics_means_df[38, "file"] # may vary due to randomization
doc_topics_means_df[37:40, "file"] # may vary due to randomization

top_words <- mallet.top.words(
  topic_model, topic_words_m[37, ], 20
)
top_words
