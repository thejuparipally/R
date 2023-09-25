# Ch. 16 - Classification
#  
# 16.2 A Small Authorship Experiment
#
rm(list = ls())


setwd("/Users/theju/Desktop/R")
#setwd("/Users/theju/Desktop/R")
#setwd("/Users/theju/Desktop/R")

source("code/corpus_functions_2.R")
input_dir <- "data/XMLAuthorCorpus"
(files_v <- dir(path = input_dir, pattern = ".*xml"))

# 16.3 Text Segmentation
# Built in sets
LETTERS # character
letters

# suppose we want to split the LETTERS into
#  13 equally sized chunks
x <- LETTERS
(groups_v <- cut(1:length(x), breaks = 13, labels = FALSE))

# chunk the groups into a list object
(chunks_l <- split(x, groups_v))

# use that idea to divide the xml data - note
#  that the textbook's listing has the same code
#  above re-listed below this
library(xml2)
for(i in seq_along(files_v)){
  xml_doc <- read_xml(file.path(input_dir, files_v[i]))
  para_text <- get_node_text(xml_doc,
    xpath = "/tei:TEI/tei:text/tei:body//tei:p",
    ns = c(tei = "http://www.tei-c.org/ns/1.0"))
  word_v <- tokenize(para_text)
  # chunking code
  groups <- cut(1:length(word_v), breaks = 10, labels = FALSE)
  chunks_l <- split(word_v, groups)
}
length(groups)
length(chunks_l)
length(chunks_l[[1]])
length(chunks_l[[2]])
length(chunks_l[[10]])
length(chunks_l[[11]]) # Error in chunks_l[[11]] : subscript out of bounds

length(chunks_l[[1]][1])
chunks_l[[1]][1]
chunks_l[[1]][1:10]

# testing is easier without the entire range of loop iterations
i <- 1
xml_doc <- read_xml(file.path(input_dir, files_v[i]))
para_text <- get_node_text(xml_doc,
                           xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                           ns = c(tei = "http://www.tei-c.org/ns/1.0"))
word_v <- tokenize(para_text)
head(word_v)
length(word_v)

groups <- cut(1:length(word_v), breaks = 10, labels = FALSE)
chunks_l <- split(word_v, groups)
str(chunks_l)

chunk_table_l <- lapply(chunks_l, table)
str(chunk_table_l) # see the raw counts of each token in the segment
head(chunk_table_l[[1]]/sum(chunk_table_l[[1]]), 12)

# proportions
# Express Table Entries as Fraction of Marginal Table
#  Returns conditional proportions given margins, 
#  i.e. entries of x, divided by the appropriate marginal sums.
#
# Usage
#  proportions(x, margin = NULL)
#  prop.table(x, margin = NULL)
# Arguments
# x       table
# margin  a vector giving the margins to split by.
#         E.g., for a matrix 1 indicates rows, 
#         2 indicates columns, 
#         c(1, 2) indicates rows and columns.
#         When x has named dimnames, it can be a character
#         vector selecting dimension names.
#
# Value - Table like x expressed relative to margin
#
# Note - prop.table is an earlier name, retained for back-compatibility.
#
chunk_frequencies_t_l <- lapply(chunk_table_l, prop.table)
length(chunk_frequencies_t_l)
head(chunk_frequencies_t_l[[1]], 12)

for(i in seq_along(files_v)){
  xml_doc <- read_xml(file.path(input_dir, files_v[i]))
  para_text <- get_node_text(xml_doc,
                             xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                             ns = c(tei = "http://www.tei-c.org/ns/1.0"))
  word_v <- tokenize(para_text)
  # chunking code
  groups <- cut(1:length(word_v), breaks = 10, labels = FALSE)
  chunks_l <- split(word_v, groups)
  chunk_table_l <- lapply(chunks_l, table)
  chunk_frequencies_t_l <- lapply(chunk_table_l, prop.table)
}

chunk_frequencies_df_l <- lapply(chunk_frequencies_t_l, data.frame)
head(chunk_frequencies_df_l[[1]], 8)

segment_df <- do.call(rbind, chunk_frequencies_df_l)
dim(segment_df)
head(segment_df)

# add segments to the loop
for(i in seq_along(files_v)){
  xml_doc <- read_xml(file.path(input_dir, files_v[i]))
  para_text <- get_node_text(xml_doc,
                             xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                             ns = c(tei = "http://www.tei-c.org/ns/1.0"))
  word_v <- tokenize(para_text)
  # chunking code
  groups <- cut(1:length(word_v), breaks = 10, labels = FALSE)
  chunks_l <- split(word_v, groups)
  chunk_table_l <- lapply(chunks_l, table)
  chunk_frequencies_t_l <- lapply(chunk_table_l, prop.table)
  chunk_frequencies_df_l <- lapply(chunk_frequencies_t_l, data.frame)
  segment_df <- do.call(rbind, chunk_frequencies_df_l)
}

# do not need to retain the row numbers, just the 
#  segment identifiers and the name of the origination file
# find the period (.) character followed by any number
#  of other characters and replace them with the empty
#  string
segment_ids_v <- gsub("\\..*", "", rownames(segment_df))
head(segment_ids_v)
tail(segment_ids_v)

# put the details together into a data frame
i <- 1
book_df <- data.frame(
  ID = paste(
    files_v[i],
    segment_ids_v,
    sep = "_"
  ), segment_df
)
head(book_df)
tail(book_df)

i <- 43
book_df <- data.frame(
  ID = paste(
    files_v[i],
    segment_ids_v,
    sep = "_"
  ), segment_df
)
head(book_df)
tail(book_df)

# create an empty container above the start of
#  the for loop so that cumulative binding
#  can take place
long_df <- NULL

# to monitor the progress through the loop
#  use the cat() function to provide a
#  progress update
for(i in seq_along(files_v)){
  xml_doc <- read_xml(file.path(input_dir, files_v[i]))
  para_text <- get_node_text(xml_doc,
                             xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                             ns = c(tei = "http://www.tei-c.org/ns/1.0"))
  word_v <- tokenize(para_text)
  # chunking code
  groups <- cut(1:length(word_v), breaks = 10, labels = FALSE)
  chunks_l <- split(word_v, groups)
  chunk_table_l <- lapply(chunks_l, table)
  chunk_frequencies_t_l <- lapply(chunk_table_l, prop.table)
  chunk_frequencies_df_l <- lapply(chunk_frequencies_t_l, data.frame)
  segment_df <- do.call(rbind, chunk_frequencies_df_l)
  segment_ids_v <- gsub("\\..*", "", rownames(segment_df))
  book_df <- data.frame(
    ID = paste(
      files_v[i],
      segment_ids_v,
      sep = "_"
    ), segment_df
  )
  long_df <- rbind(long_df, book_df)
  cat("Done processing", files_v[i],
      "which is file", i, "of", length(files_v), "\r")
}

# 16.4 Reshaping from Long to Wide Format
#
# use the same method used in the previous chapter
# to cross tabulate long_df from a long form object
# to a wide form object. First rename the columns
# then use xtabs to do the reshaping. After that
# convert the results to a data frame object.
# This will produce a new data frame with
# 430 rows, 10 for each of the 43 novels in the
# corpus. May take 30 seconds to run
colnames(long_df) <- c("file", "token", "freq")
wide_t <- xtabs(formula = freq ~ file + token,
                data = long_df)
wide_df <- as.data.frame.matrix(wide_t)
dim(wide_df)

wide_df[1:4, 1:4]
wide_df[1:10, c("of", "the")]

# 16.5 Mapping the Data to the Metadata
#
# need to map the word frequency data contained
#  in the rows to the specified authors, and
#  not specific text samples (i.e., not just
#  the specific samples)
# derive a new matrix object (metacols_m) by
#  splitting the row names using the underscore
#  character that was inserted during the
#  paste() command above.
metacols_m <- do.call(rbind, strsplit(rownames(wide_df), "_"))
head(metacols_m)
tail(metacols_m)

colnames(metacols_m) <- c("sampletext", "samplechunk")
head(metacols_m)

# to see the unique values in the sampletext column
unique(metacols_m[, "sampletext"])

# need to be able to identify works by the same
#  author.
# First, remove the ".xml" suffix
#
meta_names_v <- gsub("\\.xml$", "", metacols_m[, "sampletext"])
head(meta_names_v)
tail(meta_names_v)

# Find the digits followed by the end of string
# and remove them
author_v <- gsub("\\d+$", "", meta_names_v)
head(author_v)
tail(author_v)
unique(author_v)

# using the vector of author names
authorship_df <- data.frame(
  author_v, metacols_m, wide_df, stringsAsFactors = FALSE
)
dim(authorship_df)
authorship_df[1,1]
authorship_df[,1]

# 16.6 Reducing the Feature Set
#  The new data frame contains too many features
#  for an authorship attribution text.
#  Calculate the feature means for each token
#  and then set a retension threshold. For
#  this example use 0.005 for the minimum threshold
#
#  Do not want to alter the metadata, just the token
#  frequency data
token_means <- colMeans(authorship_df[, 4:ncol(authorship_df)])
(keepers_v <- names(token_means[which(token_means >= 0.005)]))

# identify the subset of columns in the
#  authorship_df that we need for analysis
smaller_df <- authorship_df[, names(keepers_v)]
smaller_df # no columns?

# one way to retain the metadata is through column binding
#
smaller_df <- cbind(author_v, metacols_m, smaller_df)
head(smaller_df)
tail(smaller_df)

# not quite what we need
# so put all the parts together at once
smaller_df <- authorship_df[, c(names(authorship_df)[1:3], keepers_v)]
head(smaller_df)
tail(smaller_df)

# 16.7 Performing the Classification with SVM

# Identify the rows in the new data frame that
#  belong to the anonymous author
anon_v <- which(smaller_df$author_v == "anonymous")
head(anon_v)
tail(anon_v)

# partition the data into a training set but
#  need to exclude the anon_v vector by using
#  the negation operator. Also, do not need
#  the metadata
ncol(smaller_df)
train <- smaller_df[-anon_v, 4:ncol(smaller_df)]
head(train, 1)

# Use a factor so that levels can be used
class_f <- as.factor(smaller_df[-anon_v, "author_v"])
head(class_f)

# Use support vector machines for the classification task
#
if(!require(e1071)) install.packages("e1071")
library(e1071)

model_svm <- svm(train, class_f)
model_svm
summary(model_svm)

pred_svm <- predict(model_svm, train)
as.data.frame(pred_svm)

table(pred_svm, class_f)

# try the test data
testdata <- smaller_df[anon_v, 4:ncol(smaller_df)]
final_result <- predict(model_svm, testdata)
as.data.frame(final_result)



