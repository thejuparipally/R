# Ch. 15
# Clustering
#
# 15.2 Corpus Ingestion
#
rm(list = ls())


#setwd("C:/Users/tom.tiahrt/OneDrive - The University of South Dakota/Documents/Documents/784/TAWR/")
setwd("C:/Users/tom.tiahrt/Documents/TAWR/")


# record the input directory to make 
#  parameterization easier
#
input_dir <- "data/XMLAuthorCorpus"

# get the file list from the current directory
(files_v <- dir(path = input_dir, pattern = ".*xml"))
length(files_v)

# load the package that handles the XML files
#
library(xml2)

# put the path together
head(file.path(input_dir, files_v))

# test out the loop's contents before running the loop
i <- 1
(xml_doc <- read_xml(file.path(input_dir, files_v[i])))

i <- 43
(xml_doc <- read_xml(file.path(input_dir, files_v[i])))

# 15.3 Custom Functions
#  place the custom functions in corpus_functions_2.R
source("code/corpus_functions_2.R")

# set up a list to be populated in the loop
book_freqs_l <- list()

for(i in seq_along(files_v)){
  xml_doc <- read_xml(file.path(input_dir, files_v[i]))
  para_text <- get_node_text(xml_doc,
                             xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                             ns = c(tei = "http://www.tei-c.org/ns/1.0")
  )
  word_v <- tokenize(para_text)
  freq_table <- table(word_v) / length(word_v)
  # a table is not as flexible as a data frame
  #  it is clear why the suffix is "l" 
  #  instead of "df"? 
  #
  # Note the extra set of square brackets
  #  change the way the data frame gets 
  #  populated. The "[]" places the data frame
  #
  # stringsAsFactors must be false because
  #  the strings need to be character data
  #  instead of factors (categories)
  #
  book_freqs_l[[files_v[i]]] <- as.data.frame(
    freq_table, stringsAsFactors = FALSE
  )
}

class(book_freqs_l)
names(book_freqs_l)
str(book_freqs_l)

# 15.4 Unsupervised Clusterting and
#  the Euclidian Metric
#
a <- c(10, 5)
b <- c(11, 6)
c <- c(4, 13)
my_m <- rbind(a, b, c)
colnames(my_m) <- c("f1", "f2")
my_m

(dm <- dist(my_m))
my_m[1,]
plot(my_m, type = 'n' )
text(my_m[1,1], my_m[1,2], "a", cex = 1.5)
text(my_m[2,1], my_m[2,2], "b", cex = 1.5)
text(my_m[3,1], my_m[3,2], "c", cex = 1.5)

a <- c(10, 5, 3, 5)
b <- c(11, 6, 5, 7)
c <- c(4, 13, 2, 6)
my_m <- rbind(a, b, c)
colnames(my_m) <- c("f1", "f2", "f3", "f4")
my_m
(dm <- dist(my_m))

# 15.5 Converting an R List
#  into a Data Matrix
#
freqs_df <- do.call(rbind, book_freqs_l)
head(freqs_df)

# strip the numbers
text_names_v <- gsub(
  pattern = "\\.\\d+",
  replacement = "",
  x = rownames(freqs_df)
)
head(text_names_v)

long_df <- data.frame(text_names_v, freqs_df)
head(long_df)

colnames(long_df) <- c("file", "token", "freq")
head(long_df)

# 15.6 Reshaping from Long to Wide Format
#
# Use cross tabulation to convert the 
#  format from long to wide. Want the
#  frequencies in the cells and the cross
#  tabulation to be the intersection of
#  the files and the tokens
wide_t <- xtabs(formula = freq ~ file + token,
                data = long_df)
class(wide_t)

# the "xtabs" "table" will not work as input to
#  clustering. Instead, we need a data frame.
#
wide_df <- as.data.frame.matrix(wide_t)
head(colnames(wide_df))
tail(colnames(wide_df))
dim(wide_df)

# 15.7 Preparing Data for Clustering
#  
# Need to 'winnow' the data to avoid capturing
#  themes or content. To get to the style use
#  a frequency threshold. Begin with 0.01
#
# Calculate the column means across 
#  the data frame
#
token_means <- colMeans(wide_df)
head(token_means)
tail(token_means)

threshold <- 0.01
keepers_v <- names(token_means[which(token_means >= threshold)])
head(keepers_v,12)

threshold <- 0.02
keepers_v <- names(token_means[which(token_means >= threshold)])
head(keepers_v,12)

threshold <- 0.025
keepers_v <- names(token_means[which(token_means >= threshold)])
head(keepers_v)

threshold <- 0.008
keepers_v <- names(token_means[which(token_means >= threshold)])
head(keepers_v,12)

wide_df[1:2, keepers_v] # note column access via name

dist_m <- dist(wide_df[, keepers_v])
head(dist_m, 12)

simple_dist_m <- as.matrix(dist_m)
dim(simple_dist_m)
colnames(simple_dist_m) # column names are
rownames(simple_dist_m) # the same as the row names

simple_dist_m[1:3, 1:3]

# to discover which files are the most
#  stylistically similar to Carleton1.xml
#  select the Carleton1.xml column and sort
#  the rows from smallest to largest
#
sort(simple_dist_m[, "Carleton1.xml"])

# 15.8 Clustering the data
#
cluster <- hclust(dist_m)
cluster$labels <- rownames(wide_df)
plot(cluster)

# 15.9 Practice
#
# 2. As a final experiment, write some code to see
#    what happens if you select a random collection
#    of features. In other words, instead of selecting
#    from among the most high-frequency features,
#    write code that uses the sample() function to grab
#    a random sample of 50 or 100 word features and 
#    then see if you still get accurate author clustering.
#
token_sample <- sample(token_means, size = 50)
head(token_sample)
sample_keep_v <- names(token_sample)
head(sample_keep_v)
dist_sample_m <- dist(wide_df[, sample_keep_v])
head(dist_sample_m)

windows()
cluster_sample <- hclust(dist_sample_m)
cluster_sample$labels <- rownames(wide_df)
plot(cluster_sample)


