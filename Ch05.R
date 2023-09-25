# Ch05.R

ls()
# remove all the objects from memory
rm(list = ls())
ls()

setwd("/Users/theju/Desktop/R")
getwd()

# Beginning of Startup code
text_v              <- scan("text/melville.txt", what = "character", sep = "\n")
start_v             <- which(text_v == "CHAPTER 1. Loomings.")
end_v               <- which(text_v == "orphan.")
start_metadata_v    <- text_v[1:start_v -1]
end_metadata_v      <- text_v[(end_v+1):length(text_v)]
metadata_v          <- c(start_metadata_v, end_metadata_v)
novel_lines_v       <- text_v[start_v:end_v]

novel_lines_v

?grep

chap_positions_v <- grep("^CHAPTER \\d", novel_lines_v)
novel_lines_v[chap_positions_v]
head(novel_lines_v[chap_positions_v])

tail(novel_lines_v)
chap_positions_v

last_position_v <- length(novel_lines_v)
chap_positions_v <- c(chap_positions_v, last_position_v)

# alternatively,
# chap_positions_v <- c(chap_positions_v, length(novel_lines_v))

tail(chap_positions_v)

chap_positions_v[1]
chap_positions_v[2]

# instead of individually entering the index values
#  use a for loop
for(i in 1:length(chap_positions_v)){
  print(paste("Chapter", i, "begins at position", chap_positions_v[i]))
}

chapter_raw_l <- list()
chapter_freqs_l <- list()
len_ch_pos_v <- length(chap_positions_v)
for(i in 1:len_ch_pos_v){
  if(i < len_ch_pos_v){ # NB less than vs. not equal to
    chapter_title <- novel_lines_v[chap_positions_v[i]]
    start <- chap_positions_v[i] + 1 # one past starting line (CHAPTER n)
    end   <- chap_positions_v[i + 1] - 1 # one before the next CHAPTER
    chapter_lines_v <- novel_lines_v[start:end] # words bounded by CHAPTER lines
    chapter_words_v <- tolower(paste(chapter_lines_v, collapse = " "))
    chapter_words_l <- strsplit(chapter_words_v, "\\W")
    chapter_words_v <- unlist(chapter_words_l)
    chapter_words_v <- chapter_words_v[which(chapter_words_v != "")]
    chapter_freqs_t <- table(chapter_words_v)
    
    # double brackets assigns a name or label to the list item
    #  each item is named by the chapter title. Without naming 
    #  the items will have an index number assigned to it
    chapter_raw_l[[chapter_title]] <- chapter_freqs_t
    
    # convert the raw counts to relative frequencies
    chapter_freqs_t_rel <- 100 * (chapter_freqs_t/sum(chapter_freqs_t))
    chapter_freqs_l[[chapter_title]] <- chapter_freqs_t_rel
  }
}

head(chapter_freqs_l)

# 5.6.1 rbind
# rbind - binding rows together. Requires the same
#  number of columns in each row
# e.g., 
x <- c(1,2,3,4,5)
y <- c(6,7,8,9,10)
rbind(x,y)

# what happens if the number of columns differs?
y <- c(6,7,8,9,10,11)
rbind(x,y) # recycles enough of x to cover the entries in y

# 5.6.2 More Recycling
# recycling a vector of 1
y <- 2
x * y # straightforward doubling of the elements in x

# less obvious example
y <- c(2,3)
x * y

# recycling
# [1]  2    6    6   12   10
#      ^    ^    ^    ^    ^
#      |    |    |    |    |
#     1x2  2x3  3x2  4x3  5x2  

# 5.6.3 apply
# lapply - list apply
#  similar to a for loop
#  requires a list and a function 
#  it applies the function to the list
x <- list(a = 1:4, b = 2:5, b = 3:7)
x
x$b[1] <- 13
x

x <- list(a = 1:3, b = 2:4, c = 4:6)
x
lapply(x, mean)

chapter_freqs_l[[1]]

chapter_freqs_l[[1]]["whale"]

# another form that finds all the
#  chapter instances of 'whale', 
#  but which is not intuitive, is
lapply(chapter_freqs_l, '[', 'whale')

chapter_freqs_l[[1]]["whale"]
chapter_freqs_l[[2]]["whale"]
# ...
chapter_freqs_l[[135]]["whale"]

whale_l <- lapply(chapter_freqs_l, '[', 'whale')

# too much trouble, but could be done
# rbind(whale_l[[1]], whale_l[[2]], ..., whale_l[[135]])

# 5.6.4 do.call (do dot call)
# create a list with three integer vectors
x <- list(1:3, 4:6, 7:9)
x

# convert the list to a matrix where each row is
#  one of the vectors and each column is one of
#  the three integers from each of the list items
do.call(rbind, x)

whales_m <- do.call(rbind, whale_l)
head(whales_m)

ahab_l <- lapply(chapter_freqs_l, '[', 'ahab')
ahabs_m <- do.call(rbind, ahab_l)

# 5.6.5 cbind
class(whales_m[,1])

x <- c(1,2,3,4,5,6)
y <- c(2,4,5,6,7,8)
z <- c(24,23,34,32,12,10)
test_m <- cbind(x, y, z)
test_m

# row 2, column 3
test_m[2, 3]

# show all values in the second row
test_m[2,]

test_m[,"y"]

# pull vectors from the matrix object
whales_v <- whales_m[,1]
ahabs_v <- ahabs_m[,1]

# use cbind to bind these new objects together
whales_ahabs_m <- cbind(whales_v, ahabs_v)
dim(whales_ahabs_m)

colnames(whales_ahabs_m) <- c("whale", "ahab")

barplot(whales_ahabs_m, beside = TRUE)

# 5.7 Practice
# 1. In section 5.6.3 we saw how we could use lapply
#   with the mean function to calculate the mean of
#   numerical vectors that were stored in a list, x.
#   Instead of using lapply, write a for loop that
#   achieves the same end. Then, instead of saving
#   items into a new object, just print the results
#   of each iteration to the console.
x <- list(a = 1:4, b = 2:5, b = 3:7)

x
for(i in 1:length(x)) {
  print(mean(x[[i]]))
}

# 2. a. Write code that will find the relative frequencies,
#       per chapter for another word (e.g., queequeg) from
#       the same chap_freq_l object. 
#    b. Once you have isolated all relative frequencies
#       for that word in a list, convert the list to a
#       matrix by using rbind.
#    c. Next like you converted whales_v from whales_m,
#       develop a single vector holding the chapter
#       relative frequencies for queequeg. 
#    d. After you have done this, bind whales_v, ahabs_v, 
#       and the new vector into a single matrix with
#       three columns.
#

queequeg_l <- lapply(chapter_freqs_l, '[', 'queequeg')
queequeg_m <- do.call(rbind, queequeg_l)
queequeg_v <- queequeg_m[,1]

whales_ahabs_queequeg_m <- cbind(whales_v, ahabs_v, queequeg_v)
barplot(whales_ahabs_queequeg_m, beside = T)

#
# 3. These bar plots were derived from the list of
#    relative frequency data. Write a script to plot
#    the raw occurrences of whale and ahab per 
#    chapter using chapter_raws_l you created.
whale_raw_l <- lapply(chapter_raw_l, '[', 'whale')
whale_raw_m <- do.call(rbind, whale_raw_l)
whale_raw_v <- whale_raw_m[,1]

ahab_raw_l  <- lapply(chapter_raw_l, '[', 'ahab' )
ahab_raw_m  <- do.call(rbind, ahab_raw_l)
ahab_raw_v  <- ahab_raw_m[,1]

whales_ahabs_raw_m <- cbind(whale_raw_v, ahab_raw_v)
barplot(whales_ahabs_raw_m, beside = T)

