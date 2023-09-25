# TAWR Ch. 7
#
# remove all the objects from memory
rm(list = ls())

setwd("/Users/theju/Desktop/R")

# 7.2 Start Up Code
# Beginning of Startup code
text_v           <- scan("text/melville.txt", what = "character", sep = "\n")
start_v          <- which(text_v == "CHAPTER 1. Loomings.")
end_v            <- which(text_v == "orphan.")
novel_lines_v    <- text_v[start_v:end_v]
chap_positions_v <- grep("^CHAPTER \\d", novel_lines_v)
last_position_v  <- length(novel_lines_v)
chap_positions_v <- c(chap_positions_v, last_position_v)

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

# 7.3 Mean Word Frequency
# use the raw frequency counts to calculate the
#  chapter by chapter mean word frequencies.
# To do so we need the total 
#  number of word tokens in each chapter
#  which we compute by summing the raw frequency
#  counts in each chapter. Then we calculate the
#  number of unique word types in each chapter

# begin by recalling the chapter count
length(chapter_raw_l) # should be 135

# How about the chatper titles? Check the first six
head(names(chapter_raw_l))

# because we have the chapter titles as names, we
#  can use them along with the bracket subset operator $
# check the class type
class(chapter_raw_l$`CHAPTER 1. Loomings.`) # notice the tool tip

# numeric subsetting accomplishes the same task
class(chapter_raw_l[[1]])

# word counts are available from the table
head(chapter_raw_l$`CHAPTER 1. Loomings.`)
head(chapter_raw_l[[1]])

# we can also see the structure of the object
str(chapter_raw_l$`CHAPTER 1. Loomings.`)

# Given that structure, we can
#  determine the chapter by chapter mean
#  word frequencies
sum(chapter_raw_l$`CHAPTER 1. Loomings.`) #  numerator
length(chapter_raw_l$`CHAPTER 1. Loomings.`)#denominator

summ <- sum(chapter_raw_l$`CHAPTER 1. Loomings.`)
leng <- length(chapter_raw_l$`CHAPTER 1. Loomings.`)
summ / leng

# a simpler technique is to use the mean function
mean(chapter_raw_l$`CHAPTER 1. Loomings.`)

# 7.4 Extracting Word Usage Means
# use lapply (list apply) to get the means for 
#  all the chapters
lapply(chapter_raw_l, mean)

# to make the output manageable, put it in a
#  matrix object
mean_word_use_m <- do.call(rbind,
                           lapply(chapter_raw_l, mean)
                           )
# check the class
class(mean_word_use_m)

# check the dimensions
dim(mean_word_use_m)

# the mean-word-use object's rownames
#  were populated automatically
rownames(mean_word_use_m)

# use the plot function to visualize the
#  mean word usage
plot(mean_word_use_m, type = 'h', 
     xlab = "Chapter", ylab = "Mean Word Use")
# the higher the bar, the greater the frequency
#  of individual words (word types)
# lower frequency counts indicate more
#  variety in word use

# to put the chapters on an equal footing
#  equalize them by scaling. The overall mean
#  is subtracted from each chapter mean
plot(scale(mean_word_use_m), type = 'h', 
     xlab = "Chapter", ylab = "Mean Word Use")

# 7.5 Ranking the Values
# if we dilate on "CHAPTER 32. Cetology."
#  we can compare its standing with the other
#  chapters

# using the order function we get a vector of
#  positions
an_order <- order(mean_word_use_m, decreasing = TRUE)
head(an_order)
head(mean_word_use_m[an_order])

# 7.6 Calculating the TTR Inside lapply
#     TTR (Type-Token Ratio) measure of lexical variety
#     How many times do tokens appear in a
#     text when compared to the token count
# 
# From above, calculate the mean for chapter one
#
summ <- sum(chapter_raw_l$`CHAPTER 1. Loomings.`)
leng <- length(chapter_raw_l$`CHAPTER 1. Loomings.`)
summ / leng

# the TTR is the inverse of the mean. Multiplying
#  by 100 produces a percentage
leng / summ * 100 # s/b 38.05704

# to compute one TTR for each chapter use lapply
#  and an inline function
ttr_l <- lapply(chapter_raw_l,
                function(x){length(x) / sum(x) * 100}
                )

# convert the list to a matrix using rbind and do call
ttr_m <- do.call(rbind, ttr_l)

# order and inspect the results
decr_order <- order(ttr_m, decreasing = TRUE)
head(decr_order)
head(ttr_m[decr_order,]) # the ordering is on the rows

# visualize using plot
plot(ttr_m, type = 'h', xlab = "Chapter", ylab = "TTR")

# look at it using descreasing order
plot(ttr_m[decr_order,], type = 'h', xlab = "", ylab = "TTR")

# 7.7 A Further Use of Correlation
#  Chapter length determines rate of word recycling
#  Longer chapter have more words and greater variety
#

# 7.8 Practice
#
# 1. To test the assertion that document length
#    is a strong determiner in the rate at which
#    words get recycled, measure the correlation
#    strength between TTR and chapter length. For
#    this you need two vectors of data. You already
#    have the TTR values in the ttr_m matrix.
#    Convert that data to a vector using as.vector
#    You now need another vector of chapter lengths.
#    For this you can use lapply with the sum
#    function instead of using mean. Once you 
#    have the two vectors, run a correlation analysis
#    similar to the correlation you did previously
#    with occurrences of whale and ahab. Write up
#    your code and analysis.
# 
ttr_v <- as.vector(ttr_m)
head(ttr_v)
length(ttr_v)

ch_len <- do.call(rbind, lapply(chapter_raw_l, sum))

head(ch_len)
ch_len_v <- as.vector(ch_len)
head(ch_len_v)
class(ch_len_v)

ttr_ch_len_m <- as.matrix(cbind(ttr_v, ch_len_v))
colnames(ttr_ch_len_m) <- c("TTR", "ChLen")
head(ttr_ch_len_m)

class(ttr_ch_len_m["TTR"])
class(ttr_ch_len_m["ChLen"])

cor(ttr_ch_len_m)

# 2. Run a similar correlation test using the values
#    in the mean_word_use_m instead of the TTR
#    values. Write the code and your interpretation
#    of the results
head(mean_word_use_m)
mean_word_use_v <- as.vector(mean_word_use_m)
head(mean_word_use_v)


mean_ch_len_m <- as.matrix(cbind(mean_word_use_v, ch_len_v))
colnames(mean_ch_len_m) <- c("mean", "ChLen")
head(mean_ch_len_m)
cor(mean_ch_len_m)

# 3. Use randomization to test the likelihood
#    that the correlation coefficient
#    observed with the TTR values could have
#    been the result of chance. 
#    Explain the outcome of the test.

# from the Chapter 6 R code
# use a loop
mycors_v <- NULL
for(i in 1:10000){
  mycors_v <- c(mycors_v, 
                cor(sample(ttr_v), ch_len_v))
}
min(mycors_v)
max(mycors_v)
range(mycors_v)
mean(mycors_v)
sd(mycors_v)

# 4. Explain the difference between the result
#    that you found in practice questions 1 and 2
#    (TTR-ChLen vs. mean-ChLen)




