# TAWR Chapter 3
setwd("/Users/theju/Desktop/R")

# Beginning of Startup code
text_v              <- scan("text/melville.txt", what = "character", sep = "\n")
start_v             <- which(text_v == "CHAPTER 1. Loomings.")
end_v               <- which(text_v == "orphan.")
start_metadata_v    <- text_v[1:start_v -1]
end_metadata_v      <- text_v[(end_v+1):length(text_v)]
metadata_v          <- c(start_metadata_v, end_metadata_v)
novel_lines_v       <- text_v[start_v:end_v]
novel_v             <- paste(novel_lines_v, collapse=" ")
novel_lower_v       <- tolower(novel_v)
moby_words_L        <- strsplit(novel_lower_v, "\\W")
moby_word_v         <- unlist(moby_words_L)
not_blanks_v        <- which(moby_word_v != "")
moby_word_v         <-  moby_word_v[not_blanks_v]
whale_hits_v        <- length(moby_word_v[which(moby_word_v == "whale")])
total_words_v       <- length(moby_word_v)
moby_freqs_t        <- table(moby_word_v)
sorted_moby_freqs_t <- sort(moby_freqs_t , decreasing = TRUE)
# End of Startup code

# 3.3 Accessing Word Data
#  more men than women in Moby Dick
sorted_moby_freqs_t["he"]  # access using words themselves
sorted_moby_freqs_t["she"] # instead of index values
sorted_moby_freqs_t["him"]
sorted_moby_freqs_t["her"]

# index values do retrieve specific locations
moby_word_v[4:6]

# sorted_moby_freqs_t - as a table, it
#  provides both numeric and named indexing
sorted_moby_freqs_t[1]
sorted_moby_freqs_t["the"]

# compute the relative frequencies of him and her
sorted_moby_freqs_t["him"]/sorted_moby_freqs_t["her"]
sorted_moby_freqs_t["he"]/sorted_moby_freqs_t["she"]

# total number of words in Moby Dick
length(moby_word_v)

# another way to get the same value
sum(sorted_moby_freqs_t)

# 3.4 Recycling
# Convert the raw counts into relative frequencies
#  by using division, then multiplication by 100
#  to produce percentages
moby_length_v <- sum(sorted_moby_freqs_t)
sorted_moby_rel_freqs_t <- 100 * (sorted_moby_freqs_t / moby_length_v)

# the moby_length_v value is recycled to apply to each 
#  frequency in the set

# an example using a simple vector
num_vector_v <- c(1, 2, 3, 4, 5)
num_vector_v * 10

# percentages show the number of occurrences per 100 words
sorted_moby_rel_freqs_t["the"]

# Next plot the top ten words by their frequencies
#  Note that the names() function can set or get the
#  object names. Here we need it to get the object names
plot(sorted_moby_rel_freqs_t[1:10], type = 'b',
     xlab = "Ten Most Frequent Words in Moby Dick",
     ylab = "Percentage of Full Text",
     xaxt = "n")
axis(1, 1:10, labels = names(sorted_moby_rel_freqs_t[1:10]))

# Because the RStudio window is pretty small
#  use the R window instead
windows()
plot(sorted_moby_rel_freqs_t[1:10], type = 'b',
     xlab = "Ten Most Frequent Words in Moby Dick",
     ylab = "Percentage of Full Text",
     xaxt = "n")
axis(1, 1:10, labels = names(sorted_moby_rel_freqs_t[1:10]))

# 3.5 Practice
# note: this should be placed in a function or functions
#  but we will follow the text here
text_v              <- scan("text/austen.txt", what = "character", sep = "\n")
start_v             <- which(text_v == "CHAPTER 1")
end_v               <- length(text_v)

# the metadata has already been stripped out, so that it is not
#  necessary to process it
novel_lines_v        <- text_v[start_v:end_v]
novel_v              <- paste(novel_lines_v, collapse=" ")
novel_lower_v        <- tolower(novel_v)
sense_words_L        <- strsplit(novel_lower_v, "\\W")
sense_word_v         <- unlist(sense_words_L)
not_blanks_v         <- which(sense_word_v != "")
sense_word_v         <- sense_word_v[not_blanks_v]
sense_freqs_t        <- table(sense_word_v)
sorted_sense_freqs_t <- sort(sense_freqs_t , decreasing = TRUE)
sense_length_v       <- sum(sorted_sense_freqs_t)
sorted_sense_rel_freqs_t <- 100 * 
                         (sorted_sense_freqs_t / sense_length_v)

windows()
plot(sorted_sense_rel_freqs_t[1:10], type = 'b',
     xlab = "Ten Most Frequent Words in Sense and Sensibility",
     ylab = "Percentage of Full Text",
     xaxt = "n")
axis(1, 1:10, labels = names(sorted_sense_rel_freqs_t[1:10]))

# 3.5.2 Find the unique and most frequently occurring shared
#  words between the two books
unique(c(names(sorted_moby_rel_freqs_t[1:10]),
         names(sorted_sense_rel_freqs_t[1:10])))

# 3.5.3 use the which() function in combination with 
#  the %in% operator to compute the shared
#  words in the two top-10 lists

# preliminary: find out how which() works
which(names(sorted_moby_rel_freqs_t[1:10]) == "the")
which(names(sorted_moby_rel_freqs_t[1:10]) != "")

which(names(sorted_moby_rel_freqs_t[1:10]) %in% c("the","of","him"))

which(names(sorted_moby_rel_freqs_t[1:10]) %in% c("the","of","his"))

names(sorted_moby_rel_freqs_t[
  which(names(sorted_moby_rel_freqs_t[1:10]) %in% c("the","of","his"))
])

names(sorted_moby_rel_freqs_t[
  which(names(sorted_sense_rel_freqs_t[1:10])
        %in% names(sorted_moby_rel_freqs_t[1:10]))])

# 3.5.4 use the which() function in combination with 
#  the %in% operator to compute the top-10 words that are
#  _not_ shared in the two top-10 lists

# begin with the intersection
which(names(sorted_sense_rel_freqs_t[1:10])
      %in% names(sorted_moby_rel_freqs_t[1:10]))
seq(1:10) # to test the numbers excluded use one through ten

# check the exclusion against the sequence
seq(1:10)[-which(names(sorted_sense_rel_freqs_t[1:10])
            %in% names(sorted_moby_rel_freqs_t[1:10]))]

# given that the sequence works, record the
#  intersection index values
sense_in_moby <- which(names(sorted_sense_rel_freqs_t[1:10])
        %in% names(sorted_moby_rel_freqs_t[1:10]))
sense_in_moby

# exclude the intersection
names(sorted_sense_rel_freqs_t[1:10])[-sense_in_moby]

# repeat the process above, but by reversing the positions
#  of moby dick and sense and sensibility
moby_in_sense <- which(names(sorted_moby_rel_freqs_t[1:10])
        %in% names(sorted_sense_rel_freqs_t[1:10]))
moby_in_sense

names(sorted_moby_rel_freqs_t[1:10])[-moby_in_sense]


