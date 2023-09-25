# TAWR Chapter 2
setwd("/Users/theju/Desktop/R")

# [ ] vector subscript
# [[ ]] list subscript - can only identify a single element
# $ Named component selection from a list

# TAWR uses gutentext_v first, but uses text_v thereafter
text_v <- scan("melville.txt",
                    what = "character",
                    sep = "\n")
typeof(text_v)
class(text_v)
text_v[1]
text_v[2]
text_v[3]

start_v <- which(text_v == "CHAPTER 1. Loomings.")
start_v


length(text_v)

metadata_v <- text_v[1:start_v - 1]
metadata_v

novel_lines_v <- text_v[start_v:length(text_v)]
length(novel_lines_v)

novel_v <- paste(novel_lines_v, collapse = " ")
novel_v[1]

novel_lower_v <- tolower(novel_v)
# never use "l" because it is too close to "1"; use "L" instead
moby_word_L <- strsplit(novel_lower_v, "\\W") 
length(moby_word_L)
?strsplit
moby_word_L

# A regular language is a language that can be expressed
#  with a regular expression or a deterministic or
#  non-deterministic finite automata or finite-state machine.

class(novel_lower_v)
class(moby_word_L)
str(moby_word_L)

moby_word_v <- unlist(moby_word_L)

str(moby_word_v)
class(moby_word_v)

moby_word_v[4]

not_blanks_v <- which(moby_word_v != "")
not_blanks_v

not_blanks_v[1:10]

moby_word_v <- moby_word_v[not_blanks_v]
moby_word_v[1:10]
length(moby_word_v)

moby_word_v[214869:214891]
moby_word_v[4:6]

# the function named c combines elements into a vector
mypositions_v <- c(4,5,6)
moby_word_v[mypositions_v]
moby_word_v[c(4,5,6)]

which(moby_word_v == "whale")
moby_word_v[which(moby_word_v == "whale")]

length(moby_word_v[which(moby_word_v == "whale")])
length(moby_word_v)

whale_hits_v <-length(moby_word_v[which(moby_word_v == "whale")])
whale_hits_v
total_words_v <- length(moby_word_v)
total_words_v

whale_hits_v / total_words_v

length(unique(moby_word_v))

moby_freq_t <- table(moby_word_v)
moby_freq_t[1:10]

sorted_moby_freqs_t <- sort(moby_freq_t, decreasing = TRUE)
head(sorted_moby_freqs_t)
head(sorted_moby_freqs_t, 10)
sorted_moby_freqs_t[100:122]
# 2.6 Practice
# 1
top_ten_t <- sorted_moby_freqs_t[1:10]
top_ten_t

# 2 
my_nums <- c(1:10)
plot(my_nums, top_ten_t)
plot(my_nums)
plot(top_ten_t)

