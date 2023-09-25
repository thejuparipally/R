# TAWR Chapter 4
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
novel_v             <- paste(novel_lines_v, collapse=" ")
novel_lower_v       <- tolower(novel_v)
moby_words_L        <- strsplit(novel_lower_v, "\\W")
moby_word_v         <- unlist(moby_words_L)
not_blanks_v        <- which(moby_word_v != "")
moby_word_v         <-  moby_word_v[not_blanks_v]
# End of Startup code

# Sequence Generation
seq(from = 1, to = 10, by = 1)
seq(from = 1, to = 10, by = 2)
seq(from = 1, to = 10, by = 5)
seq(from = 1, to = 10, by = 1/2)

n_time_v <- seq(from = 1, to = length(moby_word_v))
whales_v <- which(moby_word_v == 'whale')
length(whales_v)

w_count_v <- rep(NA, times = length(n_time_v))

w_count_v[whales_v] <- 1
plot(w_count_v,
     main = "Dispersion Plot of 'whale' in Moby Dick",
     xlab = "Novel Time",
     ylab = "whale",
     type = 'h',
     ylim = c(0,1), yaxt = 'n')

ahabs_v <- which(moby_word_v == 'ahab')
a_count_v <- rep(NA, length(n_time_v))
a_count_v[ahabs_v] <- 1
plot(a_count_v,
     main = "Dispersion Plot of 'ahab' in Moby Dick",
     xlab = "Novel Time",
     ylab = "ahab",
     type = 'h',
     ylim = c(0,1), yaxt = 'n')

# Let us view the results stacked one on top of the other
par(mfrow = c(2,1))
plot(w_count_v,
     main = "Dispersion Plot of 'whale' in Moby Dick",
     xlab = "Novel Time",
     ylab = "whale",
     type = 'h',
     ylim = c(0,1), yaxt = 'n')
plot(a_count_v,
     main = "Dispersion Plot of 'ahab' in Moby Dick",
     xlab = "Novel Time",
     ylab = "ahab",
     type = 'h',
     ylim = c(0,1), yaxt = 'n')

whale_hits <- grep("whale|whales|whale's|monster|leviathan", moby_word_v)
head(whale_hits)
tail(whale_hits)
moby_word_v[head(whale_hits, 8)]

ahab_hits <- grep("ahab|ahabs|ahab's|captain", moby_word_v)
paste(length(whales_v), length(whale_hits))
paste(length(ahabs_v), length(ahab_hits))

eg_v <- "this is a _test_ to see if we can keep ahab's and contractions
 such as can't and ain't. it will also show us some other oddities."
eg_v
strsplit(eg_v, "\\W")

strsplit(eg_v, "[^A-Za-z0-9']") # use this below

length(moby_word_v)

# redo the startup code to produce
#  a different parse of Moby Dick that allows
#  apostrophes to be retained.
text_v           <- scan("text/melville.txt", what = "character", sep = "\n")
start_v          <- which(text_v == "CHAPTER 1. Loomings.")
end_v            <- which(text_v == "orphan.")
start_metadata_v <- text_v[1:start_v -1]
end_metadata_v   <- text_v[(end_v+1):length(text_v)]
metadata_v       <- c(start_metadata_v, end_metadata_v)
novel_lines_v    <- text_v[start_v:end_v]
novel_v          <- paste(novel_lines_v, collapse=" ")
novel_lower_v    <- tolower(novel_v)
moby_words_L     <- strsplit(novel_lower_v, "[^A-Za-z0-9']")
moby_word_v      <- unlist(moby_words_L)
not_blanks_v     <- which(moby_word_v != "")
moby_word_v      <-  moby_word_v[not_blanks_v]

length(moby_word_v)

whale_hits_new <- grep("whale|whales|whale's|monster|leviathan", moby_word_v)
ahab_hits_new <- grep("ahab|ahabs|ahab's|captain", moby_word_v)

# get the same count even though there are fewer tokens because
#  the \\W breaks ahab's into ahab and s (the same for whale's)
paste(length(whales_v), length(whale_hits), length(whale_hits_new))
paste(length(ahabs_v), length(ahab_hits), length(ahab_hits_new))

# check the results with new frequency tables
whale_variants_v <- moby_word_v[whale_hits_new]
ahab_variants_v  <- moby_word_v[ahab_hits_new]

sort(table(whale_variants_v), decreasing = TRUE)

sort(table(ahab_variants_v), decreasing = TRUE)

# which finds exact matches
length(which(whale_variants_v == "whale"))
# grep finds patterns
length(grep("whale", whale_variants_v))

length(grep("^whale$", whale_variants_v))

# 4.6 Practice
# 4.6.1. To find the whale variants occurrences, use the same
#   grep code as before, but replace
#   which(moby_word_v == "whale")
#   with grep() and the multiple whale variants
w_variants_v <- rep(NA, length(n_time_v))
whale_hits <- grep("whale|whales|whale's|monster|leviathan", moby_word_v)
w_variants_v[whale_hits] <- 1
plot(w_count_v,
     main = "Original Dispersion of 'whale' in Moby Dick",
     xlab = "Novel Time",
     ylab = "whale",
     type = 'h',
     ylim = c(0,1), yaxt = 'n')

plot(w_variants_v,
     main = "Dispersion of 'whale' Variants in Moby Dick",
     xlab = "Novel Time",
     ylab = "whale(s)",
     type = "h",
     ylim = c(0,1),
     yaxt = 'n'
)
# There is a difference but the weight of the instances
#   remains the same

# 4.6.2
table(moby_word_v[grep("^wh..e$", moby_word_v)])

# 4.6.3
#  using embedded functions
sort(table(moby_word_v[grep("ly$", moby_word_v)]),decreasing = TRUE)

# individual operations
ly_positions   <- grep("ly$", moby_word_v)
ly_hits        <- moby_word_v[ly_positions]
ly_frequencies <- table(ly_hits)
sorted_lys     <- sort(ly_frequencies, decreasing = TRUE)
sorted_lys[1:5]

