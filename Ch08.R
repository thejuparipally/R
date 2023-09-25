# TAWR Ch. 8
#
# remove all the objects from memory
rm(list = ls())

setwd("/Users/theju/Desktop/R")

# 8.2 Start Up Code
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
chapter_lengths_m <- do.call(rbind,
                             lapply(chapter_raw_l, sum))

# 8.3 sapply (simplified apply)
#  We meed a count of the words that appear 
#   exactly once in each chapter.
#  use sapply with a custom sum function that
#   counts only the hapax legomena in each
#   chapter.
#  The main difference between lapply and sapply
#   is that lapply produces a list and sapply
#   returns a vector

# 8.4 An Inline Conditional Function
#  Create a custom function that will
#  contain a conditional statement that 
#  totals only the singletons. The R operator
#  that assures equality is == or two equal
#  signs
chapter_hapax_v <- sapply(chapter_raw_l,
                          function(x){sum(x == 1)})
head(chapter_hapax_v)

# next, divide the hapax count by the 
#  total number of words in each chapter
#  but we need the lengths to do so
chapter_lengths_m <- do.call(rbind,
                             lapply(chapter_raw_l, sum))

# one at a time looks like
chapter_hapax_v[1] / chapter_lengths_m[1]
chapter_hapax_v[2] / chapter_lengths_m[2]

# but R supports doing them all at once
hapax_percentage <- chapter_hapax_v / chapter_lengths_m 

barplot(hapax_percentage,
        beside = TRUE,
        col = "grey",
        names.arg = seq(1:length(chapter_raw_l)),
        xlab = "Chapters",
        ylab = "Hapax Percentage")
cor(chapter_hapax_v, chapter_lengths_m)


# 8.5 Practice
# 1. Use order to rank the values in hapax_percentage
#    How does the rank of the cetology chapter
#    compare to the others?
#
an_order <- order(hapax_percentage, decreasing = TRUE)

ndx <- which(an_order == 132, arr.ind = TRUE)
ndx
hapax_percentage[ndx]
ndx / length(an_order)

# 2. The correlation statistic found at the end
#    of chapter is not especially useful in and
#    of itself. It becomes much more interesting
#    when compared to another author's work, such
#    as Jane Austen's Sense and Sensibility. 
#    Save these in a list item titled sense_raws_l
#    From this object calculate the number of
#    hapax and the chapter lengths for Sense and
#    Sensibility. Compute the correlation and
#    describe how the correlation results for 
#    Melville and Austen compare and what they
#    tell us about the two writers in terms
#    of vocabulary usage habits.
#
# From the chapter 3 practice
#
ss_text_v           <- scan("text/austen.txt", what = "character", sep = "\n")
ss_start_v          <- which(ss_text_v == "CHAPTER 1")
ss_end_v            <- length(ss_text_v)
ss_novel_lines_v    <- ss_text_v[ss_start_v:ss_end_v]

ss_chap_positions_v <- grep("^CHAPTER \\d", ss_novel_lines_v)
ss_last_position_v  <- length(ss_novel_lines_v)
ss_chap_positions_v <- c(ss_chap_positions_v, 
                         ss_last_position_v)
ss_chapter_raw_l <- list()
ss_chapter_freqs_l <- list()
ss_len_ch_pos_v <- length(ss_chap_positions_v)
for(i in 1:ss_len_ch_pos_v){
  if(i < ss_len_ch_pos_v){ 
    ss_chapter_title <- novel_lines_v[ss_chap_positions_v[i]]
    ss_start <- ss_chap_positions_v[i] + 1 # one past starting line (CHAPTER n)
    ss_end   <- ss_chap_positions_v[i + 1] - 1 # one before the next CHAPTER
    ss_chapter_lines_v <- novel_lines_v[ss_start:ss_end] # words bounded by CHAPTER lines
    ss_chapter_words_v <- tolower(paste(ss_chapter_lines_v, collapse = " "))
    ss_chapter_words_l <- strsplit(ss_chapter_words_v, "\\W")
    ss_chapter_words_v <- unlist(ss_chapter_words_l)
    ss_chapter_words_v <- ss_chapter_words_v[which(ss_chapter_words_v != "")]
    ss_chapter_freqs_t <- table(ss_chapter_words_v)
    
    # double brackets assigns a name or label to the list item
    #  each item is named by the chapter title. Without naming 
    #  the items will have an index number assigned to it
    ss_chapter_raw_l[[ss_chapter_title]] <- ss_chapter_freqs_t
    
    # convert the raw counts to relative frequencies
    ss_chapter_freqs_t_rel <- 100 * 
      (ss_chapter_freqs_t/sum(ss_chapter_freqs_t))
    ss_chapter_freqs_l[[ss_chapter_title]] <- ss_chapter_freqs_t_rel
  }
}
ss_chapter_lengths_m <- do.call(rbind,
                             lapply(ss_chapter_raw_l, sum))

ss_chapter_hapax_v <- sapply(ss_chapter_raw_l,
                          function(x){sum(x == 1)})
head(ss_chapter_hapax_v)
ss_hapax_percentage <- 
  ss_chapter_hapax_v / ss_chapter_lengths_m 

barplot(ss_hapax_percentage,
        beside = TRUE,
        col = "grey",
        names.arg = seq(1:length(ss_chapter_raw_l)),
        xlab = "Chapters",
        ylab = "Sense and Sensibility Hapax %")
cor(ss_chapter_hapax_v, ss_chapter_lengths_m)
cor(chapter_hapax_v, chapter_lengths_m)

#    
# 3. 
#
md_cors_v <- NULL
ss_cors_v <- NULL
for(i in 1:10000){
  md_cors_v <- c(md_cors_v, 
                cor(sample(chapter_hapax_v), 
                    chapter_lengths_m))
  ss_cors_v <- c(ss_cors_v,
                 cor(sample(ss_chapter_hapax_v),
                     ss_chapter_lengths_m))
}
cor(chapter_hapax_v, chapter_lengths_m)
mean(md_cors_v)
sd(md_cors_v)

cor(ss_chapter_hapax_v, ss_chapter_lengths_m)
mean(ss_cors_v)
sd(ss_cors_v)




