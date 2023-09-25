# TAWR Ch. 6
#
# remove all the objects from memory
rm(list = ls())

setwd("/Users/theju/Desktop/R")

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

whale_l  <- lapply(chapter_freqs_l, '[', 'whale')
whales_m <- do.call(rbind, whale_l)
ahab_l   <- lapply(chapter_freqs_l, '[', 'ahab')
ahabs_m  <- do.call(rbind, ahab_l)
whales_v <- as.vector(whales_m[,1])
ahabs_v  <- as.vector(ahabs_m[,1])

whales_ahabs_m <- cbind(whales_v, ahabs_v)
colnames(whales_ahabs_m) <- c("whale", "ahab")

whales_ahabs_m[1:16,]

# transform the NAs into zeroes
whales_ahabs_m[which(is.na(whales_ahabs_m))] <- 0

# alternatively
the_na_positions <- which(is.na(whales_ahabs_m))
whales_ahabs_m[the_na_positions] <- 0

# overwriting the NAs with zeros allows correlation computation
cor(whales_ahabs_m)

# instead of a table just get the number
mycor <- cor(whales_ahabs_m[,"whale"], whales_ahabs_m[,"ahab"])
mycor

# considering data frames
#
# start with a matrix
x <- matrix(data = 1, nrow = 3, ncol = 3)
x

class(x[1, 2])

x[1, 2] <- "Sam I am"
x
class(x[1, 2])
class(x[1, 1])

x <- matrix(1, 3, 3)
x_df <- as.data.frame(x)
x_df

x_df[1, 2] <- "Sam I am"
x_df

class(x_df[1, 2])
class(x_df[1, 1])
class(x_df[3, 3])

x_df[, 2]

x_df[, "V2"]
x_df$V2

# convert the matrix object into a data frame
cor_data_df <- as.data.frame(whales_ahabs_m)
cor(cor_data_df)

# get a random sample
sample(cor_data_df$whale)

# get the correlation of a randomly shuffled whale column
#  to the ahab column
cor(sample(cor_data_df$whale), cor_data_df$ahab)

# use a loop
mycors_v <- NULL
for(i in 1:10000){
  mycors_v <- c(mycors_v, 
                cor(sample(cor_data_df$whale), 
                    cor_data_df$ahab))
}
min(mycors_v)
max(mycors_v)
range(mycors_v)
mean(mycors_v)
sd(mycors_v)

# create a histogram
h <- hist(mycors_v, breaks = 100, col = "grey",
          xlab = "Correlation Coefficient",
          main = "Histogram of Random Correlation\n
                  Coefficients with Normal Curve", plot = TRUE)

xfit <- seq(min(mycors_v), max(mycors_v), length = 1000)
yfit <- dnorm(xfit, mean = mean(mycors_v), sd = sd(mycors_v))

h$mids[1:2]
diff(h$mids[1:2])
length(mycors_v)

yfit
yfit <- yfit * diff(h$mids[1:2]) * length(mycors_v)

lines(xfit, yfit, col = "black", lwd = 2)

?cor.test
cor.test(cor_data_df$whale, 
         cor_data_df$ahab, alternative = "two.sided")

# Practice
#
# 1. Add two more columns to the matrix with data
#    for the words i and my and then rerun the
#    cor() function. Though we have only used cor()
#    for two columns so far, we can use it just as
#    easily as on a matrix with two or more
#    columns. Do not forget to set the frequencies
#    for any chapters where the word _does_ _not_
#    occur to zero. What does the result tell you
#    about the usage of the words i and my?
#
my_l <- lapply(chapter_freqs_l, "[", "my")
my_m <- do.call(rbind, my_l)

my_v <- my_m[,1]

i_l  <- lapply(chapter_freqs_l, "[", "i")
i_m  <- do.call(rbind, i_l)
i_v  <- i_m[,1]

whales_ahabs_my_i_m <- cbind(whales_v, ahabs_v, my_v, i_v)
whales_ahabs_my_i_m[which(is.na(whales_ahabs_my_i_m))] <- 0
cor(whales_ahabs_my_i_m)


# 2. Calculate the correlation coefficient for i and
#    my and run a randomization test to evaluate
#    whether the results are significant.
my_i_m <- cbind(my_v, i_v)
my_i_m[which(is.na(my_i_m))] <- 0

my_i_cor_data_df <- as.data.frame(my_i_m)
cor(my_i_cor_data_df$i_v, my_i_cor_data_df$my_v)


i_my_cors_v <- NULL
for(i in 1:10000){
  i_my_cors_v <- c(i_my_cors_v,
                   cor(sample(my_i_cor_data_df$i_v),
                   my_i_cor_data_df$my_v))
}
min(i_my_cors_v)
max(i_my_cors_v)
range(i_my_cors_v)
mean(i_my_cors_v)
sd(i_my_cors_v)


