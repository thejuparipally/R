# TAWR Ch. 11
#
# remove all the objects from memory
rm(list = ls())

setwd("/Users/theju/Desktop/R")

# Startup code
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

# 11.2 Using stack to Create a Data Frame
#  Move data from a list into a data frame (example)
mean_word_use_m <- do.call(rbind, lapply(chapter_raw_l, mean))

# If, instead of the mean word use in a chapter, we
#  wanted to know the length of each chapter in words
#  we could use the sum function instead of mean
chapter_lengths_m <- do.call(rbind, lapply(chapter_raw_l, sum))

# Another way to achieve the same result is to use the
#  stack function instead of do.call. The stack function
#  combines, or "concatenates" data from several 
#  data structures into one:
chapter_lengths_df <- stack(lapply(chapter_raw_l, sum))
# N.B. the stack () call returns a data frame object
#  instead of a matrix.
class(chapter_lengths_m)
class(chapter_lengths_df)

# if you examine the resulting data frame
#  (chapter_lengths_df) you will see that it has two
#  columns and that the columns are labeled "values"
#  and "ind"
head(chapter_lengths_df)

# To access the values column as a vector,
#  use the $ shortcut
head(chapter_lengths_df$values)

# Recall how we found the hapax legomena count
#  in each chapter of Moby Dick
chapter_hapax_v <- sapply(chapter_raw_l, function(x) sum(x == 1))
head(chapter_hapax_v)

# You can achieve a similar result 
#  using lapply and stack
chap_haps_df_l <- lapply(chapter_raw_l, function(x) sum(x == 1))
head(chap_haps_df_l)
chap_haps_df   <- stack(chap_haps_df_l)
head(chap_haps_df)

# The foremost advantage of stack is that it returns
#  a data frame which is easy to manipulate and
#  essential if you want to use dplyr and the
#  the tidyverse
#
# By using stack we have two data frames, one that
#  holds the total number of words per chapter,
#  chapter_lengths_df and another that has the
#  hapax legomena count per chapter, chap_haps_df.
# The two can be combined using the data.frame
#  function
a_data_frame <- data.frame(chapter_lengths_df, chap_haps_df)
head(a_data_frame)

# To improve the understandability of the data:
hap_lens_df <- data.frame(
  chap_names <- chapter_lengths_df$ind,
  chapter_lengths <- chapter_lengths_df$values,
  num_hapax <- chap_haps_df$values
)
head(hap_lens_df)

names(hap_lens_df) <- c("chap_names", "chap_lengths", "num_hapax") 
head(hap_lens_df)

# 11.3 Installing and Loading dplyr
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

# 11.4 Using mutate, filter, arrange, and select
# 11.4.1 Mutate
#  mutate is a way of "cbinding" new columns
#  into a data frame. mutate allows you to create
#  a new column by performing a calculation.
#  E.g., calculating the percentage of each 
#   chapter that is composed of hapax.
#
hap_percent <- num_hapax/chapter_lengths
new_df <- mutate(hap_lens_df, hap_percent)
head(new_df)  

barplot(new_df$hap_percent,
        names.arg = seq(1:length(chapter_raw_l)),
        xlab = "Chapter",
        ylab = "Percentage")
nice_df <- mutate(new_df, gsub("\\..*$", "", chap_names))
head(nice_df)

short_title <- gsub("\\..*$", "", chap_names)
head(short_title)
nice_df <- mutate(new_df, short_title)
head(nice_df)
colnames(nice_df)

# 11.4.2 filter()
#  The filter() function takes the place of which().
#  It allows for the selection of items that meet
#  a specific condition. The most common ue of filter 
#  is to find rows in a data frame that match some 
#  condition. E.g., use filter to identify rows
#  in new_df that have a percentage of hapax 
#  greater than some arbitrarily chosen percentage,
#  such as 0.5
filter(new_df, hap_percent > 0.5)

# 11.4.3 select
#  What filter() does for finding rows, select()
#  does for columns. E.g., using select() to
#  isolate all the hapax percentages from nice_df
head(select(nice_df, hap_percent))

# dplyr gives you the capability to chain
#  functions together using a special operator
#  %>%
#  That operator, the "chain" operator, is
#  named "then" and it works like a pipe | in UNIX.
filter(nice_df, hap_percent > 0.5) %>%
  select(short_title, hap_percent)

# Using just the greater than 0.5 values,
#  output the Tukey five-number summary
#  (six because of the mean)
filter(nice_df, hap_percent > 0.5) %>%
  select(hap_percent) %>%
  summary()

# Two-column example
filter(nice_df, hap_percent > 0.5) %>%
  select(hap_percent, chap_lengths) %>%
  summary()

# 11.4.4 arrange
#  arrange is similar to sort; it places items
#  in ascending or descending order. Here
#  filter nice_df to include the rows with a
#  hapax_percentage greater than 40% and
#  select the short title and hapax percentage
#  columns in ascending order by hapax percentage
filter(nice_df, hap_percent > 0.4) %>%
  select(short_title, hap_percent) %>%
  arrange(hap_percent)

# To reverse the order modify the parameter sent
#  to arrange
filter(nice_df, hap_percent > 0.4) %>%
  select(short_title, hap_percent) %>%
  arrange(desc(hap_percent))

# 11.5 Practice
# 1. What do the three chapters with hapax percentages
#    grater than 0.5 all have in common. Use dplyr's
#    summary() function to verify your guess
filter(nice_df, hap_percent > 0.4) %>%
  select(short_title, chap_lengths, hap_percent) %>%
  arrange(chap_lengths)

# Book
#  the filtered chapters
filter(nice_df, hap_percent > 0.5)

# examine the five-number
select(nice_df, chap_lengths) %>%
  summary()

# 2. Modify the code to identify rows with
#    a hap_percent less than 0.2. What do
#    these chapters have in common?
filter(nice_df, hap_percent < 0.2) %>%
  select(short_title, chap_lengths, hap_percent) %>%
  arrange(hap_percent)
select(nice_df, chap_lengths) %>%
  summary()

# 3. One of the chapters found in problem #2
#    is an outlier. What is unusual about it?
filter(nice_df, hap_percent < 0.2) %>%
  select(short_title, chap_lengths, hap_percent) %>%
  arrange(desc(hap_percent))
# What is unusual about the outlier is that its
#  length is much smaller than the other chapters
#  that are less than 0.2 hapax legomena

# 4. Mutate nice_df into a new data frame called
#    repeat_df
#    that includes a new column called
#    repeat_words
#    that is calculated by subtracting the number
#    of hapax in each chapter from the total
#    number of words in each chapter. Use the 
#    %>% operator to filter the results such
#    that only rows with repeat words greater
#    than 3000 are retained. Select short title,
#    chap lengths, and repeat words and arrange
#    the resulting data from largest to smallest
#    repeat words. Everything you need is 
#    already in nice df.

repeat_df <- mutate(nice_df,
  repeat_words = nice_df$chap_lengths - nice_df$num_hapax) %>%
  filter(repeat_words > 3000) %>%
  select(short_title, chap_lengths, repeat_words) %>%
  arrange(desc(repeat_words))
repeat_df

# 5. Mutate repeat_df again to include a new column
#    that calculates the rate at which repeated
#    words are repeated in each chapter
done_df <- mutate(repeat_df,
                  repeat_rate = chap_lengths / repeat_words) %>%
  arrange(desc(repeat_rate))
done_df

# 6. Start with the nice_df from above and use 
#    the %>% operator to do all of the 
#    following in one expression.
# * Mutate nice df to extract the chapter number
#    from the short title column as a new 
#    column called chap_num
# * Filter the data to keep chapters with 
#    word counts greater than 3000
# * Select all of the columns except for
#    chap names (HINT: there is an easy
#    to do this using the minus operator)
# * Second mutate - mutate the chap_num
#    column which is currently a character
#    vector into a numeric vector using
#    mutate and as.numeric
# * Arrange the result by descending
#    chapter number
final_df <- mutate(
  nice_df,
  chap_num = gsub("CHAPTER ", "", short_title)
  ) %>%
  filter(chap_lengths > 3000) %>%
  select(-chap_names) %>%
  mutate(as_num = as.numeric(chap_num)) %>%
  arrange(desc(as_num))
final_df

# 7. Why was the second mutate necessary?
#    It was needed to convert character data
#    to numeric, ensuring that the descending
#    order makes sense.


