# Instructions for Homework for week 3
#
# Be sure to load languageR
if(!require(languageR)) install.packages("languageR")

library(languageR)
data()
ratings$Word

# Subset the words in the ratings data set to those that begin with a, b, or c only
#  using a regular expression. Name it 
#   sub_word_abc
# sub_word_abc <- ____________
#
data(ratings)
sub_word_abc <- ratings[ratings$Word %in% grep("^[abc]", ratings$Word, value = TRUE), ]
View(sub_word_abc)

# Strip the unused levels from sub_word_abc
#  the reduction should be from 81 to 25
sub_word_abc <- droplevels(sub_word_abc)
nlevels(sub_word_abc$Word)

# Sort the data frame by word length in ascending order
sub_word_abc$Word <- as.character(sub_word_abc$Word)
sub_word_abc <- sub_word_abc[order(nchar(sub_word_abc$Word)), ]
View(sub_word_abc)

# Sort the data frame by word length in descending order
sub_word_abc$Word <- as.character(sub_word_abc$Word)
sub_word_abc <- sub_word_abc[order(-nchar(sub_word_abc$Word)), ]
View(sub_word_abc)

#
# Cross Tabulation
#  Create a contingency table (optionally a sparse matrix)
#  from cross-classifying factors, usually contained in
#  a data frame, using a formula interface.
# use xtabs, Wilkinson-Rodgers, tables to 
#  produce frequencies for the ratings data set
#  for Class and Complex

contingency_table <- xtabs(~ Class + Complex, data = ratings)
print(contingency_table)


head(oz,24)
print(oz)

#
# Find the number of occurrences of dorothy from the dataset "oz"
#  

dorothy_count <- sum(grepl("dorothy", oz, ignore.case = TRUE))
print(dorothy_count)

# do a dorothy specific count in"oz"
dorothy_specific_count <- sum(oz == "dorothy")
print(dorothy_specific_count)

# create a dispersion plot from the dataset "oz" for dorothy

dorothy_variants <- rep(NA, length(oz))
dorothy_hits <- grep("Dorothy|dorothy|DOROTHY", oz)
dorothy_variants[dorothy_hits] <- 1

# Create a dispersion plot for "Dorothy" variants
plot(dorothy_variants,
     main = "Dispersion of 'Dorothy' Variants in 'oz'",
     xlab = "Position",
     ylab = "Dorothy",
     type = "h",
     ylim = c(0, 1),
     yaxt = 'n'
)

# create a dispersion plot from the dataset "oz"
#  for "Lion"

lion_variants <- rep(NA, length(oz))
lion_hits <- grep("Lion|lion|LION", oz)
lion_variants[lion_hits] <- 1

plot(lion_variants,
     main = "Dispersion of 'Lion' Variants in 'oz'",
     xlab = "Position",
     ylab = "Lion",
     type = "h",
     ylim = c(0, 1),
     yaxt = 'n'
)


# create a dispersion plot from the dataset "oz" for "Cowardly Lion"

oz_lower <- tolower(oz)

cowardly_lion_variants <- rep(NA, length(oz))
lion_indices <- grep("lion", oz_lower)
cowardly_indices <- grep("cowardly", oz_lower)

for (i in cowardly_indices) {
  if ((i + 1) %in% lion_indices) {
    cowardly_lion_variants[c(i, i + 1)] <- 1
  }
}

plot(cowardly_lion_variants,
     main = "Dispersion of 'Cowardly Lion' in 'oz'",
     xlab = "Position",
     ylab = "Cowardly Lion",
     type = "h",
     ylim = c(0, 1),
     yaxt = 'n'
)

