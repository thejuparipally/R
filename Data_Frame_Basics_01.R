#
# Storage and retrieval of Data in R
#
#
if(!require(languageR)) install.packages("languageR")

library(languageR)
data()

# Dative Alternation - simplified data set
#  Description
#   A simplified version of the dative data set, 
#   used for expository purposes only.
#

head(verbs, 12)
# verbs is a factor with the verbs as levels

# RealizationOfRec - a factor with levels NP and PP
#  NP Noun Phrase, PP Prepositional Phrase
# AnimacyOfRec - a factor with levels animate and inanimate
#
# Rec - receiver
#
# AnimacyOfTheme - a factor with levels animate and inanimate.
# LengthOfTheme - a numeric vector coding the length in words of the theme.
#                 where the length is a logarithmic transformation of
#                 the original value
#
# I.e., in the first row we see LengthOfTheme: 2.6390573
exp(2.6390573) # 14
# 2nd row
exp(1.0986123) # 3


getwd()
setwd("C:/Users/tom.tiahrt/OneDrive - The University of South Dakota/Documents/Documents/784/784_23FA")
write.table(verbs, file = "verbs.txt")

rm(list = ls()) # clear the environment

verbs_read <- read.table("verbs.txt", header = TRUE)
head(verbs_read)

#
# data frames
#  often, we need to work with a subset of columns
#  or of rows in a dataframe. We do this by subscripting
#   for a dataframe the form is 
#   dataframe_name[row, column]
#  e.g., the LengthOfTheme of the first row is
verbs[1,5] # [1] 2.639057


# if we do not specify neither row nor column
#  we get the entire dataframe
verbs[,] # truncation happens after reaching max.pring
verbs # same as immediately preceding statement

# to get just one column as a column vector
verbs[,5]

# or just one row as a row vector
verbs[1,]

# note that some ambiguity is tolerated by R
verbs[1] # but what does it mean?

# where are the NP values? 
head(verbs,8)
tail(verbs,8)

verbs[5]

verbs[5] == verbs[,5] # all TRUE for LengthOfTheme

verbs[1]
verbs[,1]
verbs[1] == verbs[,1] # Error in verbs[1] == verbs[, 1] :

# we can also use the $
verbs$LengthOfTheme
verbs$LengthOfTheme[1]

# Because the row vector has names assigned to each
#  column, we can access its elements by names
row1 <- verbs[1,]
row1["LengthOfTheme"]

# There are many times when you need to select more
#  than one row. To create your own vector from
#  scratch, you can concatenate several numbers together
(row_index <- c(1, 200, 400, 600, 800))

# then you can use those values to access the rows
#  that each index value references
verbs[row_index,]

# note that the order of the row_index values 
#  matches the rows shown

# there are several ways to create vectors. Another
#  common way is to use the colon operator
1:5
5:1
-5:-1
-5:5

verbs[row_index, 1:3]
verbs[row_index, 3:5]

# The vector of column names is a string vector and can be
#  used in accessing values
verbsRI <- verbs[row_index, ]

# return the second row only
verbsRI[2,]

# return the value by name
verbsRI[c("RealizationOfRec", "Verb", "AnimacyOfRec")]

# The same result using verb names and the row index explicitly
verbs[row_index, c("RealizationOfRec", "Verb", "AnimacyOfRec")]

# If we want to subset the rows, but by a value in a
#  column, we can use a logical equivalence condition
verbs[verbs$AnimacyOfTheme == "animate", ]

# get a grumpy response if you forget the comma!
verbs[verbs$AnimacyOfTheme == "animate" ]

# the subset command works almost exactly the same way
subset(verbs, AnimacyOfTheme == "animate")

#  except when only one variable is selected
# In that case, indexing with [] returns a vector,
#  and subset returns a dataframe with one column

# A short dataframe
dfr <- data.frame(a=-5:0, b=10:15)

# Select one column, with subscripting or subset.
dfr[,"a"]

(subdfr <- subset(dfr, select=a))

class(dfr[,"a"])
class(subdfr)

# we may use the logical AND operator &
verbs[verbs$AnimacyOfTheme == "animate" & verbs$LengthOfTheme > 2, ]

# we may use the logical OR operator |
verbs[verbs$AnimacyOfTheme == "animate" | verbs$LengthOfTheme > 3.49, ]

# Row and column names can be extracted
head(rownames(verbs))

colnames(verbs)

head(verbs$AnimacyOfRec,14)
tail(verbs$AnimacyOfRec,14)

# note that the output is followed by
#  Levels: animate inanimate
# which indicates that AnimacyOfRec is a factor
#
# Factors are automatically created by R whenever
#  R encounters non-numeric data. In statistics,
#  a factor is a non-numeric predictor or response
#  variable. In the case of AnimacyOfRec there are
#  just two possibilities, hence two levels. Many
#  statistical techniques require factors instead of
#  strings. If you want strings instead of
#  factors, you need to specify it on creation
#  or convert the values to characters
animacyOfRecAsString <- as.character(verbs$AnimacyOfRec)

head(animacyOfRecAsString)
head(verbs$AnimacyOfRec)

# Suppose we subset the data to rows that have
#  AnimacyOfRec factors with animate but not inanimate
(verbsAnimate <- verbs[1:6,])

# note that both levels are retained
verbsAnimate$AnimacyOfRec

# even though only animate remains
# to wash away the inanimate level (an uninstantiated level)
#  convert to character then back to a factor
as.factor(as.character((verbsAnimate$AnimacyOfRec)))

# the same result can be accomplished with the
#  drop option
verbsAnimate$AnimacyOfRec[drop = TRUE]

# Sorting a data frame
verbsRI[order(verbsRI$LengthOfTheme), ]

# multiple columns
verbsRI[order(verbsRI$Verb, verbsRI$LengthOfTheme), ]

# order returns a vector of index values (indices)

# if you use the sort function you can order the
#  values directly
sort(verbsRI$Verb)

sort(verbsRI$Verb)[drop = TRUE]

# but you cannot add another column as in order
sort(verbsRI$Verb, verbsRI$LengthOfTheme) # grumpiness

verbsRI[order(verbsRI$Verb, verbsRI$LengthOfTheme), ]
# concatenation does not produce the same result
sort(c(verbsRI$Verb, verbsRI$LengthOfTheme))


# while you can change one item in R (2.639057)
(verbsRI$LengthOfTheme[1] <- log(16))

# you are probably better of using Excel for singletons
# However, if you want to change an entire column
# R is much easier. Suppose we need to change the
# length values from logarithms to their original
# values
exp(verbsRI$LengthOfTheme)

# columns can be added to a data frame
# suppose we are interested in the character
# count of each verb. nchar() returns that value
# e.g., 
nchar(c("antidisestablishmentarianism", "a", "the", "card"))

# lets try it on verbs
nchar(verbsRI$Verb) # nope - naughty input to nchar

# however, factors are not strings, so if we are
#  interested in a factor we need to convert it
#  to character
(verbsRI$vLength <- nchar(as.character(verbsRI$Verb)))
verbsRI

# Contingency tables from data frames
# Cross Tabulation
#  Create a contingency table (optionally a sparse matrix)
#  from cross-classifying factors, usually contained in
#  a data frame, using a formula interface.
xtabs( ~ RealizationOfRec + AnimacyOfRec, data = verbs)
xtabs( ~ AnimacyOfRec + AnimacyOfTheme, data = verbs)

# The first argument is a FORMULA object
#  with the cross-classifying variables (separated by +)
#  on the right hand side (or an object which can be
#  coerced to a formula). Interactions are not allowed.
# The tilde (~) denotes "depends on" or "is a function of"
#  or "is distributed according to" and is known as 
#  Wilkinson-Rodgers notation

# Symbol Example    Meaning
# `~`    `y~x`      Usually separates the response (y) from the the predictors (x)
# `+`    `+x`       Include a variable
# `-`    `-x`       Remove a variable
# `:`    `x1:x2`    Include an interaction variable
#                     (element-wise product of x1 and x2)
# `*`    `x1*x2`    Equivalent to x1 + x2 + x1:x2
# `^`    `x^3`      Equivalent to x * x * x
# `1`   `-1`        Add or remove an intercept term
# `(` and `)`	
# `(x1+x2)*(x3+x4)` Force precedence of evaluation
#
# When we construct a contingency table there is no
#  dependent variable (response variable). A contingency
#  table allows us to see how counts are distributed
#  over conditions, without making any claim as to
#  whether one variable might be explainable in
#  terms of other variables. Therefore, the xtabs
#  formula has nothing to the left of the tilde

# more than two factors can be cross-tablulated
verbsXtabs <- xtabs( ~ AnimacyOfRec + AnimacyOfTheme + RealizationOfRec,
                     data = verbs)

verbsXtabs # animate themes are rare

# we can limit the results to inanimate themes by conditioning them
verbsXtabs <- xtabs( ~ AnimacyOfRec + RealizationOfRec,
                     data = verbs, subset = AnimacyOfTheme != "animate")
verbsXtabs # animate themes are removed



# if we want proportions instead of totals we can
#  divide the cell in each table by the sum
#  of all cells
# sum gives the total
sum(verbsXtabs)

# double-check the result with a Boolean evaluation
sum(verbsXtabs) == nrow(verbs[verbs$AnimacyOfTheme != "animate", ])

# obtain a table of proportions - here is where R shines
verbsXtabs / sum(verbsXtabs)
# which is better? Proportions or Counts?

# if you want percentages instead of proportions just
#  multiply by 100
100 * verbsXtabs / sum(verbsXtabs)

# sometimes you will want proportions (relative frequencies)
#  with respect to (WRT) rows or columns prop.table() can
#  calculate them. When the second argument to prop.table()
#  is 1, it returns the rel freqs WRT row totals
prop.table(verbsXtabs, 1) # rows will sum to 1

# If the second argument is 2, it returns column totals
prop.table(verbsXtabs, 2) # columns will sum to 1

# we see that row proportions differ slightly for animate
#  versus inanimate recipients. The column proportions
#  are also slightly different. There is a good reason
#  for the asymmetry between rows and columns. We
#  may assume that the difference is not due to chance.
#
# For animate recipients, NP is more common than PP.
#  However, PP is more prevalent than NP for inanimate recipients.

# Calculations on Data Frames
#  What is the complexity of the theme measured by 
#   the number of words used in expressing it as
#   it covaries with the animacy of the recipient
#  Could it be that animate recipients have more
#   complex themes than inanimate recipients?
#  To assess this we need the arithmetic mean
mean(1:9)

# calculate the animate and inanimate recipients separately
(an_mean <- mean(verbs[verbs$AnimacyOfRec == "animate", ]$LengthOfTheme))
(inan_mean <- mean(verbs[verbs$AnimacyOfRec == "inanimate", ]$LengthOfTheme))
inan_mean / an_mean # inanimate is roughly 70% as complex as animate 

# we can also use the apply family of functions to compute
#  the means simultaneously
# The tapply() function
#  (Apply a Function Over a Ragged Array , that is to each (non-empty)
#    group of values given by a unique combination of the levels of
#    certain factors.)
#  The first argument is a numeric vector to be 'meaned' the second
#   argument is how to split the first argument into groups -
#   here we split on the factor levels. The third argument defines
#   the function to be applied - here it is mean, but it could be
#   any function - even a user-defined function
tapply(verbs$LengthOfTheme, verbs$AnimacyOfRec, mean)

# It is possible to go beyond a single factor to two or more
# A list defines the multiple factors
# we can also use the with() function to make the command
#  a little shorter
with(verbs, tapply(LengthOfTheme, 
                   list(AnimacyOfRec, AnimacyOfTheme), mean))

head(heid, 20)
tail(heid, 20)

# sometimes barplots reveal the data well
barplot(xtabs( ~ RealizationOfRec + AnimacyOfRec, data = verbs),beside=TRUE)
barplot(xtabs( ~ LengthOfTheme + AnimacyOfTheme, data = verbs),beside=TRUE)

