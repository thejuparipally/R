# #
# Random Variables - the outcome of an experiment
# e.g.,
#  flipping a coin; throwing a pair of fair dice; counting the words in a doc;
#  counting the customers that go through the drive thru at a bank
#  recording the elapsed time it takes to drive from Vermillion
#  to Sioux Falls
#
# Visualizing a single random variable
#
if(!require(languageR)) install.packages("languageR")

library(languageR)

names(ratings)
colnames(ratings)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

barplot(xtabs( ~ ratings$Length), main = "81 Words from 'ratings'", 
        xlab = "word length", col = "red")
text(x = 7, y = 18, 
     label = paste("mean:", round(mean(ratings$Length), 4)), adj = 0)
text(x = 7, y = 16.5, 
     label = paste("sd:", round(sd(ratings$Length), 4)), adj = 0)
text(x = 7, y = 15, 
     label = paste("median:", round(median(ratings$Length), 4)), adj = 0)
text(x = 7, y = 13.5, 
     label = paste("mode:", Mode(ratings$Length)), adj = 0)
text(x = 7, y = 12, 
     label = paste("range:", 
                   range(ratings$Length)[1], "-",
                   range(ratings$Length)[2]), adj = 0)


# Modern Applied Statistics with S
#
if(!require(MASS)) install.packages("MASS")

library(MASS)


# Length
#  a numeric vector for the length of the word in letters.
truehist(ratings$Length,
         main = "Proportions of Word Lengths",
         xlab = "word length", col = "red")

truehist(ratings$Frequency,
         main = "Logarithms of Word Frequencies",
         xlab = "log of word frequency", col = "blue")

# SynsetCount
# a numeric vector with logarithmically transformed counts
# of the number of synonym sets in WordNet in which the word is listed.
truehist(ratings$SynsetCount,
         main = "Logarithm of Word Synonyms Counts",
         xlab = "synset counts", col = "darkorange")

# FamilySize
# a numeric vector of logarithmically transformed morphological family sizes.
truehist(ratings$FamilySize,
         main = "Logarithm of Morphological Family Sizes",
         xlab = "log family size", col = "green")

# DerivEntropy
# a numeric vector with the derivational entropies of the words
truehist(ratings$rInfl,
         main = "Logarithm of Derivational Entropies",
         xlab = "log family size", col = "purple")


windows()
par(mfrow = c(3,2)) # now redo the above but in one panel

barplot(xtabs( ~ ratings$Length), main = "Words by length from 'ratings'", 
        xlab = "word length", col = "red")
truehist(ratings$Length,
         main = "Proportions of Word Lengths",
         xlab = "word length", col = "yellow")
truehist(ratings$Frequency,
         main = "Logarithms of Word Frequencies",
         xlab = "log of word frequency", col = "blue")
truehist(ratings$SynsetCount,
         main = "Logarithm of Word Synonyms Counts",
         xlab = "synset counts", col = "darkorange")
truehist(ratings$FamilySize,
         main = "Logarithm of Morphological Family Sizes",
         xlab = "log family size", col = "green")
truehist(ratings$rInfl,
         main = "Logarithm of Derivational Entropies",
         xlab = "log family size", col = "purple")

# Histogram shape depends to a great extent on bin size.
# The truehist() function
#  tries to minimize the risk of an arbitrarily shaped 
#  histogram. In spite of the defaults used by histogram()
#  the results imply discrete numbers when the actual
#  distribution is smooth. The problem can be avoided by
#  using the density function, which will produce a 
#  smoothed histogram.
#
# As an example, we can use the the reaction times elicited
#  by a visual lexical decision experiment using the same
#  words that can be found in the ratings data set. The
#  reaction times for 79 of the 81 words in the set comprise
#  the dataset lexdec.
#
# let us set the parameters to show one chart only
#
par(mfrow = c(1,1)) # one chart only
?lexdec
# Lexical decision latencies for 79 English nouns
#  Description
#    Lexical decision latencies elicited from 21 subjects for 
#    79 English concrete nouns, with variables linked to 
#    subject or word.
#
# First, let us examine the logarithmically transformed
#  reaction times as a histogram
# 
truehist(lexdec$RT, col = "red",
         main = "Logarithmically Transformed Reaction Times",
         xlab = "log RT")
# We see a somewhat skewed distribution with a long
#  (extended) right tail of long latentcies.
#
# Next, we see the density function's alteration of the
#  data
#
# We begin by using the standard histogram function hist(),
#  which can produce a histogram object. That histogram object
#  can be used to adjust the axes and ensure that the
#  appropriate scale is shown. In the instant case
#  we can create an hist object without an associated plot.
# h <- hist(lexdec$RT, freq = FALSE, plot = FALSE)
h <- hist(lexdec$RT, plot = FALSE)

# The histogram object produced by hist has several components,
#  of which we need two for the purposes of this demonstration.
#  Those two are (1) the locations of the edges of the bar, and 
#  (2) the heights of the bars. Each is available as a
#  component of the object. Their names are breaks and density
#  To provide easy access use a density object
d <- density(lexdec$RT)

# The density object gives us the x and y coordinates of the
#  graph. The smallest and largest values of each are 
#  available to set the limits of the x and y axes. 
#
# Using the the range() function to obtain the smallest
#  and largest values from the input:
(xlimit <- range(h$breaks, d$x))
(ylimit <- range(0, h$density, d$y))

# We use zero for the y-axis (the vertical axis)
#  because we want the origin to be included as the
#  lowest value
xlimit[1]
xlimit[2]

# note that the breaks option gets hist() to produce the same
#  output as truehist()
hist(lexdec$RT, freq = FALSE, xlim = xlimit, 
     ylim = ylimit, main = "Logarithimic Transformed Reaction Time",
     xlab = "log Reaction Time", 
     col = "lightgray", 
     border = "darkgray", 
     breaks = seq(xlimit[1], xlimit[2], by = 0.1))


# next, let us stack the two charts one on top of the other to
#  clarify the similarity
par(mfrow = c(2,1))

# seen above
truehist(lexdec$RT, col = "red",
         main = "Logarithmically Transformed Reaction Times",
         xlab = "log RT")


hist(lexdec$RT, freq = FALSE, xlim = xlimit, 
     ylim = ylimit, main = "Logarithimic Transformed Reaction Time",
     xlab = "log Reaction Time", 
     col = "lightgray", 
     border = "darkgray", 
     breaks = seq(xlimit[1], xlimit[2], by = 0.1))

# return to the hist object-based product all by itself
par(mfrow = c(1,1))
hist(lexdec$RT, freq = FALSE, xlim = xlimit, 
     ylim = ylimit, main = "Logarithimic Transformed Reaction Time",
     xlab = "log Reaction Time", 
     col = "lightgray", 
     border = "darkgray", 
     breaks = seq(xlimit[1], xlimit[2], by = 0.1))

# add the curve for the density using lines
lines(d$x, d$y, lwd = 2)

# as a strictly technical consideration the specification
#  for lines can be reduced because using the x and y
#  attributes explicitly means that the lines() will know
#  how to extract and use them.
lines(d, lwd = 3, col = "red") # 3 to make it obvious

# the general plotting function plot() can make the
#  chart for us without specifying the x and y values
#  directly
plot(h)
plot(d)

# the essential point is that R's default function can
#  provide a perfectly reasonable interpretation of the input
#  submitted to it
plot(sort(lexdec$RT), ylab = "log response time")

# When you send a variable to plot(), it assumes your
#  vector is a sequence of Y-values to be matched one-to-one
#  with an index sequence of X-values comprising the counting
#  numbers 1 to the count of Y-values. When the values
#  are sorted, the result is that plot has the rank
#  of each value shown in the plot
# We can refine that by using quartiles
plot(quantile(lexdec$RT), 
     xlab = "Quartiles", ylab = "log RT", pch = 16) # pch 16 gives us the filled in dots


# The index numbers along the x-axis are infelicitous.
#  Let us improve them by forcing them to be what we need
#  them to be
# The mtext() function can place text in the margins
#  to indicate the x locations
#
plot(quantile(lexdec$RT), xaxt = "n", 
     xlab = "Quartiles", ylab = "log RT", pch = 16)
mtext(c("0%","25%", "50%", "75%", "100%"), 
      side = 1, at = 1:5, line = 1, cex = 0.8 )

# note that the at = 1:5 indicates the locations
#  where mtext will place the text. Where the
#  earlier plot placed the quartiles was based on
#  the simple sequence of index values implicit in the
#  data, here we have adorned those data points with explicit
#  labels 
#
# Let is progress from quartiles to deciles
#
plot(quantile(lexdec$RT, seq(0,1,0.1)),
     xaxt = "n", xlab = "Deciles", ylab = "log response time",
     pch = 16)
# now we add the labels and use paste0 to paste without
#  intervening characters between the ticks
mtext(paste0(seq(0,100,10), rep("%",11)),
      side = 1, at = 1:11, line = 1, cex = 0.8)

# what does the seq function do?
seq(0, 1, 0.1) # it provide the values that comprise the input

# What does quartile do given the seq
quantile(lexdec$RT, seq(0, 1, 0.1))

# suppressing the default tick marks is done by
#  the xaxt = 'n' setting

# paste takes two or more strings and 'pastes' them
#  together and separate them with a space (by default)
paste("1", "A", "2", "B", "3", "C")
paste("dog", "cat", "canary")

# you can change the separation character
paste("dog", "cat", "canary", sep = ";")

# and eliminate it
paste("dog", "cat", "canary", sep = "")
# or through a separate function
paste0("dog", "cat", "canary")

paste("dog", "cat", "canary", sep = "-")

# this how we were able to set the x axis values
paste0(seq(0,100,10), rep("%",11))

# we can orient the text along the x axis horizontally
#
plot(quantile(lexdec$RT, seq(0,1,0.1)),
     xaxt = "n", xlab = "Deciles", ylab = "log response time",
     pch = 16)
# the las = 2 rotates the decile value by 90 degrees
mtext(paste0(seq(0,100,10), rep("%",11)),
      side = 1, at = 1:11, line = 1, cex = 0.9, las = 2)

# now let us graph the estimated density, the ordered
#  values, and a box and whiskers plot
par(mfrow = c(1,3))
h <- hist(exp(lexdec$RT), plot = FALSE)
d <- density(exp(lexdec$RT))

(xlimit <- range(h$breaks, d$x))
(ylimit <- range(0, h$density, d$y))

plot(exp(lexdec$RT), xlim = xlimit, 
     ylim = ylimit, main = "RT in msec",
     xlab = "Reaction Time",
     ylab = "Density",
     col = "lightgray")

lines(d, lwd = 3, col = "red") # 3 to make it obvious

plot(sort(exp(lexdec$RT)), main = "RT in msec")

# use exp to invert the transformation
boxplot(exp(lexdec$RT), main = "RT in msec")


# non-transformed results
h <- hist(lexdec$RT, plot = FALSE)
d <- density(lexdec$RT)

(xlimit <- range(h$breaks, d$x))
(ylimit <- range(0, h$density, d$y))

plot(lexdec$RT, xlim = xlimit, 
     ylim = ylimit, main = "Log RT in msec",
     xlab = "Log Reaction Time",
     ylab = "Density",
     col = "lightgray")
lines(d, lwd = 3, col = "red") # 3 to make it obvious

plot(sort(lexdec$RT), main = "Log RT in msec")

boxplot(lexdec$RT, main = "Log RT in msec")

windows()
par(mfrow = c(2,3))
h <- hist(exp(lexdec$RT), plot = FALSE)
d <- density(exp(lexdec$RT))

(xlimit <- range(h$breaks, d$x))
(ylimit <- range(0, h$density, d$y))

plot(exp(lexdec$RT), xlim = xlimit, 
     ylim = ylimit, main = "RT in msec",
     xlab = "Reaction Time",
     ylab = "Density",
     col = "lightgray")

lines(d, lwd = 3, col = "red") # 3 to make it obvious

plot(sort(exp(lexdec$RT)), main = "RT in msec")

# use exp to invert the transformation
boxplot(exp(lexdec$RT), main = "RT in msec")


# non-transformed results
h <- hist(lexdec$RT, plot = FALSE)
d <- density(lexdec$RT)

(xlimit <- range(h$breaks, d$x))
(ylimit <- range(0, h$density, d$y))

plot(lexdec$RT, xlim = xlimit, 
     ylim = ylimit, main = "Log RT in msec",
     xlab = "Log Reaction Time",
     ylab = "Density",
     col = "lightgray")
lines(d, lwd = 3, col = "red") # 3 to make it obvious

plot(sort(lexdec$RT), main = "Log RT in msec")

boxplot(lexdec$RT, main = "Log RT in msec")

# comparing the upper level to the lower level shows that
#  the skewing is reduced by the log tranformation, but 
#  not eliminated. The boxplot makes it easy to see.
#  There are still quite a few outliers, but their number
#  is fewer the box itself is closer to the center of
#  the plot
# This example demonstrates the utility of a logarithmic
#  transformation - it reduces or elimites skew in the data.
#  The transformation is necessary for some statistical
#  techniques to work. Without a logarithmic transformation
#  high leverage points can dominate the output. There can be
#  trends partially or completely obscured by them.
#
# In statistics, a high leverage point is an observation
#  that has extreme predictor values. It is an observation
#  whose predictor value far from the other observations.
#  High leverage points can have a significant impact
#   on the regression analysis; The predicted responses, 
#   the estimated slope coefficients, or the hypothesis test
#   may be dramatically affected.
# High leverage points differ from outliers. 
#   An outlier is a data point whose response value does
#   deviates from the general trend of the rest of the data
#  
# _________________________________________________________
# _________________________________________________________
#  
# Previously we worked with the animacy and NP/PP data
#
(verbsXtabs <- xtabs( ~ AnimacyOfRec + RealizationOfRec,
    data = verbs[verbs$AnimacyOfTheme != "animate", ]))

# we can use a bar plot to visual this data
par(mfrow = c(1,2))
barplot(verbsXtabs, legend.text = c("anim","inanim"), 
        col = c("red","blue"))
barplot(verbsXtabs, beside = TRUE, 
        legend.text = rownames(verbsXtabs), 
        col = c("red","blue"))

# I recommend that you use the side-by-side version
#  because it is difficult to tell the relative size
#  in the first chart
(verbsXtabs <- xtabs( ~ AnimacyOfRec + AccessOfRec + RealizationOfRecipient,
   data = dative))

# The data could be placed in a bar plot but there are
#  too many combinations to see it well
# instead use mosaic plot
windows()
par(mfrow = c(1,1))
mosaicplot(verbsXtabs, main = "Dative")

# The areas of the twelve rectangles are proportional
#  to the counts of the twelve cells in the contingency
#  tables
#
# The relation between two numeric variables with many
#  values can be revealed by a scatterplot. Sometimes
#  we need to build the results in multiple steps

# Step 1 - plot the scattered points
plot(ratings$Frequency, ratings$FamilySize, pch = 16)

# Step 2 - recall the use of lowess in DSCI 724
#  i.e., use lowess to plot a Scatter Plot Smoothing line
#  This function performs the computations for the
#  LOWESS smoother which uses locally-weighted
#  polynomial regression
lines(lowess(ratings$Frequency, ratings$FamilySize),
      col = "red", lwd = 2.5)

# By default, the name of each vector are used as
#  axes label
# The plot shows that there is a substantial positive correlation
#  between frequency and family size. Recall the definitions:
#
# Frequency
#  a numeric vector of logarithmically transformed frequencies
# FamilySize
#  a numeric vector of logarithmically transformed morphological
#  family sizes
# 
# Simultaneously, there is quite a bit of variation in the data
#  which may qualify as noise. 
# The lowess line shows that the data must proceed in log
#  frequency almost two logarithmic units (to 3 then almost to 4)
# Once the line turns upward it becomes almost a straight line
#  The curve itself is known as a scatterplot smoother because
#  it smooths away a great deal of the turbulence in the data.
# lowess uses bins in a manner similar to the way the histogram
#  uses bins. The default binning for lowess works reasonably well
#  but can be adjusted. 
# f is the smoother span	
#  This gives the proportion of points in the plot which
#  influence the smooth at each value. Larger values give
#  more smoothness.

plot(ratings$Frequency, ratings$FamilySize, pch = 16)
lines(lowess(ratings$Frequency, ratings$FamilySize, f = 1/8),
      col = "green", lwd = 2.5)
lines(lowess(ratings$Frequency, ratings$FamilySize, f = 1/4),
      col = "blue", lwd = 2.5)
lines(lowess(ratings$Frequency, ratings$FamilySize, f = 3/4),
      col = "red", lwd = 2.5)
lines(lowess(ratings$Frequency, ratings$FamilySize, f = 7/8),
      col = "darkorange", lwd = 2.5)
lines(lowess(ratings$Frequency, ratings$FamilySize, f = 3),
      col = "purple", lwd = 2.5)

# plot using the words themselves
windows()
# first draw the plot without the data points (type = "n")
plot(ratings$Frequency, ratings$FamilySize, type = "n",
     xlab = "Frequency", ylab = "Family Size")
text(ratings$Frequency, ratings$FamilySize,
     as.character(ratings$Word), cex = 0.7)

# Sometimes the best approach is to make a single
#  multipanel plot with all the relevant variables
# we can use the pairs plots, which mirror the 
#  results around the main diagonal. The diagonal
#  houses the labels, which apply in both directions
pairs( ratings[ , -c(1, 6:8, 10:14)])

# the condition on the columns removes the numbers
#  specified (the minus sign removes them). The excluded
#  columns in this case are the factors, which cannot
#  be plotted in the same way numerics are.
#
# the strong correlations shown indicate collinearity.
#  That makes it difficult or impossible to determine
#  which predictors best explain the response variable
#
# A trellis is a wooden or metal grid for growing
#  roses or other flowers that need vertical support
# Trellis graphs are charts that data are organized
#  as many graphs simultaneously. The pairwise graph
#  shown above is one of them. The lattice package has
#  more instances of them
if(!require(lattice)) install.packages("lattice")
library(lattice)

# lattice is helpful when dealing with different groups
#  of data points. E.g., the ratings data contains words
#  from plants or animals. The factor "Class" (with levels
#  "animal" and "plant") can be viewed as a grouping factor.
#  Another grouping factor could be whether the word is
#  morphologically complex (e.g., woodpecker) or simple
#  (dog)
# 
# The difference between native and non-native English
#  speaking people will differ in the time for a lexical
#  decision. Error rates may differ as well. To explore
#  the results we can use the factor NativeLanguage to
#  group the data.
# It is possible, using lattice, to make a grouped boxplot
bwplot(RT ~ Correct | NativeLanguage, data = lexdec)

# The formula, 
#  RT ~ Correct | NativeLanguage
# considers RT as depending on the correctness of the
#  response (Correct) conditions on the levels
#  of NativeLanguage
# Unsurprisingly, it takes longer for non-native speakers
#  to make a lexical decision. But surprisingly, the
#  incorrect answers for native speakers is from faster
#  responses than for non-native speakers
#
# Consider the words from ratings and their weighted ratings
# Subjective estimates of the weight of the referents of
#   81 English nouns
# Description
#    Subjective estimates on a seven-point scale of the
#    weight of the referents of 81 English nouns.
#
weightRatings[1:5, ]
#
# 
windows()
xylowess.fnc(Rating ~ Frequency | Subject, data = weightRatings,
             xlab = "log Frequency", ylab = "Weight Rating")

# we can get the same result without the smoothed lines by using:
xyplot(Rating ~ Frequency | Subject, data = weightRatings,
             xlab = "log Frequency", ylab = "Weight Rating")
# xyplot is part of the lattice package, xylowess.fnc is
#  from languageR and is a Trellis scatterplot with smoothers

# A second important trellis graph is the conditioning plot
#  An example is from the english data set (4568 rows) which
#  contains mean reaction times to 2284 words for two
#  subject populations. To avoid duplication, use just
#  the young population
youngs <- english[english$AgeSubject == "young", ]
nrow(youngs) # just the young subjects and not the old

# the data frame has 
#  WrittenFrequency
#    numeric vector with log frequency in the CELEX lexical database.
#  FamilySize
#    numeric vector with log morphological family size.
#  NumberComplexSynsets
#    numeric vector with the log-transformed count of synonym sets
#    in WordNet in which the word is listed as part of a compound.
#
# A conditioning plot can help
#   but using WrittenFrequency would produce one panel
#   for each frequency. Instead, use the function equal.count()
#   to use shingles
#  A shingle is a data structure used in Trellis, 
#   and is a generalization of factors to ‘continuous’ variables.
#   It consists of a numeric vector along with some possibly
#   overlapping intervals. These intervals are the ‘levels’
#   of the shingle. The levels and nlevels functions,
#   usually applicable to factors, also work on shingles
#
xylowess.fnc(FamilySize ~ NumberComplexSynsets | 
               equal.count(WrittenFrequency), data = youngs)


