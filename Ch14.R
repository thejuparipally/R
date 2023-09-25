# Ch. 14
# Sentiment Analysis
#
# 14.2 Loading syuzhet
#
rm(list = ls())
#setwd("/Users/theju/Desktop/R")
setwd("/Users/theju/Desktop/R")
# syuzhet shee you (dz)zoat
#  The word syuzhet comes from the Russian formalist 
#  Vladimir Propp who divided the narrative into 
#  the fabula and the syuzhet. The fabula are the specific
#  elements of the plot and the syuzhet is the manner
#  in which those elements are organized into the linear
#  movement of the narrative. 
if(!require(syuzhet)) install.packages("syuzhet")
library(syuzhet)

dir()

# 14.3 Loading a Text
#
# The moby v and sense v objects contain the
#  entire text of each novel as a single 
#  string. No tokenization or other text
#  manipulation has been done.
moby_v <- get_text_as_string(path_to_file = "text/melville.txt")
class(moby_v)
moby_v[1,45]

sense_v <- get_text_as_string(path_to_file = "text/austen.txt")

# The get sentiment function will provide the overall
#  sentiment 
get_sentiment(moby_v)  # more negative than positive language
get_sentiment(sense_v) # less negative than moby dick

# 14.4 Getting Sentiment Values
#
# syuzhet supports tokenization by word boundaries
#  or by sentence boundaries. If tokenized by sentences
#  the sum of the sentiments of each word. Sentences
#  are the fundamental unit of composition, so using 
#  them makes sense. The use of sentences provides
#  us with a progression of sentiment through the
#  novels. 
moby_sentences_v  <- get_sentences(moby_v)
sense_sentences_v <- get_sentences(sense_v)

# get sentences works differently than strsplit()
#  which we used to parse texts earlier. strsplit()
#  uses regular expression (e.g., "\\W" or [^A-Za-z0-9'])
#  and splits text into words instead of sentences
#  
moby_sentences_v[1:3]

# note that metadata is harder to parse than the text
#  itself
sense_sentences_v[1:3]

# moving down further in the text shows the difference
sense_sentences_v[5:8]

# 14.5 Accessing Sentiment
#
# using get sentiment to analyze the text. This requires
#  a character vector and a method that determines
#  which dictionary to use. The default dictionary is
#  named "syuzhet" but other possible methods are 
#  "bing" "afinn" "nrc" and "stanford"
#
moby_sentiments_v  <- get_sentiment(moby_sentences_v)
sense_sentiments_v <- get_sentiment(sense_sentences_v)

str(moby_sentiments_v)
# each value corresponds to sentence

# positive values indicate positive sentiment, negative
#  values indicate negative sentiment
head(moby_sentiments_v)
moby_sentences_v[4]
moby_sentences_v[6]

head(moby_sentiments_v,12)

moby_sentences_v[7]
moby_sentences_v[8]

moby_sentences_v[c(7,8)]

paste("means: md", 
      round(mean(moby_sentiments_v),4), 
      "sense", round(mean(sense_sentiments_v),4))
paste("stdevs: md",
      round(sd(moby_sentiments_v),4), 
      "sense", round(sd(sense_sentiments_v),4))


# 14.6 Plotting
#
# use the x-axis for time (narrative time)
#  and the y-axis for the sentiment
#
# first attemp a visualization with the builtin plot
#  function
plot(moby_sentiments_v,
     type = "l",
     xlab = "Novel Time",
     ylab = "Sentiment",
     main = "Moby Dick's Raw Sentiment Values")

# too much information to understand it well
#  that is, it is difficult to separate the 
#  signal from the noise
#
# instead, use the syuzhet's smoothing capability
#  simple_plot can use three types of smoothing
#  1. moving average
#  2. loess
#  3. discrete cosine transformation (DCT)
#
windows()
simple_plot(sense_sentiments_v,
            title = "Sense & Sensibility Simple Plot")

# The top graph includes three lines based on three
#  different ways of smoothing the data. This allows
#  for easy comparison and different levels of detail.
#  The second graph shows only the DCT smoothed line,
#  but does so on a normalized time axis. Notice that
#  the x-axis in the top graph goes from zero to
#  over 4000. Those points refer to sentences in the
#  text. Though the lines have different specifics,
#  they follow the same general trajectory. According
#  to the graph, Sense and Sensibility starts out
#  positive, descends to a low point midway through
#  the novel, and then ascends somewhat to the
#  denouement.

simple_plot(moby_sentiments_v,
            title = "Moby Dick Simple Plot")

# Need to be able to normalize different novel
#  lengths to be able to compare them.
#
if(!require(zoo)) install.packages("zoo")
library(zoo)


#  A simple way to do so is to use percentages
#  instead of raw numbers. Using a rolling mean
#  (moving window) and a rescale function
#  put both novels on the same footing
#
(moby_sentiment_len <- length(moby_sentiments_v))
(moby_window <- round(moby_sentiment_len * 0.1))
moby_rolled <- rollmean(moby_sentiments_v,
                        k = moby_window)
(sense_sentiment_len <- length(sense_sentiments_v))
(sense_window <- round(sense_sentiment_len * 0.1))
sense_rolled <- rollmean(sense_sentiments_v,
                         k = moby_window)

# The rescale-x-2 function re-scales values
#  to a normalized x and y axis. This is useful
#  for comparing two sentiment arcs. 
#
moby_scaled  <- rescale_x_2(moby_rolled)
sense_scaled <- rescale_x_2(sense_rolled)

# rescale-x-2 returns a list of three vectors
#  (x, y, z)
# x is a vector of values from 0 to 1 that
#  is the same length as the input vector v
# y is a vector of values from 0 to 1 that
#  is the same length as the input vector v
# z is a scaled vector of values from -1 to +1
#  that is the same length as the input vector v
# 
plot(moby_scaled$x,
     moby_scaled$z,
     type = "l",
     col  = "blue",
     xlab = "Narrative Time",
     ylab = "Emotinal Valence",
     main = "Moby Dick and Sense-Sensibility - Rolling Means")
lines(sense_scaled$x, sense_scaled$z, col = "red")
legend("topright", 
       legend=c("Moby Dick", "Sense & Sensibility"),
       inset = 0.01, col=c("blue", "red"), lty=1, cex=0.8)

# 14.8 Computing Plot Similarity
#
# Even though we can plot a chart using the 
#  normalization technique, we cannot use
#  that mathematically (e.g., comparing averages
#  is how we got Simpson's Paradox)
#
# The authors approach may or may not address
#  this problem
#
# In addition, moving averages loses information
#  at the beginning and end of the data sequence
#
# use get-dct-transform to smooth the two
#  data sequences. By default, smoothing takes
#  place but also mapping. get-dct-transform
#  returns a 100-unit result. If the scale-range
#  is set to TRUE the most negative value is
#  set to -1 and the most positive value is +1
#  The values in-between are scaled proportionally
moby_dct <- get_dct_transform(
  moby_sentiments_v,
  scale_range = TRUE
)

plot(moby_dct,
     type = "l",
     col = "blue",
     xlab = "Narrative Time",
     ylab = "Emotional Valence",
     main = "Moby Dick with DCT-Smooth and Normalized Time")

# using reverse len to see the entire series
moby_dct <- get_dct_transform(
  moby_sentiments_v,
  x_reverse_len = length(moby_sentiments_v),
  scale_range = TRUE
)

plot(moby_dct,
     type = "l",
     col = "blue",
     xlab = "Narrative Time",
     ylab = "Emotional Valence",
     main = "Moby Dick with DCT-Smooth but without Normalized Time")
# notice that the axis went to the end of the units (> 9k)

normed_moby_shape <- get_dct_transform(
  moby_sentiments_v,
  x_reverse_len = 100,
  scale_range = TRUE
)
normed_sense_shape <- get_dct_transform(
  sense_sentiments_v,
  x_reverse_len = 100,
  scale_range = TRUE
)

plot(normed_moby_shape,
  type = "l",
  col  = "blue",
  xlab = "Narrative Time",
  ylab = "Emotional Valence",
  main = "Two Novels with DCT-Smoothing and Normalized Time"
)
lines(normed_sense_shape, col = "red")
legend("topright", 
       legend=c("Moby Dick", "Sense & Sensibility"),
       inset = 0.01, col=c("blue", "red"), lty=1, cex=0.8)

# mathematical comparison
#
cor(normed_moby_shape, normed_sense_shape)
