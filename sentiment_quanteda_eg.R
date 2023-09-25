# Sentiment Analysis on Financial Data
#
# https://cran.rstudio.com/bin/windows/Rtools/rtools42/rtools.html
#
if(!require(devtools)) install.packages("devtools")
library(devtools)

if(!require(quanteda)) install.packages("quanteda")
library(quanteda)

setwd("/Users/theju/Desktop/R")
corp <-  readRDS("data_corpus_earning-calls.rds")

# Construct a tokens object, either by importing a
#  named list of characters from an external
#  tokenizer, or by calling the internal
#  quanteda tokenizer.
toks <- tokens(corp, 
               remove_punct = TRUE, 
               remove_numbers = TRUE, 
               remove_symbol = TRUE)

# tokens_remove
# These functions select or discard tokens from a
#  tokens object. For convenience, the functions
#  tokens_remove and tokens_keep are defined as
#  shortcuts for
#    tokens_select(x, pattern, selection = "remove")
#  and
#    tokens_select(x, pattern, selection = "keep")
#  respectively. The most common usage for
#  tokens_remove will be to eliminate stop words
#  from a text or text-based object, while the most
#  common use of tokens_select will be to select tokens
#  with only positive pattern matches from a list of
#  regular expressions, including a dictionary. 
#  startpos and endpos determine the positions of
#  tokens searched for pattern and areas affected
#  are expanded by window.
toks <- tokens_remove(toks, 
                      pattern = stopwords("en"), 
                      min_nchar = 2)

# Construct a sparse document-feature matrix,
#  from a character, corpus, tokens, or even other dfm object.
dfmt <- dfm(toks)

# Finance sentiment analysis is a task we can perform by using
#  the financial sentiment dictionary created by
#  Loughran and Mcdonald. The L&M dictionary
#  is available as part of the quanteda.sentiment package.
#  The dictionary contains 4,232 words under categories
#  such as “negative”, “positive”, "litigious",
#  "constraining", "superfluous", or "uncertainty".

if(!require(quanteda.sentiment)){
  devtools::install_github("quanteda/quanteda.sentiment")
} 
  

require(quanteda.sentiment)
data_dictionary_LoughranMcDonald[c(1, 3)]

# We can count the frequencies of the negative and
#  uncertainty words using dfm_lookup() 
#  Note that the dictionary contains no multi-word expressions.
#  The frequencies of the sentiment words are normalized by
#  dividing them by the total number of words.
#  This reduces the effect that the longer document lengths
#  have more opportunties to qualify for a category
dfmt_dict <- dfm_lookup(dfmt, 
      data_dictionary_LoughranMcDonald[c(1, 3)]) / ntoken(dfmt)

dat_dict <- cbind(convert(dfmt_dict, to = "data.frame"),
                  docvars(dfmt_dict))

# Using dictionary analysis results, we can see how
#  sentiment in earning calls changed in 2017.
#  Managers discuss financial results from the previous periods
#  in events called "earnings calls" to describe their perspective
#  on how their companies business is going. 
#  Here, earnings calls held between January to March are
#  about the previous year’s final quarter's results
#  (“NEGATIVE.0″ and ” UNCERTAINTY.0″) 
#  Those from October to December are about the third quarter’s
#  results
#  (“NEGATIVE.3″ and ” UNCERTAINTY.3″ ). 

# All earnings calls are located in the two-dimensional space
#  formed by negativity and uncertainty. For visualization, 
#  we examine five companies whose changes in sentiment 
#  are the greatest as measured by Euclidean distance.

# Reshape Grouped Data
#  This function reshapes a data frame between
#  ‘wide’ format (with repeated measurements 
#  in separate columns of the same row)
#  and ‘long’ format (with the repeated measurements in
#  separate rows)
dat_wide <- reshape(dat_dict[-5],        # data
                    timevar = "quarter", # default is 'time' here qtrly
                    idvar = "company",   # dict id for 1 group (or individual)
                    direction = "wide")  # wide or long

# warning that multiple rows match quarter=1 and the first is taken

# square the differences between the quarters
dat_wide$change <- sqrt((dat_wide$NEGATIVE.0 - dat_wide$NEGATIVE.3) ^ 2 +
                          (dat_wide$UNCERTAINTY.0 - dat_wide$UNCERTAINTY.3) ^ 2)

# pull the top 5
dat_top <- head(dat_wide[order(dat_wide$change, decreasing = TRUE),], 5)
dat_top

par(mar = c(4, 4, 1, 1))
plot(dat_top$NEGATIVE.0, dat_top$UNCERTAINTY.0, ylim = c(0, 0.06), xlim = c(0, 0.06), 
     col = 1:5, pch = 1:5, xlab = "Negativity", ylab = "Uncertainty")
arrows(dat_top$NEGATIVE.0, dat_top$UNCERTAINTY.0, 
       dat_top$NEGATIVE.3, dat_top$UNCERTAINTY.3,
       code = 2, length = 0.1, col = 1:5)
legend("topright", col = 1:5, legend = dat_top$company, lty = 1, pch = 1:5)


# The plot shows that uncertainty decreased dramatically
#  for Dish Network but increased for Tractor Supply
#  and Lockheed Martin; negativity increased but
#  uncertainty remained about the same for Foot Locker; 
#  both uncertainty and negativity decreased 
#  for Caterpillar.



