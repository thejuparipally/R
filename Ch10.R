# TAWR Ch. 10
#


# 10.1 Getting Organized
#  create a 'results' directory if you do not have one
#
# 10.2 Separating Functions for Reuse
#  set up a separate file called corpus_functions.R
#  
# remove all the objects from memory
rm(list = ls())

setwd("/Users/theju/Desktop/R")
source("code/corpus_functions.R")

input_dir  <- "data/text"
output_dir <- "data/results"

# The objective is an interactive Keyword in Context (KWIC)
#  function that allows the user to enter different
#  file paths and keywords, then return the hits for
#  those terms along with context on either side of
#  the key term
#
# 10.3 User Interaction
#  R has built-in functions that require user input.
#  One is file.choose() provides the user with the
#  the ability to locate a file using the file system's
#  capabilities.
mytext <- scan(file.choose(), what = "character", sep = "\n")
mytext[1:30]

# 10.4 readline
#  There are other R functions that allow users to enter
#  input. One of them is readline, which prints information
#  to the R console and then accept input.
#  Enter the following expression into the console
#  and hit return
myyear <- readline("What year was Moby Dick published? \n")

# 10.5 Building a Better KWIC Function
#  create a function called doitKwic and call it using
#  the following form:
#  doitKwic(directory_path)
#


#  The directory_path is the only necessary argument.
#  Place the function in corpus_functions.R


# 10.6 Fixing Some Problems
#  The text is all lowercase, but anything can be entered
#  for processing. However, given the case folding to
#  lowercase, it is not possible for the current
#  R code to find uppercase-specific keywords.
#
#  The code can also encounter problems if a keyword
#  is too close to either the end or the start of
#  the text. 
#
doitKwic(input_dir)

# 10.7 Practice
#  see corpus functions
#
#  For example, using 'dog' from Melville's Moby Dick:
#    position                      left keyword                   right
# 1     14555       like a newfoundland     dog           just from the
# 2     16376               that in the     dog           days will mow
# 3     27192           swimming like a     dog       throwing his long
# 4     51031              at last down     dog     and kennel starting
# 5     51107               be called a     dog             sir then be
# 6     51565                 call me a     dog        blazes he called
# 7     73930          the sacred white     dog              was by far
# 8    107614                lives in a     dog              or a horse
# 9    107700           kindness of the     dog      the accursed shark
# 10   137047 ungracious and ungrateful     dog       cried starbuck he
# 11   137077       give way greyhounds     dog                 to it i
# 12   147004              whale that a     dog             does to the
# 13   167296         the ram lecherous     dog            he begets us
# 14   170197         bunger bunger you     dog           laugh out why
# 15   170577             in pickle you     dog           you should be
# 16   171104                and like a     dog strangely snuffing this
# 17   202940            feet high hang     dog       look and cowardly
# 18   206897          sagacious ship s     dog         will in drawing
# 19   206949              and then the     dog           vane and then
#
source("code/corpus_functions.R")

# 10.7 Practice
# 1. see corpus_functions.R
kwic_results <- doItKwicBetter(input_dir)
print(kwic_results)


# 10.7 Practice
# 2. see corpus_functions.R
doItKwicStillBetter(input_dir, output_dir)

# 10.7 Practice
# 3. see corpus_functions.R
doItKwicBest(input_dir, output_dir, "[^A-Za-z0-9']", TRUE)
doItKwicBest(input_dir, output_dir, "[^A-Za-z0-9']", FALSE)
doItKwicBest(input_dir, output_dir, "\\W", FALSE)

