# TAWR Ch. 9
#
# remove all the objects from memory
rm(list = ls())

setwd("/Users/theju/Desktop/R")

# 9.1 Introduction
#  define a variable that contains the relative
#  path to the directory of text files
input_dir <- "text"

# we will want the dir command to return the files
#  in the directory that have the .txt suffix
#  (file extension)
files_v <- dir(input_dir, "\\.txt$")
files_v

files_v <- dir(input_dir, "\\.txt$", full.names = TRUE)
files_v


# 9.2 Custom Functions
#
# begin with an example of what happens when
#  the length function encounters a zero-length vector
x <- vector(mode = "numeric", length = 0)
x
class(x)  # yes, it is numeric
length(x) # and its length is zero

# using the length of a zero-length vector in a sequence
1:length(x)

# place the results in a variable and test them
y <- length(x)
length(y) # !!! it is a length of one!!!

# if seq_along is used we have
y <- seq_along(x)
seq_along(y) 


# here the word print means print to the console
#  (i.e., to the screen)
for(i in seq_along(x)){ # does nothing
  print(i)
}

for(i in length(x)){ # prints a zero
  print(i)
}

# create a custom function that will
#  display the files stored in the director of a path
#  the definition will include a default parameter
#  (argument) that will be used if no parameter
#  is passed to the function (pattern is that parameter)
#
# Function purpose: print a vector of file paths and
#  names preceded by the index value of each file name 
show_files <- function(directory_path, pattern = "\\.txt$"){
  file_names_v <- dir(directory_path, 
                      pattern, 
                      full.names = TRUE)
  
  # use seq_along instead of length because seq_along will
  #  handle zero-length vectors without failing.
  for(i in seq_along(file_names_v)){
    # use the catenate (the Latin for 'chain') function
    cat(i, file_names_v[i], "\n", sep = " ")
  }
  
}


show_files(input_dir) # show files with seq_along


# Therefore, we use length and avoid the problem case
#  with an explicit test of length
show_files_len <- function(directory_path, pattern = "\\.txt$"){
  file_names_v <- dir(directory_path, 
                      pattern, 
                      full.names = TRUE)
  

  # use seq_along instead of length because seq_along will
  #  handle zero-length vectors without failing.
  for(i in seq_along(file_names_v)){
    # use the catenate (the Latin for 'chain') function
    cat(i, file_names_v[i], "\n", sep = " ")
  }
}
show_files_len("") # error case: empty directory 

show_files_len(input_dir) # show files with length

# 9.3 A Tokenization Function
#
# Function returns an ordered vector of words (tokens)
#  from the file referenced in the file_path argument
#  Reads in the text file using file_path, pattern
#   then converts the text into a tokenized vector
#   of words.
#  The pattern argument is the tokenization patter
#  returns a vector of word tokens
#
make_token_v <- function(file_path, pattern = "\\W") {
  # read the file in (note that we need the file id
  #  and the input directory)
  text_v <- scan(file_path, what = "character", sep = "\n")
  # convert to a single string
  text_v <- paste(text_v, collapse = " ")
  # fold case so that all characters are lowercase
  text_lower_v <- tolower(text_v)
  # split text using the regular expression from
  #  'pattern' argument
  text_words_v <- strsplit(text_lower_v, pattern)
  # unlist the result
  text_words_v <- unlist(text_words_v)
  # remove the blanks
  text_words_v <- text_words_v[which(text_words_v != "")]
  return(text_words_v) # return the word vector
}

austen_word_v <- make_token_v("text/austen.txt")
head(austen_word_v)

# 9.4 Finding Keywords and Their Contextual Neighbors
# find all the instances of the word 'anguish'
#  in Sense and Sensibility
positions_v <- which(austen_word_v == 'anguish')
positions_v # 56584 108040
length(positions_v) # 2

# identify the first instance of 'anguish'
first_instance <- positions_v[1]
austen_word_v[first_instance]
first_instance

austen_word_v[(first_instance-1):(first_instance+1)]
austen_word_v[(first_instance-2):(first_instance+3)] 

# pretty printing
cat(austen_word_v[(first_instance-2):(first_instance+3)])

# 9.5 Practice
# 1. Using the functions described in this chapter
#    and what you know about vector indexing,
#    write a five-word KWIC list for all the
#    occurrences of the word dog in both Moby Dick
#    as well as Sense and Sensibility, separately
#
# Already have SS read in
positions_v <- which(austen_word_v == 'dog')
length(positions_v)
first_instance <- positions_v[1]
austen_word_v[(first_instance - 5):(first_instance + 6)]
cat(austen_word_v[(first_instance - 5):(first_instance + 6)])

cat(austen_word_v[(first_instance - 5):(first_instance -1)]," [",
    austen_word_v[first_instance], "] ",
    austen_word_v[(first_instance + 1):(first_instance + 5)])


melville_word_v <- make_token_v("text/melville.txt")
head(melville_word_v, 8)
positions_v <- which(melville_word_v == 'dog')
length(positions_v)


for(i in 1:length(positions_v)){
  if(i == 1){
    cat("'dog' in Moby Dick\n")
  }
  cat("------------------------ ", i, " ------------------------", "\n")
  current <- positions_v[i]
  cat(melville_word_v[(current - 5):(current -1)]," [",
      melville_word_v[current], "] ",
      melville_word_v[(current + 1):(current + 5)], "\n")
  if(i == length(positions_v)){
    cat("\n'dog' in Sense and Sensibility\n", 
        "------------------------ ", 
        1,
        " ------------------------", "\n")
    cat(austen_word_v[(first_instance - 5):(first_instance -1)]," [",
        austen_word_v[first_instance], "] ",
        austen_word_v[(first_instance + 1):(first_instance + 5)], "\n")
  }
}

