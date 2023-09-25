# corpus_functions.R
#
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

# A Tokenization Function
#
# Function returns an ordered vector of words (tokens)
#  from the file referenced in the file_path argument
#  Reads in the text file using file_path, pattern
#   then converts the text into a tokenized vector
#   of words.
#  The pattern argument is the tokenization patter
#  returns a vector of word tokens
#
make_token_v <- function(file_path, 
                         pattern = "\\W",
                         make_lower = TRUE) {
  # read the file in (note that we need the file id
  #  and the input directory)
  text_v <- scan(file_path, what = "character", sep = "\n")
  # convert to a single string
  text_v <- paste(text_v, collapse = " ")
  # fold case so that all characters are lowercase
  print(paste("make lower: ", make_lower))
  if(make_lower == TRUE) {
    text_case_v <- tolower(text_v)
  } else {
    text_case_v <- text_v
  }
  
  # split text using the regular expression from
  #  'pattern' argument
  text_words_v <- strsplit(text_case_v, pattern)
  # unlist the result
  text_words_v <- unlist(text_words_v)
  # remove the blanks
  text_words_v <- text_words_v[which(text_words_v != "")]
  return(text_words_v) # return the word vector
}

# Function to search for a term within the given
#  directory.
#  The user will be asked to enter a file name. The file
#  specified will be searched for a keyword to find and
#  a "context" number for content on either side of the
#  keyword

doitKwic <- function(directory_path){
  show_files(directory_path) # what txt files are present
  # allow the user to choose the file using
  #  its index number
  cat("Enter the file's index number\n")
  file_id <- as.numeric(readline(show_files(directory_path)))
  keyword <- tolower(readline("Enter a Keyword: "))
  context <- as.numeric(readline("How many context words? "))
  word_v  <- make_token_v(dir(directory_path, full.names = TRUE)[file_id])
  hits_v  <- which(word_v == keyword)
  for(i in seq_along(hits_v)){
    start   <- hits_v[i] - context
    if(start < 1){start <- 1}
    end     <- hits_v[i] + context
    if(end > length(word_v)){end <- length(word_v)}
    before  <- word_v[start:(start + context - 1)]
    after   <- word_v[(start + context + 1):end ]
    keyword <- word_v[start + context]
    cat("------------------------",
        i,
        "------------------------\n")
    cat(before, "[", keyword, "]", after, "\n")
  }
}

# (10.7 Practice)
# 1. doItKwicBetter()
#  In prior exercises and lessons you have learned how to:
#
#  (1) instantiate an empty object outside of a for loop and
#      then how to add new data to that object during the loop. 
#  (2) use cbind() to add columns of data
#  (3) use rbind() to add rows
#  (4) use paste() with the collapse argument to glue together
#      pieces in a vector of values 
#  (5) use cat() to concatenate items in a vector
#  (6) use colnames() to get and set the names of columns
#      in a data frame
#
#  Using all of this knowledge, modify the the function
#  written in this chapter (doitKwic()) so that the results
#  of a KWIC search are put into a data frame object in
#  which each row is a single KWIC result. 
#
#  Name this new function doItKwicBetter()
#   The data frame returned should have four columns:
#   1. position - index of keyword location in file
#   2. left context - words found to the keyword's left
#   3. keyword - word sought
#   4. right context - words found to the keyword's right
# E.g.
#
doItKwicBetter <- function(directory_path, 
                           pattern = "\\W",
                           make_lower = TRUE){
  show_files(directory_path) # what txt files are present
  # allow the user to choose the file using
  #  its index number
  cat("Enter the file's index number\n")
  file_id <- as.numeric(readline(show_files(directory_path)))
  keyword <- readline("Enter a Keyword: ")
  context <- as.numeric(readline("How many context words? "))
  word_v  <- make_token_v(dir(directory_path, full.names = TRUE)[file_id],
                          pattern, make_lower)
  hits_v  <- which(word_v == keyword)
  # create a data frame to hold the results
  d_frame <- data.frame(matrix(nrow = length(hits_v), ncol = 4)) 
  # put the names in the columns
  colnames(d_frame) <- c("position", "left", "keyword", "right")
  for(i in seq_along(hits_v)){
    position <- hits_v[i]
    start <- position - context
    if(start < 1){
      start <- 1
    }
    end <- position + context
    if(end > length(word_v)){
      end <- length(word_v)
    }
    before      <- start:(start + context - 1)
    left        <- paste(word_v[before], collapse = " ")
    keyword     <- word_v[position]
    after       <- (start + context + 1):end
    right       <- paste(word_v[after], collapse = " ")
    d_frame[i,] <- c(position, left, keyword, right)
  }
  return(d_frame)
}


# (10.7 Practice)
# 2. Copy the function you created in the exercise above and
#    modify it to include a feedback loop asking if the
#    results should be saved as a .csv file. If the user
#    answers 'y' for 'yes,' generate a file name based in 
#    the existing user input (keyword, file name, context)
#    and write that file to the results directory using
#    the write.csv() function, as in this example below.
#    save this new function in your corpus_functions.R
#    file as doItKwicStillBetter()
#
doItKwicStillBetter<- function(input_directory_path,
                               output_directory_path = "results",
                               pattern = "\\W",
                               make_lower = TRUE){
  df <- doItKwicBetter(input_directory_path,
                       pattern,
                       make_lower)
  print(df)
  answer <- readline("Do you want to save the results to a file? (y/n)\n")
  if(answer == 'y' || answer == 'Y'){
    # create a file name
    fname <- paste(output_directory_path,
                   "/", 
                   df[1,"keyword"], "_",
                   df[1,"right"],
                   ".csv",
                   sep = "")
    print(paste("file name: ", fname))
    write.csv(df, file = fname )
  }
  return(df)
}

#
# (10.7 Practice)
# 3. Neither of these "better" KWIC functions give the user
#    any options for tokenizing the texts. Right now both
#    rely on the default behavior to the make_token_v() 
#    function, which uses the regular expression "\\W"
#    To give users flexibility to change the way files
#    get tokenized, we need to alter the line of code
#    that calls make_token_v() to include a pattern
#    argument, and we also need to add a new argument
#    to the parameters of our KWIC function. Rewrite your
#    doItKwicStillBetter() to achieve this objective and
#    and save it as doItKwicBest(). After you save the
#    function, you should be able to call it using the
#    code shown below. Recall that
#    [^A-Za-z9-0']
#    is a regular expression that retains apostrophes
#    and possessives. If your function is correct, you 
#    will be able to search for instances of ahab's
#    After you have coded this new version, check your
#    solution with the solution at the back of the book
#    where you will find one more useful iteration
#    of this function explained. Once you have
#    finished the practice exercises for this chapter
#    save doItKwic(), doItKwicBetter(), doItKwicStillBetter(),
#    and doItKwicBest() to your corpus_functions.R
#    file so that you can easily access them in
#    the future.
#
doItKwicBest <- function(input_directory_path,
                         output_directory_path = "results",
                         pattern = "[^A-Za-z0-9']",
                         make_lower = TRUE){
  df <- doItKwicStillBetter(input_directory_path,
                            output_directory_path,
                            pattern, make_lower)
  return(df)
}
