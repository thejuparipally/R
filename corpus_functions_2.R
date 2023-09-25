# corpus_functions_2.R
#
#
get_node_text <- function(node, xpath, ns){
  paragraph_nodes <- xml_find_all(node, xpath, ns)
  paragraph_v <- xml_text(paragraph_nodes)
  paste(paragraph_v, collapse = "")
}

tokenize <- function(text_v, 
                     pattern = "[^A-Za-z0-9']", 
                     lower = TRUE){
  if(lower){
    text_v <- tolower(text_v)
  }
  word_v <- unlist(strsplit(text_v, pattern))
  word_v[which(word_v != "")]
}

