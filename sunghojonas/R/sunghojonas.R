#install devtools and roxygen2
library(devtools)
library(roxygen2)
  
#' palindrome of String value.
#'
#' This function takes an parameter representing the string 
#' for which you want to create a palindrome
#' and makes input String value as a palindrome.
#' @param input_string String value.
#' @return a String value , which is palindrome.
#' @examples
#' input <- "hello"
#' palindrome <- create_palindrome(input)
#' print(palindrome) # Output: "helloolleh"
#' @export
create_palindrome <- function(input_string) {
  reversed_string <- paste(rev(strsplit(input_string, "")[[1]]), collapse = "")
  palindrome <- paste(input_string, reversed_string, sep = "")
  return(palindrome)
}

#' swaps the symbols of two input indices in a string.
#'
#' This function swaps the symbols of two input indices in a string and
#' returns the string with the swapped symbols.

#' @param input_string the string in which symbols need to be swapped.
#' @param index1 the first index.
#' @param index2 the second index.
#' @return string with the swapped symbols.
#' @examples 
#' input <- "abcdef"
#' index1 <- 2
#' index2 <- 5
#' swapped_string <- swap_symbols(input, index1, index2)
#' print(swapped_string)  # Output: "aecdbf"
#' @export
swap_symbols <- function(input_string, index1, index2) {
  if (index1 < 1 || index2 < 1 || index1 > nchar(input_string) || index2 > nchar(input_string)) {
    stop("Invalid indices. Please provide valid indices within the string length.")
  }
  
  symbols <- strsplit(input_string, "")[[1]]
  
  temp <- symbols[index1]
  symbols[index1] <- symbols[index2]
  symbols[index2] <- temp
  
  swapped_string <- paste(symbols, collapse = "")
  return(swapped_string)
}

#' counts the number of characters in a string and determines whether it is an even or odd number
#'
#' This function takes an input string and counts the number of characters 
#' in the string. It then determines whether the count is an even or odd number 
#' and returns a descriptive string indicating the count and its parity.


#' @param input_string The string for which you want to count the characters.
#' @return A string indicating the number of characters in the input string and whether it is an even or odd count.
#' @examples 
#' input <- "Hello, World!"
#' result <- count_characters(input)
#' print(result)  # Output: "13 characters (odd)"
#' @export
count_characters <- function(input_string) {
  num_characters <- nchar(input_string)
  is_even <- num_characters %% 2 == 0
  
  if (is_even) {
    return(paste(num_characters, "characters (even)"))
  } else {
    return(paste(num_characters, "characters (odd)"))
  }
}
