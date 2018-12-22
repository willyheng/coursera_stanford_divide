library(tidyverse)

f <- file("input.txt")
arr <- as.numeric(readLines(f) )
close(f)

quicksort <- function(arr, method = c("first", "median", "last")) {
  temp_arr <<- arr
  calls <- q_recurse(1, length(arr), method)
  x <- list(sorted = temp_arr, calls = calls)
  rm(temp_arr, pos = ".GlobalEnv")
  x
}

choosePivot <- function(l, r, method) {
  if (method == "first") {
    l
  } else if (method == "last") {
    r
  } else if (method == "median") {
    df <- data.frame(index = c(l, (l+r) %/% 2, r))
    df$value <- temp_arr[df$index]
    df$index[min(which(df$value == median(df$value)))]
  } else {
    stop("Invalid method")
  }
}

q_recurse <- function(l, r, method) {
  if (r <= l) {
    return(0)
  }
  pivot <- choosePivot(l, r, method)
  
  # Swap pivot to first position
  pivot_value <- temp_arr[pivot]
  temp_arr[pivot] <<- temp_arr[l]
  temp_arr[l] <<- pivot_value
  
  i <- l + 1
  for (j in (l+1):r) {
    if (temp_arr[j] < pivot_value) {
      temp_swap <- temp_arr[i]
      temp_arr[i] <<- temp_arr[j]
      temp_arr[j] <<- temp_swap
      i <- i + 1
    }
  }
  
  temp_swap <- temp_arr[i-1]
  temp_arr[i - 1] <<- temp_arr[l]
  temp_arr[l] <<- temp_swap
  
  calls <- q_recurse(l, i-2, method)
  calls <- calls + q_recurse(i, r, method)
  
  return(calls + r - l)
}

input <- arr
quicksort(input, method = "first")
quicksort(input, method = "last")
quicksort(input, method = "median")

