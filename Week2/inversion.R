library(tidyverse)

f <- file("IntegerArray.txt")
arr <- as.numeric(readLines(f) )
close(f)

# Does merge sort + count inversions
merge_sort <- function(arr) {
  if (length(arr) == 1)
    return(list(arr = arr, inv = 0))
  mid <- length(arr) %/% 2
  l <- merge_sort(arr[1:mid])
  r <- merge_sort(arr[-(1:mid)])
  
  i <- j <- 1
  inv <- l$inv + r$inv
  comb <- NULL
  for (i in 1:length(l$arr)) {
    while(r$arr[j] < l$arr[i] && j <= length(r$arr)) {
      inv <- inv + (length(l$arr) - i) + 1
      comb <- c(comb, r$arr[j])
      j <- j + 1
    }
    comb <- c(comb, l$arr[i])
  }
  if (j <= length(r$arr)) comb <- c(comb, r$arr[j:length(r$arr)])
  
  list(arr = comb, inv = inv)
}

# Test cases
system.time(merge_sort(arr))

merge_sort(c(4,6,1,2,8, 3))   # Ans: 7
merge_sort(c(1,3,5,2,4,6))    # Ans: 3
merge_sort(c(1,5,3,2,4))      # Ans: 4

system.time(sort(arr))
