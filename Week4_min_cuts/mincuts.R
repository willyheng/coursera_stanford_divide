library(tidyverse)

f <- file("karger.txt")
arr <- readLines(f)
close(f)

# Build edges from input file
edges <- map(arr, 
             function(x) {
               l <- str_split(x, "[\\t ]")[[1]]
               v <- as.numeric(l[1])
               links <- as.numeric(l[2:length(l)])
               links <- links[links > v]
               if (length(links) > 0) data.frame(v2 = links)
               else NULL
             }) %>%
  bind_rows(.id = "v1") %>% 
  mutate_all(as.numeric) %>%
  filter(!is.na(v2)) 

rem_vertices <- 1:length(arr)

# Recursive find mincut
find_mincut <- function(edges, rem_vertices, seed = 100) {
  if (length(rem_vertices) == 2) {
    return (nrow(edges))
  }
  set.seed(seed)
  
  # Select edge
  remove_edge_row <- sample(1:nrow(edges), 1)
  to_keep <- edges[remove_edge_row, 1]  # Edge to keep is always smaller based on construction
  to_remove <- edges[remove_edge_row, 2]
  
  # Remove chosen edge
  edges <- rbind(edges[-remove_edge_row, ])
  
  # Set all removed node to smaller node in both v1 and v2
  edges <- mutate(edges, 
                  v1 = ifelse(v1 == to_remove, to_keep, v1), 
                  v2 = ifelse(v2 == to_remove, to_keep, v2)) %>%
    filter(! v1 == v2)  # Remove nodes that are looping
  
  # Remove vertex from remaining list
  rem_vertices <- rem_vertices[rem_vertices != to_remove]
  return(find_mincut(edges, rem_vertices, seed = seed + 1))
}

# Run find_mincut 100 times
mincuts <- map(1:100, ~find_mincut(edges, rem_vertices, .)) %>% 
  map(~data.frame(mincuts = .)) %>% 
  bind_rows(.id = "seed")

# Extract mincut
mincuts %>% top_n(n = 1, wt = desc(mincuts))
  
