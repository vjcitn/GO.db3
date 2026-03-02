# Function to find all ancestors for each node in a DAG
# Input: data.frame with columns 'id' and 'parent'
#        Multiple rows with same 'id' indicate multiple parents
# Output: data.frame with columns 'id' and 'ancestor'
#         Root nodes (with no parents) will have ancestor = NA

find_all_ancestors <- function(dat) {
  # Handle empty input
  if (nrow(dat) == 0) {
    return(data.frame(id = character(0), ancestor = character(0), stringsAsFactors = FALSE))
  }
  
  # Ensure character columns
  dat$id <- as.character(dat$id)
  dat$parent <- as.character(dat$parent)
  
  # Build lookup: child -> parents (as character vector)
  parent_map <- split(dat$parent, dat$id)
  parent_map <- lapply(parent_map, as.character)
  
  # Memoization cache
  cache <- new.env(hash = TRUE)
  
  ancestors_of <- function(x) {
    x <- as.character(x)
    if (!is.null(cache[[x]])) return(cache[[x]])
    
    parents <- parent_map[[x]]
    # Filter out NA parents
    parents <- parents[!is.na(parents)]
    
    if (is.null(parents) || length(parents) == 0) {
      result <- character(0)
    } else {
      result <- unique(c(parents, unlist(lapply(parents, ancestors_of))))
    }
    
    cache[[x]] <- result
    result
  }
  
  # Only process nodes that appear in the id column
  all_nodes <- unique(dat$id)
  
  # Build result including NA for roots
  result_list <- lapply(all_nodes, function(x) {
    ancs <- ancestors_of(x)
    if (length(ancs) == 0) {
      data.frame(id = x, ancestor = NA_character_, stringsAsFactors = FALSE)
    } else {
      data.frame(id = x, ancestor = ancs, stringsAsFactors = FALSE)
    }
  })
  
  do.call(rbind, result_list)
}

# Example usage:
# Create sample DAG where D has two parents (B and C)
# example_df <- data.frame(
#   id = c("A", "B", "C", "D", "D"),
#   parent = c(NA, "A", "A", "B", "C"),
#   stringsAsFactors = FALSE
# )
# 
# Structure:
#     A
#    / \
#   B   C
#    \ /
#     D
#
# Find all ancestors
# result <- find_all_ancestors(example_df)
# print(result)
#
# Expected output (with NA for root):
#   id ancestor
# 1  A     <NA>
# 2  B        A
# 3  C        A
# 4  D        B
# 5  D        C
# 6  D        A

# To use with your data:
# your_result <- find_all_ancestors(your_data)
