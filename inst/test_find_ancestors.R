library(testthat)
library(hedgehog)

# Source the function to test
source("find_ancestors.R")

# ============================================================================
# Custom generators for DAG structures
# ============================================================================

# Generator for a simple linear chain: A <- B <- C <- D
gen.linear_chain <- function(n = 5) {
  generate(for (size in gen.element(2:n)) {
    ids <- as.character(1:size)
    parents <- c(NA, ids[1:(size-1)])
    data.frame(id = ids, parent = parents, stringsAsFactors = FALSE)
  })
}

# Generator for a proper DAG with multiple parents per node
gen.dag_with_multiple_parents <- function() {
  generate(for (n_nodes in gen.element(4:10)) {
    ids <- paste0("n", 1:n_nodes)
    
    # Build edges: each node (except first) can have 1-2 parents
    # from nodes with lower indices
    edges <- list()
    
    # First node is root
    for (i in 2:n_nodes) {
      # Number of parents (1 or 2)
      n_parents <- sample(1:min(2, i-1), 1)
      # Select parents from earlier nodes
      parent_indices <- sample(1:(i-1), n_parents)
      
      for (p_idx in parent_indices) {
        edges[[length(edges) + 1]] <- data.frame(
          id = ids[i],
          parent = ids[p_idx],
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Root node row
    root_row <- data.frame(id = ids[1], parent = NA, stringsAsFactors = FALSE)
    
    # Combine all edges
    do.call(rbind, c(list(root_row), edges))
  })
}

# Generator for a diamond-shaped DAG (classic example)
gen.diamond_dag <- function() {
  generate(for (x in gen.element(1:100)) {  # Just for variability
    data.frame(
      id = c("A", "B", "C", "D", "D"),
      parent = c(NA, "A", "A", "B", "C"),
      stringsAsFactors = FALSE
    )
  })
}

# Generator for a tree structure with one root and multiple branches
gen.tree <- function(max_nodes = 10) {
  generate(for (n_nodes in gen.int(10)) {
    n <- min(n_nodes + 2, max_nodes)
    ids <- as.character(1:n)
    
    # First node is root
    parents <- rep(NA, n)
    
    # Assign parents to other nodes, ensuring they reference earlier nodes
    for (i in 2:n) {
      parents[i] <- ids[sample(1:(i-1), 1)]
    }
    
    data.frame(id = ids, parent = parents, stringsAsFactors = FALSE)
  })
}

# Generator for a DAG with a single root (tree - only one parent per node)
gen.single_root_dag <- function() {
  generate(for (n_nodes in gen.element(3:15)) {
    ids <- paste0("node_", 1:n_nodes)
    parents <- rep(NA, n_nodes)
    
    # First node is the root
    parents[1] <- NA
    
    # Assign parents ensuring DAG property (parent index < child index)
    for (i in 2:n_nodes) {
      parents[i] <- ids[sample(1:(i-1), 1)]
    }
    
    data.frame(id = ids, parent = parents, stringsAsFactors = FALSE)
  })
}

# Generator for multiple disconnected trees
gen.forest <- function() {
  generate(for (n_roots in gen.element(2:4)) {
    for (nodes_per_tree in gen.element(2:5)) {
      n_total <- n_roots * nodes_per_tree
      ids <- paste0("n", 1:n_total)
      parents <- rep(NA, n_total)
      
      # Create separate trees
      for (root_idx in 1:n_roots) {
        tree_start <- (root_idx - 1) * nodes_per_tree + 1
        tree_end <- root_idx * nodes_per_tree
        tree_ids <- ids[tree_start:tree_end]
        
        # First node in each tree is a root
        for (i in 2:nodes_per_tree) {
          idx <- tree_start + i - 1
          parents[idx] <- tree_ids[sample(1:(i-1), 1)]
        }
      }
      
      data.frame(id = ids, parent = parents, stringsAsFactors = FALSE)
    }
  })
}

# ============================================================================
# Property-based tests
# ============================================================================

test_that("All ancestors are reachable via parent links", {
  forall(
    gen.dag_with_multiple_parents(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # Filter out NA ancestors (root nodes)
      result <- result[!is.na(result$ancestor), ]
      
      # For each row in result, verify ancestor is reachable from id
      for (i in seq_len(nrow(result))) {
        id <- result$id[i]
        ancestor <- result$ancestor[i]
        
        # BFS to check if ancestor is reachable from id
        visited <- character(0)
        queue <- id
        found <- FALSE
        
        while (length(queue) > 0 && !found) {
          current <- queue[1]
          queue <- queue[-1]
          
          if (current %in% visited) next
          visited <- c(visited, current)
          
          # Get all parents of current node
          parents <- df$parent[df$id == current & !is.na(df$parent)]
          
          if (ancestor %in% parents) {
            found <- TRUE
            break
          }
          
          queue <- c(queue, parents)
        }
        
        expect_true(found, 
                   label = sprintf("Ancestor %s not reachable from %s", ancestor, id))
      }
      
      # Always make at least one expectation for hedgehog
      expect_true(TRUE, label = "Reachability test completed")
    }
  )
})

test_that("No node is its own ancestor", {
  forall(
    gen.dag_with_multiple_parents(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # Filter out NA ancestors
      result <- result[!is.na(result$ancestor), ]
      
      # Check that id != ancestor for all rows
      if (nrow(result) > 0) {
        expect_true(all(result$id != result$ancestor),
                   label = "Found node that is its own ancestor")
      } else {
        # If no non-NA ancestors, still make an expectation
        expect_true(TRUE, label = "No non-NA ancestors to check")
      }
    }
  )
})

test_that("Root nodes have no ancestors (NA)", {
  forall(
    gen.dag_with_multiple_parents(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # Find all root nodes (those with NA parent or no parent entries)
      all_ids <- unique(df$id)
      ids_with_parents <- unique(df$id[!is.na(df$parent)])
      roots <- setdiff(all_ids, ids_with_parents)
      
      # Roots should have NA as their ancestor
      if (length(roots) > 0) {
        for (root in roots) {
          root_rows <- result[result$id == root, ]
          # Should have exactly one row with NA ancestor
          expect_equal(nrow(root_rows), 1,
                      label = sprintf("Root %s should have exactly one row", root))
          expect_true(is.na(root_rows$ancestor[1]),
                     label = sprintf("Root %s should have NA ancestor", root))
        }
      } else {
        # No roots to check
        expect_true(TRUE, label = "No roots to check")
      }
    }
  )
})

test_that("All ancestors are found (completeness)", {
  forall(
    gen.dag_with_multiple_parents(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # For each node, manually compute expected ancestors using BFS
      unique_ids <- unique(df$id)
      
      for (node_id in unique_ids) {
        expected_ancestors <- character(0)
        visited <- character(0)
        queue <- node_id
        
        while (length(queue) > 0) {
          current <- queue[1]
          queue <- queue[-1]
          
          if (current %in% visited) next
          visited <- c(visited, current)
          
          # Get all parents of current node
          parents <- df$parent[df$id == current & !is.na(df$parent)]
          
          for (parent in parents) {
            if (!(parent %in% expected_ancestors)) {
              expected_ancestors <- c(expected_ancestors, parent)
              queue <- c(queue, parent)
            }
          }
        }
        
        # Get actual ancestors from result (excluding NA)
        actual_ancestors <- result$ancestor[result$id == node_id & !is.na(result$ancestor)]
        
        # Check they match
        expect_true(setequal(actual_ancestors, expected_ancestors),
                   label = sprintf("Mismatch for node %s: expected %s, got %s", 
                                 node_id, 
                                 paste(sort(expected_ancestors), collapse=","),
                                 paste(sort(actual_ancestors), collapse=",")))
      }
      
      # Always make at least one expectation
      expect_true(length(unique_ids) > 0, label = "Completeness test completed")
    }
  )
})

test_that("No duplicate (id, ancestor) pairs", {
  forall(
    gen.single_root_dag(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # Filter out NA ancestors
      result <- result[!is.na(result$ancestor), ]
      
      if (nrow(result) > 0) {
        # Create a unique key
        keys <- paste(result$id, result$ancestor, sep = ":")
        expect_equal(length(keys), length(unique(keys)),
                    label = "Found duplicate (id, ancestor) pairs")
      } else {
        expect_true(TRUE, label = "No non-NA ancestors to check for duplicates")
      }
    }
  )
})

test_that("Ancestors are in parent-to-root order", {
  forall(
    gen.linear_chain(10),
    function(df) {
      result <- find_all_ancestors(df)
      
      # Filter out NA ancestors
      result <- result[!is.na(result$ancestor), ]
      
      # Track if we made any checks
      checked_any <- FALSE
      
      # For nodes in a linear chain, verify order
      for (node_id in df$id) {
        node_ancestors <- result$ancestor[result$id == node_id]
        
        if (length(node_ancestors) > 1) {
          checked_any <- TRUE
          # Each ancestor should be the parent of the previous one
          for (i in 2:length(node_ancestors)) {
            prev_ancestor <- node_ancestors[i - 1]
            curr_ancestor <- node_ancestors[i]
            
            # curr_ancestor should be the parent of prev_ancestor
            prev_parent <- df$parent[df$id == prev_ancestor]
            expect_equal(prev_parent, curr_ancestor,
                        label = sprintf("Order violation: %s should be parent of %s",
                                     curr_ancestor, prev_ancestor))
          }
        }
      }
      
      # Always make at least one expectation for hedgehog
      expect_true(TRUE, label = "Order test completed")
    }
  )
})

test_that("Empty data.frame returns empty result", {
  df <- data.frame(id = character(0), parent = character(0), stringsAsFactors = FALSE)
  result <- find_all_ancestors(df)
  
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("id", "ancestor"))
})

test_that("Single node with no parent returns NA ancestor", {
  df <- data.frame(id = "A", parent = NA, stringsAsFactors = FALSE)
  result <- find_all_ancestors(df)
  
  # Should have one row with NA ancestor
  expect_equal(nrow(result), 1)
  expect_equal(result$id[1], "A")
  expect_true(is.na(result$ancestor[1]))
})

test_that("Concrete test: Diamond DAG with node D having parents B and C", {
  # Create the diamond structure:
  #     A
  #    / \
  #   B   C
  #    \ /
  #     D
  df <- data.frame(
    id = c("A", "B", "C", "D", "D"),
    parent = c(NA, "A", "A", "B", "C"),
    stringsAsFactors = FALSE
  )
  
  result <- find_all_ancestors(df)
  
  # A should have NA ancestor (root node)
  a_ancestors <- result$ancestor[result$id == "A"]
  expect_equal(length(a_ancestors), 1)
  expect_true(is.na(a_ancestors[1]))
  
  # B should have ancestor A
  b_ancestors <- result$ancestor[result$id == "B" & !is.na(result$ancestor)]
  expect_true(setequal(b_ancestors, "A"))
  
  # C should have ancestor A
  c_ancestors <- result$ancestor[result$id == "C" & !is.na(result$ancestor)]
  expect_true(setequal(c_ancestors, "A"))
  
  # D should have ancestors B, C, and A
  d_ancestors <- result$ancestor[result$id == "D" & !is.na(result$ancestor)]
  expect_true(setequal(d_ancestors, c("B", "C", "A")))
  expect_equal(length(d_ancestors), 3)
})

test_that("Transitivity: if A is ancestor of B and B is ancestor of C, then A is ancestor of C", {
  forall(
    gen.dag_with_multiple_parents(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # Filter out NA ancestors
      result <- result[!is.na(result$ancestor), ]
      
      # Track if we checked anything
      checked <- FALSE
      
      # For each node C, verify transitivity
      unique_ids <- unique(df$id)
      for (node_c in unique_ids) {
        # Get all ancestors of C
        c_ancestors <- result$ancestor[result$id == node_c]
        
        # For each ancestor B of C
        for (node_b in c_ancestors) {
          # Get all ancestors of B
          b_ancestors <- result$ancestor[result$id == node_b]
          
          # All ancestors of B should also be ancestors of C
          for (node_a in b_ancestors) {
            checked <- TRUE
            expect_true(node_a %in% c_ancestors,
                       label = sprintf("Transitivity violated: %s is ancestor of %s, %s is ancestor of %s, but %s is not ancestor of %s",
                                    node_a, node_b, node_b, node_c, node_a, node_c))
          }
        }
      }
      
      # Always make at least one expectation
      expect_true(TRUE, label = "Transitivity test completed")
    }
  )
})

test_that("Function handles multiple roots (forest) correctly", {
  forall(
    gen.forest(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # Find all roots
      roots <- df$id[is.na(df$parent)]
      
      # Roots should have NA as ancestor, non-roots should not
      for (root in roots) {
        root_ancestors <- result$ancestor[result$id == root]
        expect_true(all(is.na(root_ancestors)),
                   label = sprintf("Root %s should only have NA ancestors", root))
      }
      
      # Filter out NA ancestors for remaining checks
      result_non_na <- result[!is.na(result$ancestor), ]
      
      # Verify all ancestors in results are also in the original df
      if (nrow(result_non_na) > 0) {
        expect_true(all(result_non_na$ancestor %in% df$id),
                   label = "Unknown ancestor found")
      } else {
        expect_true(TRUE, label = "No non-NA ancestors in forest")
      }
    }
  )
})

test_that("Diamond DAG: node with two parents has all ancestors", {
  forall(
    gen.diamond_dag(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # In diamond: D has parents B and C, both have parent A
      # So D's ancestors should be {B, C, A}
      d_ancestors <- result$ancestor[result$id == "D" & !is.na(result$ancestor)]
      
      expect_true(all(c("B", "C", "A") %in% d_ancestors),
                 label = "Diamond DAG: D should have ancestors B, C, and A")
      
      expect_equal(length(d_ancestors), 3,
                  label = "Diamond DAG: D should have exactly 3 ancestors")
    }
  )
})

test_that("Nodes with multiple parents: all paths explored", {
  forall(
    gen.dag_with_multiple_parents(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # For each node, verify that all ancestors reachable through any parent path are found
      unique_ids <- unique(df$id)
      
      for (node_id in unique_ids) {
        # Manually compute all ancestors using DFS through all parent paths
        expected_ancestors <- character(0)
        visited <- character(0)
        stack <- node_id
        
        while (length(stack) > 0) {
          current <- stack[1]
          stack <- stack[-1]
          
          if (current %in% visited) next
          visited <- c(visited, current)
          
          # Get all parents of current node
          parents <- df$parent[df$id == current & !is.na(df$parent)]
          
          for (p in parents) {
            if (!(p %in% expected_ancestors)) {
              expected_ancestors <- c(expected_ancestors, p)
              stack <- c(stack, p)
            }
          }
        }
        
        # Get actual ancestors from result (excluding NA)
        actual_ancestors <- result$ancestor[result$id == node_id & !is.na(result$ancestor)]
        
        # Check they match
        expect_true(setequal(actual_ancestors, expected_ancestors),
                   label = sprintf("Mismatch for node %s with multiple parents", node_id))
      }
      
      # Always make at least one expectation
      expect_true(length(unique_ids) > 0, label = "Multiple parents test completed")
    }
  )
})

test_that("No ancestors counted multiple times even with multiple paths", {
  forall(
    gen.dag_with_multiple_parents(),
    function(df) {
      result <- find_all_ancestors(df)
      
      # Filter out NA ancestors
      result <- result[!is.na(result$ancestor), ]
      
      # For each node, verify no duplicate ancestors
      unique_ids <- unique(df$id)
      
      for (node_id in unique_ids) {
        node_ancestors <- result$ancestor[result$id == node_id]
        
        expect_equal(length(node_ancestors), length(unique(node_ancestors)),
                    label = sprintf("Node %s has duplicate ancestors", node_id))
      }
      
      # Always make at least one expectation
      expect_true(length(unique_ids) > 0, label = "No duplicates test completed")
    }
  )
})

# ============================================================================
# Run all tests
# ============================================================================

cat("Running property-based tests for find_all_ancestors...\n")
test_file("test_find_ancestors.R")
