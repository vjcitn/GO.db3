# GO.db3 is an experimental package that partially emulates GO.db 

The [GO.db](https://bioconductor.org/packages/GO.db) package has a long
history as an outcome of the 
[BioconductorAnnotationPipeline](https://github.com/bioconductor/BioconductorAnnotationPipeline).  It works via the classes and methods of [AnnotationDbi](https://bioconductor.org/packages/AnnotationDbi).

AnnotationDbi defines a significant collection of database schemas with
the overall strategy depicted here, in a figure from an AnnotationDbi vignette.

<img src="https://raw.githubusercontent.com/vjcitn/GO.db2/fbf46341c64c1188cb0c0d082ccb3c7adce5ae42/man/figures/AnnoDbi.png" width="400px"/>

Many changes have occurred since the production of AnnotationDbi.

- The architects of the system have left Bioconductor.
- Some of the concepts targeted in the diagram aren't as significant to active users
as they once were (e.g., microarray annotations).
- Underlying resources being conveyed into the ecosystem have changed,
and access to some has been curtailed.

The Bioconductor project will leave GO.db in the state it had at 3.22.  Alternative approaches
to conveying Gene Ontology information for use in workflows will be explored in
a number of packages.  This is one example.

# Using GO.db3

## Installation

```
BiocManager::install("vjcitn/GO.db3") # be sure devtools and remotes are installed
```

## Examples

We do not use `select`.  We define `select3` as an ordinary function.

```
> library(GO.db3)
> select3("GO.db3", keys="GO:0009435", keytype="GOID", columns=c("GOID", "TERM", "ONTOLOGY"))
        GOID                      TERM ONTOLOGY
1 GO:0009435 NAD+ biosynthetic process       BP
```

We do not create environments like GOBPPARENTS in the package namespace.  Instead,
functions produce the environments.

```
> get("GO:0009435", GOBPPARENTS())
        is_a         is_a         is_a 
"GO:0006164" "GO:0019359" "GO:0019674" 
```

We have not, at this point, recreated all the interfaces to GO that were
available in GO.db.  Issues should be filed about missing elements.

## Details

A collection of parquet files underlies this package.  The collection was created using
the tooling at [BiocGOPrep](https://github.com/vjcitn/BiocGOprep).

```
allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
basename(allcon$files)
 [1] "go_bp_offspring.parquet" "go_bp_parents.parquet"  
 [3] "go_cc_offspring.parquet" "go_cc_parents.parquet"  
 [5] "go_mf_offspring.parquet" "go_mf_parents.parquet"  
 [7] "go_obsolete.parquet"     "go_ontology.parquet"    
 [9] "go_synonym.parquet"      "go_term.parquet"        
[11] "map_counts.parquet"     
```

Note that these files are 1/10th the size of the equivalent sqlite database.

### Environments

We need:

```
 [5] "GO_dbschema"   "GO.db"         "GOBPANCESTOR"  "GOBPCHILDREN" 
 [9] "GOBPOFFSPRING" "GOBPPARENTS"   "GOCCANCESTOR"  "GOCCCHILDREN" 
[13] "GOCCOFFSPRING" "GOCCPARENTS"   "GOMAPCOUNTS"   "GOMFANCESTOR" 
[17] "GOMFCHILDREN"  "GOMFOFFSPRING" "GOMFPARENTS"   "GOOBSOLETE"   
[21] "GOSYNONYM"     "GOTERM" 
```

as of 0.0.4 we had

```
[1] "GO.db"         "GOBPPARENTS"   "GOMFPARENTS"   "GOSYNONYM"    
[5] "keys"          "report_goparq" "select"        "show"         
```

How do we compute GOBPANCESTOR?  Note that we have roots for
subontologies.

```
> select(GO.db, keys="GO:0003674", columns="TERM")
        GOID               TERM
1 GO:0003674 molecular_function
> select(GO.db, keys="GO:0005575", columns="TERM")
        GOID               TERM
1 GO:0005575 cellular_component
> select(GO.db, keys="GO:0008150", columns="TERM")
        GOID               TERM
1 GO:0008150 biological_process
```

The parent resource:
```
> bppar = read_parquet("/Users/vincentcarey/Library/R/arm64/4.5/library/GO.db3/extdata/go323/go_bp_parents.parquet")
> head(bppar)
# A tibble: 6 × 3
  go_id      parent_id  relationship_type
  <chr>      <chr>      <chr>            
1 GO:0000001 GO:0048308 is_a             
2 GO:0000001 GO:0048311 is_a             
3 GO:0000011 GO:0007033 is_a             
```

The relationship types:
```
> table(bppar$relationship_type)

         ends_during       happens_during             has_part 
                   1                   13                  416 
                is_a negatively_regulates            occurs_in 
               41724                 2581                  139 
             part_of positively_regulates            regulates 
                4493                 2595                 2937 
```

Claude proposes:
```
# Function to find all ancestors for each node in a DAG
# Input: data.frame with columns 'id' and 'parent'
# Output: data.frame with columns 'id' and 'ancestor'

find_all_ancestors <- function(df) {
  # Function to get all ancestors for a single node
  get_ancestors <- function(node_id, parent_map) {
    ancestors <- c()
    current <- node_id
    
    # Traverse up the tree until we reach a root (parent is NA or not in map)
    while (!is.na(current) && current %in% names(parent_map)) {
      parent <- parent_map[[as.character(current)]]
      if (is.na(parent)) {
        break
      }
      ancestors <- c(ancestors, parent)
      current <- parent
    }
    
    return(ancestors)
  }
  
  # Create a named vector for fast parent lookup
  parent_map <- setNames(df$parent, df$id)
  
  # Build result list
  result_list <- list()
  
  for (node in df$id) {
    ancestors <- get_ancestors(node, parent_map)
    
    if (length(ancestors) > 0) {
      # Create a data.frame for this node's ancestors
      result_list[[length(result_list) + 1]] <- data.frame(
        id = rep(node, length(ancestors)),
        ancestor = ancestors,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all results
  if (length(result_list) > 0) {
    result <- do.call(rbind, result_list)
    rownames(result) <- NULL
    return(result)
  } else {
    # Return empty data.frame with correct structure if no ancestors found
    return(data.frame(id = character(0), ancestor = character(0), stringsAsFactors = FALSE))
  }
}

# Example usage:
# Create sample data
# example_df <- data.frame(
#   id = c("A", "B", "C", "D", "E"),
#   parent = c(NA, "A", "A", "B", "C")
# )
# 
# # Find all ancestors
# ancestors_df <- find_all_ancestors(example_df)
# print(ancestors_df)
#
# Expected output:
#   id ancestor
# 1  B        A
# 2  C        A
# 3  D        B
# 4  D        A
# 5  E        C
# 6  E        A

# To use with your data:
# your_result <- find_all_ancestors(your_data)
  

