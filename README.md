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
