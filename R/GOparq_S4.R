#' class for parquet representation of GO
#' @note This class just serves as a bridge to parquet files that emulate
#' the former SQLite schema of AnnotationDbi-based GO.db.
#' @import methods
#' @export
setClass("GOparq", representation(conn="ANY"))

#' display for parquet representation of GO
#' @param object instance of GOparq
#' @export
setMethod("show", "GOparq", function(object) {
  cat(sprintf("GO.db analog for Bioconductor %s\n", as.character(BiocManager::version())))
  cat(report_goparq())
  cat("use select, mapIds, etc. ...\n")
})

#' connector
#' @export
GO.db = function() {
    allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
    termref = arrow::open_dataset(grep("_term", allcon$files, value=TRUE))
    new("GOparq", conn=list(allcon=allcon, termref=termref))
}

#' emulate AnnotationDbi
#' @importFrom AnnotationDbi select
#' @rawNamespace import(dplyr, except=select)
#' @import arrow
#' @param x instance of GOparq
#' @param keys for filtering, if NULL, no filtering performed
#' @param columns for joining and selecting
#' @param keytype for table selection: one of "term", "goid"; all caps may be used
#' @param \dots not used
#' @return data.frame
#' @examples
#' GO <- GO.db()
#' select(GO, keys="low-affinity zinc ion transmembrane transporter activity", keytype="term",
#'     columns=c("GOID", "DEFINITION"))
#' select(GO, keys="GO:0009435", keytype="GOID", columns="TERM")
#' select(GO, keys="GO:0009435", keytype="GOID", columns=c("GOID", "TERM", "ONTOLOGY"))
#' @export
setMethod("select", "GOparq", function(x, keys, columns, keytype, ...) {
# backwards compatibility of missing keytype
    if (missing(keytype)) {
     message("missing keytype is allowed for backwards compatibility; it will become a warning in bioc > 3.23")
     keytype = "goid"
     }
# need lower case for parquet
    keytype = tolower(keytype)
    if (missing(columns)) stop("'columns' is missing but is an obligatory character vector argument to 'select(GO,...)'.")
    if (missing(keys)) stop("'keys' is missing but is an obligatory character vector argument to 'select(GO,...)'.")
    possible_columns = c("GOID", "TERM", "ONTOLOGY", "DEFINITION")
    chkc = setdiff(columns, possible_columns)
    if (length(chkc)>0) stop(sprintf("%s is not a permissible value in columns.", chkc))
    if (!("GOID" %in% columns)) columns = c("GOID", columns)  # GO.db generally returned GOID
    stopifnot(keytype %in% c("term", "goid"))
    stopifnot(length(keytype) == 1)
    allcon = x@conn$allcon
    termref = x@conn$termref
    if (keytype == "term") {
        if (!is.null(keys)) 
            thetab = dplyr::filter(termref, term %in% keys)
    } else if (keytype == "goid") {
        if (!is.null(keys)) 
            thetab = dplyr::filter(termref, go_id %in% keys)
    }
    if (!is.null(columns)) {
        if ("GOID" %in% columns) 
            thetab = mutate(thetab, GOID = go_id)
        if ("TERM" %in% columns) 
            thetab = mutate(thetab, TERM = term)
        if ("ONTOLOGY" %in% columns) 
            thetab = mutate(thetab, ONTOLOGY = ontology)
        if ("DEFINITION" %in% columns) 
            thetab = mutate(thetab, DEFINITION = definition)
        thetab = as.data.frame(thetab)

        return(thetab[, columns])
    }
    as.data.frame(thetab)
})
