#' superficial emulator of AnnotationDbi::select for Gene Ontology
#' @import arrow
#' @import dplyr
#' @param x character, must be "GO.db3"
#' @param keys for filtering, if NULL, no filtering performed
#' @param columns for joining and selecting
#' @param keytype for table selection: one of "term", "goid"; all caps may be used
#' @return data.frame
#' @note GO.db permitted columns and keytypes "DEFINITION", "GOID", "ONTOLOGY", "TERM"
#' @examples
#' select3("GO.db3", keys="low-affinity zinc ion transmembrane transporter activity", keytype="term")
#' select3("GO.db3", keys="GO:0009435", keytype="GOID") |> dplyr::select(-definition)
#' select3("GO.db3", keys="GO:0009435", keytype="goid", columns=c("GOID", "TERM", "ONTOLOGY"))
#' @export
select3 = function (x, keys, columns = NULL, keytype, ...) 
{
    keytype = tolower(keytype)
    stopifnot(keytype %in% c("term", "goid"))
    stopifnot(length(keytype) == 1)
    pname = packageName()
    stopifnot(x == pname)
    allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
    termref = arrow::open_dataset(grep("_term", allcon$files, value=TRUE))
    if (keytype == "term") {
        if (!is.null(keys)) 
            thetab = dplyr::filter(termref, term %in% keys)
    }
    else if (keytype == "goid") {
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
}

