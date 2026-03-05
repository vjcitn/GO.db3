#' columns method
#' @param x instance of GOparq
#' @export
setMethod("columns", "GOparq", function(x) c("GOID", "TERM", "DEFINITION", "ONTOLOGY"))

#' keytypes method
#' @param x instance of GOparq
#' @export
setMethod("keytypes", "GOparq", function(x) c("GOID", "TERM", "DEFINITION", "ONTOLOGY"))

#' mapIds method
#' @importFrom IRanges CharacterList
#' @param x instance of GOparq
#' @param keys character vector
#' @param column single character element
#' @param keytype single character element among value of keytypes
#' @param \dots not used
#' @param multiVals, only relevant for keytype="ONTOLOGY", can take values 'first' (default),
#' 'list', or 'CharacterList'
#' @examples
#' mapIds(GO.db3::GO.db, keys=c("MF", "BP"), column="GOID", keytype="ONTOLOGY", multiVals="CharacterList")
#' @export
setMethod("mapIds", "GOparq", function(x, keys, column, keytype, ..., multiVals) {
  stopifnot(column %in% columns(x))
  stopifnot(keytype %in% keytypes(x))
  if (is.function(multiVals)) stop("multiVals function argument not supported at this time.")
  src = slot(x, "conn")$termref
  termsdf = src |> as.data.frame()
  if (missing(multiVals)) multiVals = "first"
  if (keytype == "GOID") {
     if (column == "TERM") return(termsdf |> dplyr::select(go_id, term) |> 
       filter(go_id %in% keys) |> transmute(GOID=go_id, TERM=term))
     else if (column == "DEFINITION") return(termsdf |> dplyr::select(go_id, definition) |> 
       filter(go_id %in% keys) |> transmute(GOID=go_id, DEFINITION=definition))
     else if (column == "ONTOLOGY") return(termsdf |> dplyr::select(go_id, ontology) |> 
       filter(go_id %in% keys) |> transmute(GOID=go_id, ONTOLOGY=ontology))
     else if (column == "GOID") { names(x) = x; return(x) }
  }
  else if (keytype == "TERM") {
     if (column == "GOID") return(termsdf |> dplyr::select(go_id, term) |> 
       filter(term %in% keys) |> transmute(GOID=go_id, TERM=term))
     else if (column == "DEFINITION") return(termsdf |> dplyr::select(go_id, term, definition) |> 
       filter(term %in% keys) |> transmute(GOID=go_id, DEFINITION=definition))
     else if (column == "ONTOLOGY") return(termsdf |> dplyr::select(go_id, term, ontology) |> 
       filter(term %in% keys) |> transmute(GOID=go_id, ONTOLOGY=ontology))
  }
  else if (keytype == "ONTOLOGY") {  # now there will be multiVals
     if (column == "GOID") {
       ans = termsdf |> dplyr::select(go_id, term, ontology) |> 
           filter(ontology %in% keys) |> transmute(GOID=go_id, ONTOLOGY=ontology)
       if (multiVals == "first") {
           ans = split(ans, ans$ONTOLOGY)
           res = lapply(ans, function(x) x[1,,drop=FALSE])
           return(do.call(rbind, res))
       }
       else if (multiVals == "list") {
           ans = split(ans, ans$ONTOLOGY)
           return(ans)
           }
       else if (multiVals == "CharacterList") {
           ans = split(ans$GOID, ans$ONTOLOGY)
           return(do.call(IRanges::CharacterList, ans))
           }
     }
    else stop("combination of ketype, column, multiVals not supported at this time.")
   }
})
   
