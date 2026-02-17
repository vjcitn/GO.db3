
#' emulate the GO.db environments, but as functions: GOBP
#' @return environment mapping from GOID to parents in BP ontology
#' @export
GOBPPARENTS = function() {
 allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
 tabref= arrow::open_dataset(grep("go_bp_parents", allcon$files, value=TRUE))
 thetab = tabref |> as.data.frame()
 ans = new.env()
 pars = thetab$parent_id
 names(pars) = thetab$relationship_type
 ids = thetab$go_id
 spar = split(pars, ids)
 nids = names(spar)
 for (i in seq_len(length(nids))) assign(nids[i], spar[[i]], ans)
 ans
}

#' emulate the GO.db environments, but as functions: GOMF
#' @return environment mapping from GOID to parents in MF ontology
#' @export
GOMFPARENTS = function() {
 allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
 tabref= arrow::open_dataset(grep("go_mf_parents", allcon$files, value=TRUE))
 thetab = tabref |> as.data.frame()
 ans = new.env()
 pars = thetab$parent_id
 names(pars) = thetab$relationship_type
 ids = thetab$go_id
 spar = split(pars, ids)
 nids = names(spar)
 for (i in seq_len(length(nids))) assign(nids[i], spar[[i]], ans)
 ans
}

#' emulate the GO.db environments, but as functions: GOSYNONYM
#' @return environment mapping from GOID to synonyms
#' @examples
#' head(unname(GOSYNONYM()[["GO:0009435"]]))
#' # this could be better
#' tmpcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
#' syn = arrow::open_dataset(grep("synonym", tmpcon$files, value=TRUE))
#' syn |> dplyr::filter(go_id == "GO:0009435") |> dplyr::collect()
#' @export
GOSYNONYM = function() {
 allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
 tabref= arrow::open_dataset(grep("go_synonym", allcon$files, value=TRUE))
 thetab = tabref |> as.data.frame()
 ans = new.env()
 syns = thetab$synonym
 names(syns) = thetab$scope
 ids = thetab$go_id
 ssyn = split(syns, ids)
 nids = names(ssyn)
 for (i in seq_len(length(nids))) assign(nids[i], ssyn[[i]], ans)
 ans
}
