#' self-describing object for GO interface
#' @export
GO.db <- NULL

#' environment for GO ANCESTOR CC
#' @export
GOCCANCESTOR <- NULL

#' environment for GO ANCESTOR MF
#' @export
GOMFANCESTOR <- NULL

#' environment for GO ANCESTOR BP
#' @export
GOBPANCESTOR <- NULL

#' environment for GO PARENTS CC
#' @name GOCCPARENTS
#' @export
GOCCPARENTS <- NULL

#' environment for GO PARENTS MF
#' @name GOMFPARENTS
#' @export
GOMFPARENTS <- NULL

#' environment for GO PARENTS BP
#' @name GOBPPARENTS
#' @export
GOBPPARENTS <- NULL

#' environment for GO CHILDREN CC
#' @name GOCCCHILDREN
#' @note Previous versions of GO.db used 'isa' to name elements
#' of this environment instead of 'is_a' which is
#' now used in go.obo, and here.
#' @export
GOCCCHILDREN <- NULL

#' environment for GO CHILDREN MF
#' @name GOMFCHILDREN
#' @note Previous versions of GO.db used 'isa' to name elements
#' of this environment instead of 'is_a' which is
#' now used in go.obo, and here.
#' @export
GOMFCHILDREN <- NULL

#' environment for GO CHILDREN BP
#' @name GOBPCHILDREN
#' @note Previous versions of GO.db used 'isa' to name elements
#' of this environment instead of 'is_a' which is
#' now used in go.obo, and here.
#' @export
GOBPCHILDREN <- NULL

#' environment for GO OFFSPRING CC
#' @name GOCCOFFSPRING
#' @export
GOCCOFFSPRING <- NULL

#' environment for GO OFFSPRING MF
#' @name GOMFOFFSPRING
#' @export
GOMFOFFSPRING <- NULL

#' environment for GO OFFSPRING BP
#' @name GOBPOFFSPRING
#' @export
GOBPOFFSPRING <- NULL

#' environment for GO TERM
#' @importClassesFrom AnnotationDbi GOTerms
#' @name GOTERM
#' @export
GOTERM <- NULL


#' environment for GO SYNONYM
#' @name GOSYNONYM
#' @note The behavior is unlike that of GO.db::GOSYNONYM which gives
#' stanzas from OBO.  Synonymous phrases are returned along with ids
#' of secondary GO terms.
#' @examples
#' get("GO:0000215", GO.db3::GOSYNONYM)
#' # see  https://www.ebi.ac.uk/QuickGO/term/GO:0000215 for related information
#' @export
GOSYNONYM <- NULL


#' environment for GOTERM
#' @export
GOTERM <- NULL

.onLoad <- function(libname, pkgname) {
  ns <- getNamespace(pkgname)
  
  # Remove if exists
  if (exists("GO.db", envir = ns, inherits = FALSE)) {
    if (bindingIsLocked("GO.db", ns)) {
      unlockBinding("GO.db", ns)
    }
    rm(list = "GO.db", envir = ns)
  }
  
  # Now create the active binding for GO.db
  makeActiveBinding("GO.db", .GO.db, ns)

# new bindings for ANCESTOR environments
  fl = vector("list", 3)
  names(fl) = c("cc", "mf", "bp")
  for (type in c("cc", "mf", "bp")) {
    tab = readRDS(system.file("ancestors", sprintf("%sancestorstab.rds", type), package="GO.db3"))
# those tables were produced using code in inst/utils, as ancestor enumeration in DAGs is
# not readily available in R ... method was verified against networkx, and hedgehog
# code for further verification is available in inst/verif
    thelist = split(tab[,2], tab[,1])
    ttype = toupper(type)
    CURRENT = sprintf("GO%sANCESTOR", ttype)
    CURRENTE = sprintf("GO%sANCESTOR_env", ttype)
  # start environment production
     assign(CURRENTE, new.env(hash=TRUE), envir=ns)
     nn = lapply(seq_len(length(thelist)),
        function(i) assign(names(thelist)[i], thelist[[i]], envir=get(CURRENTE, envir=ns)))
  # Now create the active binding
     fl[[type]] = local({
        currente <- CURRENTE 
        function() {
        get(currente, envir=ns)
        }})
     rm(list=CURRENT, envir=ns)
     makeActiveBinding(CURRENT, fl[[type]], ns)
    }
# end ANCESTOR environments

# begin PARENTS environments
  fl2 = vector("list", 3)
  names(fl2) = c("cc", "mf", "bp")
  for (type in c("cc", "mf", "bp")) {
    allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
    tabref= arrow::open_dataset(grep(sprintf("go_%s_parents", type), allcon$files, value=TRUE))
    thetab = tabref |> as.data.frame()
    pars = thetab$parent_id
    names(pars) = thetab$relationship_type
    ids = thetab$go_id
    spar = split(pars, ids)
    nids = names(spar)
#    
    ttype = toupper(type)
    CURRENT2 = sprintf("GO%sPARENTS", ttype)
    CURRENTE2 = sprintf("GO%sPARENTS_env", ttype)
  # start environment production
     assign(CURRENTE2, new.env(hash=TRUE), envir=ns)
     nn = lapply(seq_len(length(nids)),
        function(i) assign(nids[i], spar[[i]], envir=get(CURRENTE2, envir=ns)))
  # Now create the active binding
     fl2[[type]] = local({
        currente2 <- CURRENTE2
        function() {
        get(currente2, envir=ns)
        }})
     rm(list=CURRENT2, envir=ns)
     makeActiveBinding(CURRENT2, fl2[[type]], ns)
    }
## end PARENTS environments

## do SYNONYM env

    allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
    tabref= arrow::open_dataset(grep("go_synonym", allcon$files, value=TRUE))
    thetab = tabref |> as.data.frame()
  
    syns = thetab$synonym
    names(syns) = thetab$scope
    ids = thetab$go_id
    ssyn = split(syns, ids)
    nids = names(ssyn)

    CURRENT2 = "GOSYNONYM"
    CURRENTE2 ="GOSYNONYM_env"
  # start environment production
    assign(CURRENTE2, new.env(hash=TRUE), envir=ns)

    for (i in seq_len(length(nids))) assign(nids[i], ssyn[[i]], envir=get(CURRENTE2, envir=ns))

  # Now create the active binding
     fl2 = local({
        currente2 <- CURRENTE2
        function() {
        get(currente2, envir=ns)
        }})
     rm(list=CURRENT2, envir=ns)
     makeActiveBinding(CURRENT2, fl2, ns)

# DO CHILDREN env

  fl2 = vector("list", 3)
  names(fl2) = c("cc", "mf", "bp")
  for (type in c("cc", "mf", "bp")) {
    allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
    tabref= arrow::open_dataset(grep(sprintf("go_%s_parents", type), allcon$files, value=TRUE))
    thetab = tabref |> as.data.frame()
    uu = split(thetab$go_id, thetab$parent_id)
    rr = split(thetab$relationship_type, thetab$parent_id)
    for (i in seq_len(length(rr))) names(uu[[i]]) = rr[[i]]
    nids = names(rr)
#
    ttype = toupper(type)
    CURRENT2 = sprintf("GO%sCHILDREN", ttype)
    CURRENTE2 = sprintf("GO%sCHILDREN_env", ttype)
  # start environment production
     assign(CURRENTE2, new.env(hash=TRUE), envir=ns)
     nn = lapply(seq_len(length(uu)),
        function(i) assign(nids[i], uu[[i]], envir=get(CURRENTE2, envir=ns)))
  # Now create the active binding
     fl2[[type]] = local({
        currente2 <- CURRENTE2
        function() {
        get(currente2, envir=ns)
        }})
     rm(list=CURRENT2, envir=ns)
     makeActiveBinding(CURRENT2, fl2[[type]], ns)
     }

# DO OFFSPRING env

  fl2 = vector("list", 3)
  names(fl2) = c("cc", "mf", "bp")
  for (type in c("cc", "mf", "bp")) {
    allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
    tabref= arrow::open_dataset(grep(sprintf("go_%s_offspring", type), allcon$files, value=TRUE))
    thetab = tabref |> as.data.frame()
    uu = split(thetab$offspring_id, thetab$go_id)
    nids = names(uu)
#
    ttype = toupper(type)
    CURRENT2 = sprintf("GO%sOFFSPRING", ttype)
    CURRENTE2 = sprintf("GO%sOFFSPRING_env", ttype)
  # start environment production
     assign(CURRENTE2, new.env(hash=TRUE), envir=ns)
     nn = lapply(seq_len(length(uu)),
        function(i) assign(nids[i], uu[[i]], envir=get(CURRENTE2, envir=ns)))
  # Now create the active binding
     fl2[[type]] = local({
        currente2 <- CURRENTE2
        function() {
        get(currente2, envir=ns)
        }})
     rm(list=CURRENT2, envir=ns)
     makeActiveBinding(CURRENT2, fl2[[type]], ns)
     }

# END OFFSPRING

# DO TERM env
    allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
    tabref= arrow::open_dataset(grep("term", allcon$files, value=TRUE))
    thetab = tabref |> as.data.frame()
    nel = nrow(thetab)
    ans = vector("list", nel)
    ids = thetab[["go_id"]]
    terms = thetab[["term"]]
    onts = thetab[["ontology"]]
    defs = thetab[["definition"]]
    nids = thetab[["go_id"]]
    names(ans) = nids
    for (i in seq_len(nel)) 
      ans[[i]] = new("GOTerms", GOID=ids[i],
           Term=terms[i], Ontology=onts[i],
           Definition=defs[i], Synonym=NA_character_,
             Secondary=NA_character_)
    CURRENT2 = "GOTERM"
    CURRENTE2 = "GOTERM_env"
  # start environment production
     assign(CURRENTE2, new.env(hash=TRUE), envir=ns)
     nn = lapply(seq_len(length(ans)),
        function(i) assign(nids[i], ans[[i]], envir=get(CURRENTE2, envir=ns)))
  # Now create the active binding
     fl2[[type]] = local({
        currente2 <- CURRENTE2
        function() {
        get(currente2, envir=ns)
        }})
     rm(list=CURRENT2, envir=ns)
     makeActiveBinding(CURRENT2, fl2[[type]], ns)

#> head(ter)
## A tibble: 6 × 4
#  go_id      term                                            ontology definition
#  <chr>      <chr>                                           <chr>    <chr>     
#1 GO:0000001 mitochondrion inheritance                       BP       The distr…
#2 GO:0000006 high-affinity zinc transmembrane transporter a… MF       Enables t…
#3 GO:0000007 low-affinity zinc ion transmembrane transporte… MF       Enables t…
#4 GO:0000009 alpha-1,6-mannosyltransferase activity          MF       Catalysis…
#5 GO:0000010 heptaprenyl diphosphate synthase activity       MF       Catalysis…
#6 GO:0000011 vacuole inheritance                             BP       The distr…
#> getClass("GOTerms")
#Class "GOTerms" [package "AnnotationDbi"]
#
#Slots:
#                                                                        
#Name:        GOID       Term   Ontology Definition    Synonym  Secondary
#Class:  character  character  character  character  character  character

}
