#' self-describing object for GO interface
#' @export
GO.db <- NULL

#' self-describing object for GO ANCESTOR CC
#' @export
GOCCANCESTOR <- NULL

#' self-describing object for GO ANCESTOR MF
#' @export
GOMFANCESTOR <- NULL

#' self-describing object for GO ANCESTOR BP
#' @export
GOBPANCESTOR <- NULL

#' self-describing object for GO PARENTS CC
#' @name GOCCPARENTS
#' @export
GOCCPARENTS <- NULL

#' self-describing object for GO PARENTS MF
#' @name GOMFPARENTS
#' @export
GOMFPARENTS <- NULL

#' self-describing object for GO PARENTS BP
#' @name GOBPPARENTS
#' @export
GOBPPARENTS <- NULL

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

}
