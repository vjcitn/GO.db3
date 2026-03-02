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
}
