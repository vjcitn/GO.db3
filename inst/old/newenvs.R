makeANCESTORS = function() function(type="cc") {

# inst/ancestors will hold rds files produced by code in inst/utils

 tab = readRDS(system.file("ancestors", sprintf("%sancestorstab.rds", type), package="GO.db3"))
 thelist = split(tab[,2], tab[,1])

 ttype = toupper(type)
 CURRENT = sprintf("GO%sANCESTOR", ttype)
 CURRENTE = sprintf("GO%sANCESTOR_env", ttype)

# this is somewhat clumsy but works to establish a functional environment
# note that GO2ALLEGS uses the GO hierarchy to add genes mapped to conceptual offspring of term
#

# start environment production
  assign(CURRENTE, new.env(hash=TRUE), envir=ns)
  nn = lapply(seq_len(length(thelist)),
     function(i) assign(names(thelist)[i], thelist[[i]], , envir=get(CURRENTE, envir=ns)))
# Now create the active binding
#  f = function() {
#     get(CURRENTE)
#     }
#  rm(list=CURRENT, envir=ns)
#  makeActiveBinding(CURRENT, f, ns)
  get(CURRENTE)
}

