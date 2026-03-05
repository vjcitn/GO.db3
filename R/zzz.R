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
#' @importFrom stats na.omit
#' @name GOSYNONYM
#' @examples
#' get("GO:1905121", slot(GOSYNONYM, "datacache"))
#' GOSYNONYM[["GO:1905121"]]
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
        env = get(currente, envir=ns)
        tt = new("GOparqMap", datacache=env, keys=ls(env))
        tt
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
        env = get(currente2, envir=ns)
        tt = new("GOparqMap", datacache=env, keys=ls(env))
        tt
        }})
     rm(list=CURRENT2, envir=ns)
     makeActiveBinding(CURRENT2, fl2[[type]], ns)
    }
## end PARENTS environments

## Note: the production of thousands of GOTerms objects might
## slow down package installation.  The ans list could be serialized
## as it was for the synonyms resource.

# DO TERM env

    allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
#    tabref= arrow::open_dataset(grep("term", allcon$files, value=TRUE))
#    thetab = tabref |> as.data.frame()

    thetab = read_parquet(grep("term", allcon$files, value=TRUE)) |> as.data.frame()
    syntab = read_parquet(grep("synonym", allcon$files, value=TRUE)) |> as.data.frame()
    thetab = dplyr::left_join(thetab, syntab, by="go_id")

    spltab = split(thetab, thetab$go_id)
    nel = length(spltab)
    nids = names(spltab)
    ans = vector("list", nel)

#    nel = nrow(thetab)
#    ids = thetab[["go_id"]]
#    terms = thetab[["term"]]
#    onts = thetab[["ontology"]]
#    defs = thetab[["definition"]]
#    nids = thetab[["go_id"]]
#    names(ans) = nids
    for (i in seq_len(nel)) {    # THIS ANS WILL BE USED FOR SYNONYMS, to get Term, Ont, Def
      cur = spltab[[i]]
      ans[[i]] = new("GOTerms", GOID=cur$go_id[1],
           Term=cur$term[1], Ontology=cur$ont[1],
           Definition=cur$definition[1], Synonym=as.character(na.omit(cur$synoym)),
             Secondary=as.character(na.omit(cur$secondary)))
      }
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
        env = get(currente2, envir=ns)
        tt = new("GOparqMap", datacache=env, keys=ls(env))
        tt
        }})
     rm(list=CURRENT2, envir=ns)
     makeActiveBinding(CURRENT2, fl2[[type]], ns)

## do SYNONYM env

# blocking this code which was essential for emulating the
# GOSYNONYM environment of GO.db, but is slow
# Each release it will be run to produce an gosynonym_x_xx.rds for 
# storage in inst/synonyms
if (FALSE) {
     allcon = arrow::open_dataset(system.file("extdata", "go323", package="GO.db3"))
     tabref= arrow::open_dataset(grep("go_synonym", allcon$files, value=TRUE))

     tr = tabref |> dplyr::collect()
     tabref = as.data.frame(tr)

     hassec = tabref[which(!is.na(tabref$secondary)),]

     tt = split(hassec, hassec$secondary)
     obs = lapply(tt, function(x) {
       curt = ans[[x$go_id]] #get(x$go_id[1], GOTERM, envir=ns)
       new("GOTerms", GOID=x$go_id[1], Term=slot(curt, "Term"),
         Ontology=slot(curt, "Ontology"), Definition=slot(curt, "Definition"),
         Synonym=(tabref |> filter(go_id==x$go_id[1]))$synonym, Secondary=x$secondary)
     })
     nids = names(obs)
}

# special serialization step after passing check to see if we
# can get package load time down from 16 seconds

#    saveRDS(obs, file="~/gosynonym_3_23.rds")
    obs = readRDS(system.file("synonyms", "gosynonym_3_23.rds", package="GO.db3"))
    nids = names(obs)


    CURRENT2 = "GOSYNONYM"
    CURRENTE2 ="GOSYNONYM_env"
  # start environment production
    assign(CURRENTE2, new.env(hash=TRUE), envir=ns)

    for (i in seq_len(length(nids))) assign(nids[i], obs[[i]], envir=get(CURRENTE2, envir=ns))

  # Now create the active binding
     fl2 = local({
        currente2 <- CURRENTE2
        function() {
        env = get(currente2, envir=ns)
        tt = new("GOparqMap", datacache=env, keys=ls(env))
        tt
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
        #get(currente2, envir=ns)
        #ans = get(currente2, envir=ns)
        env = get(currente2, envir=ns)
        tt = new("GOparqMap", datacache=env, keys=ls(env))
        tt
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
        env = get(currente2, envir=ns)
        tt = new("GOparqMap", datacache=env, keys=ls(env))
        tt
        }})
     rm(list=CURRENT2, envir=ns)
     makeActiveBinding(CURRENT2, fl2[[type]], ns)
     }

# END OFFSPRING


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
