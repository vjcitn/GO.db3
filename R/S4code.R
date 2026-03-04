# complete emulation would address all these:
#> methods(class=class(GOMFPARENTS)) # AnnDbBimap
# [1] [                 [[                $                 as.character     
# [5] as.data.frame     as.list           colmetanames      colnames         
# [9] contents          count.links       count.mappedkeys  count.mappedLkeys
#[13] count.mappedRkeys dbconn            dbfile            dbInfo           
#[17] dbmeta            dbschema          dim               direction        
#[21] direction<-       eapply            exists            get              
#[25] getBimapFilters   isNA              keyname           keys             
#[29] keys<-            length            links             Lkeyname         
#[33] Lkeys             Lkeys<-           Llength           ls               
#[37] mappedkeys        mappedLkeys       mappedRkeys       mget             
#[41] ncol              nhit              nrow              Rattribnames     
#[45] Rattribnames<-    revmap            Rkeyname          Rkeys            
#[49] Rkeys<-           Rlength           sample            show             
#[53] subset            summary           tagname           toTable          
#see '?methods' for accessing help and source code

# from globaltest:
# importFrom(AnnotationDbi, Term, Secondary, as.list, Ontology, mappedkeys, keys)

#print.GOparqEnv = function(x, ...) cat("GOparqEnv: use ls, get, mget, [[, ...\n")

#' inheritor
#' @importClassesFrom AnnotationDbi AnnDbBimap GOTermsAnnDbBimap
#' @export
setClass("GOparqMap", contains="GOTermsAnnDbBimap")  # only will address items that work on an environment

#' selector
#' @param x instance of GOparqMap
#' @param i character 
#' @param j character, likely not used
#' @param \dots not used
#' @export
setMethod("[[", "GOparqMap", function(x,i,j,...) get(i, slot(x,"datacache")))


#tt = new("GOparqMap", datacache=GOTERM, Lkeys=ls(GOTERM))
#head(keys(tt))
#methods(class="GOparqMap")
#tt = new("GOparqMap", datacache=GOTERM, Lkeys=ls(GOTERM))
#tt[["GO:0000001"]]
#setMethod("[[", "GOparqMap", function(x,i,j,...) get(i, slot(x@datacache)))
#tt[["GO:0000001"]]

