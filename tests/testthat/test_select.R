
test_that("select works", {
   
   a1 =  select(GO.db, keys="low-affinity zinc ion transmembrane transporter activity", keytype="term",
     columns=c("GOID", "DEFINITION"))
   a2 = select(GO.db, keys="GO:0009435", keytype="GOID", columns="TERM")
   a3 = select(GO.db, keys="GO:0009435", keytype="GOID", columns=c("GOID", "TERM", "ONTOLOGY"))
# check that GOID is forced in as column
   a4 = select(GO.db, keys="GO:0009435", keytype="GOID", columns=c("TERM", "ONTOLOGY"))
# check that missing keytype is allowed
   a5 = select(GO.db, keys="GO:0009435", columns=c("TERM", "ONTOLOGY"))
   expect_true(all(dim(a3)==c(1L, 3L)))
   expect_true(all(dim(a4)==c(1L, 3L)))
   expect_true(all(dim(a5)==c(1L, 3L)))
   expect_message(select(GO.db, keys="GO:0009435", columns=c("TERM", "ONTOLOGY"))) # no keytype given
   expect_error(select(GO.db, keys="GO:0009435", keytype="GOID"))
})
  
