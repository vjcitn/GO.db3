  
test_that("check a synonym", {
   chk = get("GO:2001182", GOSYNONYM)
   id = slot(chk, "GOID")
   expect_equal(id, "GO:0032655")
   ont = slot(chk, "Ontology")
   expect_equal(ont, "BP")
   def = slot(chk, "Definition")
   expect_equal(def, "Any process that modulates the frequency, rate, or extent of interleukin-12 production.")
   syns = slot(chk, "Synonym")
   expect_true("regulation of IL-12 production" %in% syns)
})

test_that("check a parent", {
   chk = get("GO:0032655", GOBPPARENTS)
   expect_true(all(c("GO:0001817", "GO:0032615") %in% as.character(chk)))
})
