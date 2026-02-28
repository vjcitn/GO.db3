#' provide metadata about GO.obo used for this package
#' @export
report_goparq = function () 
{
    f = system.file("extdata", "prologue.txt", package = "GO.db3")
    cont = readLines(f)
    cat(grep("^format|^data-version|property_value", cont, value = TRUE), 
        sep = "\n")
}

