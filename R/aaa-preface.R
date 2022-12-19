`$.cnt` <- function(x, y) {    
    checkmate::assertNames(y, type = "unique", subset.of = names(x), .var.name = as.character(substitute(x)))
    NextMethod()
}

`[[.cnt` <- `$.cnt`

poc <- function(...) {
    structure(
        list(...),
        class = "cnt"
    )
}
