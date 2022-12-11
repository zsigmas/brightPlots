`$.cnt` <- function(x, y) {
    stopifnot(y %in% names(x))
    NextMethod()
}

`[[.cnt` <- `$.cnt`

poc <- function(...) {
    structure(
        list(...),
        class = "cnt"
    )
}
