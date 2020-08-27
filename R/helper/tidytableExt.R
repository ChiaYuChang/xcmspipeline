gather. <- function(.data, id.vars, ...) {
        if (!is.data.table(.data)) {
                .data <- as.data.table(.data)
        }
        as_tidytable(melt(.data, id.vars = id.vars, ...))
}

spread. <- function(.data, formula, ...) {
        as_tidytable(dcast.data.table(.data, formula, ...))
}