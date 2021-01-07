readDefaultConfig <- function(basePATH, xcmsPATH) {
        DEFAULT <- list(
                base = import(file  = basePATH,
                              which = "Parameters",
                              range = readxl::cell_cols("A:D"),
                              setclass = "data.table"),
                phase = list(
                        NEG = import(file  = basePATH,
                                     which = "NEG", 
                                     range = readxl::cell_cols("A:D"), 
                                     setclass = "data.table"),
                        POS = import(file  =  basePATH, 
                                     which = "POS", 
                                     range = readxl::cell_cols("A:D"), 
                                     setclass = "data.table")
                ),
                xcms = import(file = xcmsPATH, 
                              setclass = "data.table")
        )
        return(DEFAULT)
}