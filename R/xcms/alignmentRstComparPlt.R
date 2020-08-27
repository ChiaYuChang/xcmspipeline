#! DEPRECATED
alignmentRstComparPlt <- function(obiwarp, peakGrp, file, ...) {
        phases <- names(obiwarp)
        alignment.p <- mapply(obiwarp, peakGrp, FUN = function(ow, pg) {
                ggarrange(ow$p$distr.p, pg$p$distr.p, ow$p$p, pg$p$p, ncol = 2, nrow = 2, common.legend = T, legend = "bottom")
        }, SIMPLIFY = F)
        
        pth <- dirname(file)
        fmt <- str_extract(basename(file), pattern = "\\..+$")
        fn  <- str_remove(basename(file), str_c(fmt, "$"))
        
        for (i in length(phases)) {
                f <- str_glue(fn[1], "_", phases[1], fmt)
                ggsave(filename = f, plot = alignment.p[[i]], path = pth, ...)
        }
        return(alignment.p)
}
