pkgname <- "manhattanVAAST"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('manhattanVAAST')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("manhattanVAAST")
### * manhattanVAAST

flush(stderr()); flush(stdout())

### Name: manhattanVAAST
### Title: a function to plot VAAST simple reports.
### Aliases: manhattanVAAST
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n.features, sig.level, vaast.simple, title, sig.line = TRUE, 
    axis.text = TRUE, custom.xlab = "index", sig.hjust = 0, sig.vjust = 0) 
{
    dat <- read.table(vaast.simple, header = FALSE, skip = 1, 
        sep = "\t", fill = TRUE)
    dat <- dat[dat$V4 > 0, ]
    dat <- dat[, 1:6]
    dat <- dat[grep(dat$V5, pattern = "chr"), ]
    pos.dat <- strsplit(as.character(dat$V5), split = ":|;", 
        perl = TRUE)
    dat$seqid <- sapply(pos.dat, FUN = function(xx) {
        return(xx[[1]][1])
    })
    dat$pos <- as.numeric(as.character(sapply(pos.dat, FUN = function(xx) {
        return(xx[[2]][1])
    })))
    dat <- dat[order(dat$seqid, dat$pos), ]
    dat$index.vec <- cumsum(1:length(dat$pos))
    dat$len.correction1 <- unlist(tapply(dat$pos, INDEX = dat$seqid, 
        FUN = function(x) {
            c(max(x), rep(0, length(x) - 1))
        }, simplify = TRUE))
    dat$len.correction3 <- unlist(tapply(dat$pos, INDEX = dat$seqid, 
        FUN = function(x) {
            rep(max(x), length(x))
        }, simplify = TRUE))
    dat <- dat[order(-dat$len.correction3, dat$seqid, dat$pos), 
        ]
    dat$len.correction2 <- cumsum(dat$len.correction1)
    dat$relative.pos <- as.numeric(as.character(dat$len.correction2)) + 
        as.numeric(as.character(dat$pos))
    axis.name <- unique(dat$seqid)
    axis.name.pos.rel <- as.vector(tapply(dat$index, INDEX = dat$seqid, 
        FUN = function(x) {
            middle <- (max(x) + min(x))/2
        }))
    axis.name.pos.real <- sort(as.vector(tapply(dat$relative.pos, 
        INDEX = dat$seqid, FUN = function(x) {
            middle <- (max(x) + min(x))/2
        })))
    plot(y = -log10(as.numeric(as.character(dat$V3))), x = dat$relative.pos, 
        col = as.factor(dat$seqid), xaxt = "n", xlab = custom.xlab, 
        pch = 20, ylab = "-log10(p-value)", main = title)
    if (sig.line == TRUE) {
        abline(h = -log10(sig.level/n.features), lty = 2, lwd = 3, 
            col = "grey")
        text(x = (0.5 * max(dat$relative.pos)) + sig.hjust, y = 0.5 + 
            -log10(sig.level/n.features) + sig.vjust, "genome-wide sig. level", 
            cex = 1)
    }
    if (axis.text == TRUE) {
        axis(1, at = axis.name.pos.real, labels = axis.name, 
            las = 2)
    }
    if (axis.text == FALSE) {
        axis(1, at = axis.name.pos.real, labels = FALSE, las = 2)
    }
    return(dat)
  }



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
