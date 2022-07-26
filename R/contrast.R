


setMethod(
  "contrast",
  signature(object = "merMod", specs = "formula"),
  function(object, specs) {
    tms <- stats::terms(specs)
    tlab <- attr(tms, "term.labels")
    specs <- stats::reformulate(tlab, intercept = FALSE)
    x <- stats::model.matrix(object)
    fx <- stats::model.matrix(specs, object@frame)
    cmx <- colMeans(x)
    conx <- qr.solve(qr(fx), x)
    if ( is.null(nrow(conx)) ) conx <- t(conx)
    ## Replace extraneous terms with means in cmx
    ulab <- unique(unlist(strsplit(tlab, ":")))
    avoid.vars <- unlist(lapply(ulab, function(j) {
      if ( is.factor(object@frame[, j]) )
        paste0(j, levels(object@frame[, j]))
      else if ( is.character(object@frame[, j]) )
        paste0(j, unique(object@frame[, j]))
      else
        NULL
    }))
    avoid <- logical( ncol(x) )
    for ( i in 1:length(avoid.vars) ) {
      avoid <- avoid |
        grepl( avoid.vars[i], colnames(x), fixed = TRUE )
    }
    for ( j in which(!avoid) ) conx[, j] <- cmx[j]
    ## Return contrast matrix
    new("contrast", .Data = conx)
  })


setMethod(
  "delta",
  signature(a = "contrast", b = "contrast"),
  function(a, b, ...) {
    stopifnot( ncol(a) == ncol(b) )
    stopifnot( nrow(a) %% nrow(b) == 0 )
    namesf <- function(anames, bnames, sep = " - ") {
      paste( paste0("(", anames), paste0(bnames, ")"), sep = sep )
    }
    na <- nrow(a)
    nb <- nrow(b)
    if ( na >= nb ) {
      res <- a
      i <- rep( 1:nb, na %/% nb )
      res@.Data <- a@.Data - b@.Data[i, ]
      rownames(res) <- namesf( rownames(a), rownames(b)[i] )
    } else {
      res <- b
      i <- rep( 1:na, nb %/% na )
      res@.Data <- a@.Data[i, ] - b@.Data
      rownames(res) <- namesf( rownames(a)[i], rownames(b) )
    }
    res
  }
)



setMethod(
  "-", signature(e1 = "contrast", e2 = "contrast"),
  function(e1, e2) {
    delta(e1, e2)
  }
)




## --- S3 Methods ----------------------------------------------------

c.contrast <- function(..., recursive = FALSE) {
  new("contrast", .Data = do.call("rbind", list(...)) )
}

names.contrast <- function(x) rownames(x@.Data)

ncol.contrast <- function(x) NCOL( x@.Data )

nrow.contrast <- function(x) {
  if ( length(d <- dim(x@.Data)) ) d[1L] else 1L
}

rbind.contrast <- function(..., deparse.level = 1) c(...)





## --- Indexing ------------------------------------------------------

setMethod(
  "[", signature(x = "contrast", i = "numeric", j = "missing",
                 drop = "ANY"),
  function(x, i, j, ..., drop) {
    x@.Data <- x@.Data[i,, drop = FALSE]
    x
  })

setMethod(
  "[", signature(x = "contrast", i = "character", j = "missing",
                 drop = "ANY"),
  function(x, i, j, ..., drop) {
    m <- na.exclude(match(i, rownames(x)))
    x@.Data <- x@.Data[m,, drop = FALSE]
    x
  })

setMethod(
  "[", signature(x = "contrast", i = "logical", j = "missing",
                 drop = "ANY"),
  function(x, i, j, ..., drop) {
    x@.Data <- x@.Data[i,, drop = FALSE]
    x
  })

setMethod(
  "[", signature(x = "contrast", i = "missing", j = "missing",
                 drop = "ANY"),
  function(x, i, j, ..., drop) {
    x
  })

setMethod(
  "[", signature(x = "contrast", i = "missing", j = "ANY",
                 drop = "ANY"),
  function(x, i, j, ..., drop) {
    stop("Cannot index 'contrast' objects without argument 'i'")
  })


## Catch-all indexing
setMethod(
  "[", signature(x = "contrast", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop) {
    fmt <- function(x) {
      if ( length(x) > 1 )
        paste0("c(", x, ")", collapse = ",")
      else
        paste0("(", x, ")")
    }
    stop("Undefined method of indexing 'contrast' objects: i ",
         fmt(class(i)), ", j ", fmt(class(j)))
  })



