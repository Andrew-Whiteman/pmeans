

## --- contrast ------------------------------------------------------
##
#' @title Construct contrast matrices based on a fitted model object
#'
#' @description
#' Facilitates building linear contrasts for parameters from fitted
#' model objects. Similar in goal to the existing \code{emmeans}
#' package.
#'
#' @param object  A fitted model object
#' @param specs   A \code{formula} specifying parameters of interest
#' @param ...     Any additional arguments
#'
#' @name contrast
#'
setGeneric("contrast",
           function(object, specs, ...)
             standardGeneric("contrast"))


## --- delta ---------------------------------------------------------
setGeneric("delta",
           function(a, b, ...) standardGeneric("delta"))


## --- pmeans --------------------------------------------------------
##
#' @title Predicted outcome means from a fitted model object
#'
setGeneric("pmeans",
           function(mod, con, ...) standardGeneric("pmeans"))


######################################################################
