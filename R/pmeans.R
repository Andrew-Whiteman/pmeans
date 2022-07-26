

setMethod(
  "pmeans",
  signature(mod = "lmerMod", con = "contrast"),
  function(mod, con) {
    v <- lme4::vcov.merMod(mod)
    pm <- c( con@.Data %*% fixef(mod) )
    se <- numeric( length(pm) )
    for ( i in 1:length(pm) ) {
      x <- con@.Data[i,, drop = FALSE] %*% v %*% con@.Data[i,]
      se[i] <- sqrt( c(as.matrix(x)) )
    }
    z <- pm / se
    res <- cbind(
      "Estimate"   = pm,
      "Std. Error" = se,
      "z value"    = z,
      "Pr(>|z|)"   = 2 * stats::pnorm(-abs(z))
    )
    rownames(res) <- names(con)
    res
  })
