library("plyr")
library("MASS")

month <- ordered(rep(1:12, length = 72))
deseasf <- function(value) rlm(value ~ month - 1)


array_loops <- system.time({
  models <- as.list(rep(NA, 24 * 24))
  dim(models) <- c(24, 24)

  deseas <- array(NA, c(24, 24, 72))
  dimnames(deseas) <- dimnames(ozone)

  for (i in seq_len(24)) {
    for(j in seq_len(24)) {
      mod <- deseasf(ozone[i, j, ])

      models[[i, j]] <- mod
      deseas[i, j, ] <- resid(mod)
    }
  }
})



array_apply <- system.time({
  models <- apply(ozone, 1:2, deseasf)

  resids <- unlist(lapply(models, resid))
  dim(resids) <- c(72, 24, 24)
  deseas <- aperm(resids, c(2, 3, 1))
  dimnames(deseas) <- dimnames(ozone)
})


array_plyr <- system.time({
  models <- aaply(ozone, 1:2, deseasf)
  deseas <- aaply(models, 1:2, resid)
})

array <- cbind(array_loops, array_apply, array_plyr)

library("reshape")
ozonedf <- melt(ozone)
deseasf_df <- function(df) {
  rlm(value ~ month - 1, data = df)
}

df_apply <- system.time({
  pieces <- split(ozonedf, list(ozonedf$lat, ozonedf$long))
  models <- lapply(pieces, deseasf_df)

  results <- mapply(function(model, df) {
    cbind(df[rep(1, 72), c("lat", "long")], resid(model))
  }, models, pieces)
  deseasdf <- do.call("rbind", results)
})

df_plyr <- system.time({
  models <- dlply(ozonedf, .(lat, long), deseasf_df)
  deseas <- ldply(models, resid)
})

df <- cbind(df_apply, df_plyr)
