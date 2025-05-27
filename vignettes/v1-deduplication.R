## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(blocking)
library(data.table)

## -----------------------------------------------------------------------------
data(RLdata500)
head(RLdata500)

## -----------------------------------------------------------------------------
RLdata500[, id_count :=.N, ent_id] ## how many times given unit occurs
RLdata500[is.na(fname_c2), fname_c2:=""]
RLdata500[is.na(lname_c2), lname_c2:=""]
RLdata500[, bm:=sprintf("%02d", bm)] ## add leading zeros to month
RLdata500[, bd:=sprintf("%02d", bd)] ## add leading zeros to month
RLdata500[, txt:=tolower(paste0(fname_c1,fname_c2,lname_c1,lname_c2,by,bm,bd))]
head(RLdata500)

## -----------------------------------------------------------------------------
set.seed(2024)
df_blocks <- blocking(x = RLdata500$txt, ann = "nnd", verbose = 1, graph = TRUE)

## -----------------------------------------------------------------------------
df_blocks

## -----------------------------------------------------------------------------
str(df_blocks,1)

## -----------------------------------------------------------------------------
plot(df_blocks$graph, vertex.size=1, vertex.label = NA)

## -----------------------------------------------------------------------------
head(df_blocks$result)

## -----------------------------------------------------------------------------
df_block_melted <- melt(df_blocks$result, id.vars = c("block", "dist"))
df_block_melted_rec_block <- unique(df_block_melted[, .(rec_id=value, block)])
head(df_block_melted_rec_block)

## -----------------------------------------------------------------------------
RLdata500[df_block_melted_rec_block, on = "rec_id", block_id := i.block]
head(RLdata500)

## -----------------------------------------------------------------------------
RLdata500[, .(uniq_blocks = uniqueN(block_id)), .(ent_id)][, .N, uniq_blocks]

## -----------------------------------------------------------------------------
hist(df_blocks$result$dist, xlab = "Distances", ylab = "Frequency", breaks = "fd",
     main = "Distances calculated between units")

## -----------------------------------------------------------------------------
df_for_density <- copy(df_block_melted[block %in% RLdata500$block_id])
df_for_density[, match:= block %in% RLdata500[id_count == 2]$block_id]

plot(density(df_for_density[match==FALSE]$dist), col = "blue", xlim = c(0, 0.8), 
     main = "Distribution of distances between\nclusters type (match=red, non-match=blue)")
lines(density(df_for_density[match==TRUE]$dist), col = "red", xlim = c(0, 0.8))

