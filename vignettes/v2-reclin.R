## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(blocking)
library(data.table)

## -----------------------------------------------------------------------------
data(census)
data(cis)

## -----------------------------------------------------------------------------
head(census)

## -----------------------------------------------------------------------------
head(cis)

## -----------------------------------------------------------------------------
census[is.na(DOB_DAY), DOB_DAY := ""]
census[is.na(DOB_MON), DOB_MON := ""]
census[is.na(DOB_YEAR), DOB_YEAR := ""]
cis[is.na(DOB_DAY), DOB_DAY := ""]
cis[is.na(DOB_MON), DOB_MON := ""]
cis[is.na(DOB_YEAR), DOB_YEAR := ""]
census[, txt:=paste0(PERNAME1, PERNAME2, SEX, DOB_DAY, DOB_MON, DOB_YEAR, ENUMCAP, ENUMPC)]
cis[, txt:=paste0(PERNAME1, PERNAME2, SEX, DOB_DAY, DOB_MON, DOB_YEAR, ENUMCAP, ENUMPC)]

## -----------------------------------------------------------------------------
set.seed(2024)
result1 <- blocking(x = census$txt, y = cis$txt, verbose = 1)

## -----------------------------------------------------------------------------
hist(result1$result$dist, main = "Distribution of distances between pairs", xlab = "Distances")

## -----------------------------------------------------------------------------
head(result1$result, n= 10)

## -----------------------------------------------------------------------------
cbind(t(census[1, 1:9]), t(cis[8152, 1:9]))

## -----------------------------------------------------------------------------
matches <- merge(x = census[, .(x=1:.N, PERSON_ID)],
                 y = cis[, .(y = 1:.N, PERSON_ID)],
                 by = "PERSON_ID")
matches[, block:=1:.N]
head(matches)

## -----------------------------------------------------------------------------
set.seed(2024)
result2 <- blocking(x = census$txt, y = cis$txt, verbose = 1,
                    true_blocks = matches[, .(x, y, block)])

## -----------------------------------------------------------------------------
result2

## -----------------------------------------------------------------------------
set.seed(2024)
ann_control_pars <- controls_ann()
ann_control_pars$nnd$epsilon <- 0.2

result3 <- blocking(x = census$txt, y = cis$txt, verbose = 1, 
                    true_blocks = matches[, .(x, y, block)], 
                    control_ann = ann_control_pars)

## -----------------------------------------------------------------------------
result3

