## ----style, echo = FALSE, results = 'asis'--------------------------------------------------------
BiocStyle::markdown()
options(width=100, max.print=1000)

## ----setup, echo=FALSE, messages=FALSE, warnings=FALSE--------------------------------------------
knitr::opts_chunk$set(cache=TRUE)
suppressPackageStartupMessages({
    library(Biostrings)
    library(GenomicRanges)
})

