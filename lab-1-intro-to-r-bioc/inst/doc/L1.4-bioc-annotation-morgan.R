## ----style, echo = FALSE, results = 'asis'-------------------------------
BiocStyle::markdown()

## ----setup, echo=FALSE, warning=FALSE-------------------------------------------------------------
options(max.print=1000, width=100)
knitr::opts_chunk$set(cache=TRUE)
suppressPackageStartupMessages({
    library(org.Hs.eg.db)
    library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    library(BSgenome.Hsapiens.UCSC.hg19)
    library(GenomicRanges)
    library(biomaRt)
    library(rtracklayer)
    library(Gviz)
    library(AnnotationHub)
})

