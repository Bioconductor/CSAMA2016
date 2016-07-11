# Two ideas for team exercises

## Highly replicated yeast dataset

Make sure to remove the
bad replicates detailed in the `Bad_replicate_identification`
directory. Compare different Bioconductor RNA-seq methods using this dataset,
e.g. perform 3 vs 3 comparisons, or 5 vs 5, and evaluate the
sets of DEG are consistent with the held-out samples. Repeat
many times and summarize the plots.

* [paper](http://rnajournal.cshlp.org/content/22/6/839)
* [repository](https://github.com/bartongroup/profDGE48)

## SEQC technical replicates

Count tables for the SEQC project have been added to Bioconductor as a data package
by Yang Liao and Wei Shi. Obtain a set of counts for technical
replicates and assess how many genes for these replicates do the
counts follow something similar to a Poisson. 

* [Bioconductor package](https://bioconductor.org/packages/release/data/experiment/html/seqc.html)

Ideas:

* Plot the variance over the mean of normalized counts
* The samples do not have equal library size. Use estimated size
  factors and row means of the technical replicates to estimate
  a value lambda_ij for each gene and each sample. Calculate the CDF
  for the observed counts given these lambda_ij. Is this distribution
  uniform? What if you stratify by the row mean?

