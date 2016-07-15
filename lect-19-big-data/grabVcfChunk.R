library(Rsamtools)
library(VariantAnnotation)
x = "http://1000genomes.s3.amazonaws.com/release/20110521/ALL.chr8.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz"
tf = TabixFile(x)
p = ScanVcfParam(which=GRanges("8", IRanges(1e7, 1.001e7)))
kk = scanVcfHeader(x)
kk
r = readVcf(tf, "hg19", param=p)
r
