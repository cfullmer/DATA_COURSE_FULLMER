#splitting and pairing fastq files

?fastqFilter
?fastq_pair
library(dada2)
library(ShortRead)

getwd()
path <-file.path(getwd(),"/fastq")
filtpath<- file.path(path,"filtered")
if(!file_test("-d", filtpath)) dir.create(filtpath) # make directory for filtered fqs if not already present
fns <- list.files(path,full.names = TRUE,include.dirs = FALSE)
fastqs <- fns[grepl(".fastq$", fns)]


plotQualityProfile(fns)

?list.files
