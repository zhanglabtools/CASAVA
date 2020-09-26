# last time modified: 2020/9/26
# This script could be used to annotate a large number of variants using the 
# whole-genome CASAVA scores. 
#
# Any question please contact cz@amss.ac.cn, we will try to respond ASAP.
#
# Please following the steps to get CASAVA scores:
# 1) Download CASAVA scores (RData files) and put them into a folder (CASAVAPath).
# 2) Prepare the input file so that the first and second columns represent
#    chromosomes and their locations of genetics variants in hg19.
# 3) Change the name of input and output below.
# 4) Run this script

# Output:
# A dataframe containing the chromosome, location, and CASAVA scores of
# corresponding input variants. If the outputName is not null, then a CSV file 
# will be saved for further use.
################################################################################
require(data.table)
CASAVAScore <- function(inputName, outputName, CASAVAPath, verbose = T) {
  
  bedFile <- data.table::fread(inputName, stringsAsFactors = FALSE)
  
  chromosome <- bedFile[[1]]
  chromosome <- tolower(chromosome)
  if (!any(startsWith(chromosome, 'chr'))) chromosome <- paste0('chr', chromosome)
  
    
  location <- bedFile[[2]]
  location <- as.numeric(location)
  location <- ceiling(location / 200) # CASAVA score is in 200-bp resolution and compressed
  
  result <- matrix(NA, nrow = nrow(bedFile), ncol = 24)
  for (chr in sprintf('chr%s', c(1:22, 'x'))) {
    fileName <- paste0(CASAVAPath, chr, '.RData')
    load(fileName)  # casavaScore
    index <- chromosome == chr
    result[index, ] <- casavaScore[location[index], ] 
    if (verbose) print(paste0("Finish processing chromosome: ", chr))
  }
  result <- data.frame(chromosome, location = bedFile[[2]], result, stringsAsFactors = F)
  colnames(result) <- c('chromosome', 'location', colnames(casavaScore))
  
  if (!is.null(outputName)) {
    result <- as.data.frame(result)
    data.table::fwrite(result, file = outputName)
  } 
  return(result)
}
################################################################################
# change the configuration according to your own and automatically get the result
CASAVAPath <- 'K:/data/CASAVA/'
inputName <- 'K:/project-four/TssCombine/GenerateSnp/data/variants/test_2.csv'
outputName <- 'K:/temp.csv'

result <- CASAVAScore(inputName, outputName, CASAVAPath)
