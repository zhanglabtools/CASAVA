# last time modified: 2020/12/26
# This script could be used to annotate a large number of variants using the 
# whole-genome CASAVA scores. 
#
# Any question please contact cz@amss.ac.cn, we will try to respond as soon as .
#
# You could follow the steps to get CASAVA scores:
# 1) Download CASAVA scores (RData files) and put them into a folder (CASAVAPath).
# https://github.com/zhanglabtools/CASAVA/releases/tag/CASAVA

# 2) Prepare the input file so that the first and second columns represent
#    chromosomes and their locations of genetics variants in hg19.

# 3) Change the name of input and output files below.

# 4) Run this script

# Output:
# A dataframe containing the chromosome, location, and CASAVA scores of
# corresponding input variants. If the outputName is not null, then a CSV file 
# will be saved for further use.
################################################################################
# Here is the process of getting CASAVA score. The only dependency is data.table
# package to speed-up the IO.
require(data.table)
CASAVAScore <- function(CASAVAPath, inputName, outputName = NULL, verbose = T) {
  
  bedFile <- data.table::fread(inputName, stringsAsFactors = FALSE)
  
  chromosome <- bedFile[[1]]
  chromosome <- tolower(chromosome)
  if (!any(startsWith(chromosome, 'chr'))) chromosome <- paste0('chr', chromosome)
    
  # CASAVA score is in 200-bp resolution and compressed
  location <- bedFile[[2]]
  location <- as.numeric(location)
  location <- ceiling(location / 200)
  
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
# Here is how to get CASAVA score.

# 1) change the configuration according to your own.
CASAVAPath <- 'K:/data/CASAVA/'  # The path where you locate the downloaded files
inputName <- 'K:/input.csv'  # The location of input file.
outputName <- 'K:/output.csv'  # The location of input file.

# 2) When outputName is not NULL, write the results out.
result <- CASAVAScore(CASAVAPath, inputName, outputName)

# 3) When outputName is not NULL, only get the results in current R envorinment.
# result <- CASAVAScore(CASAVAPath, inputName)

# 4) Check the results
print(colnames(result))
print(head(result, n = 5))