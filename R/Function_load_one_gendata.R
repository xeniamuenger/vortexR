## Function that loads the loads the additional loci under selection

## collate_gen function ##
# collate_one_gen <- function(project,
#                         populations, 
#                         scenario = NULL, 
#                         dir_in = NULL,
#                         #dec_sep = ".",
#                         save2disk = TRUE,
#                         dir_out = "ProcessedData", 
#                         verbose = TRUE) {
#   if (is.null(scenario)) {
#     fname <- project
#     pat <- paste0(fname, ".gen")
#   } else {
#     fname <- paste(project, scenario, sep = "_")
#     pat <- paste0("^", fname, ".*\\.gen$")
#   }
collate_one_gen <- function(filename,
                            runs, 
                            populations,
                            verbose = FALSE){
  
  lines <- readLines(filename)  ## need to adapt this to read multiple files (I think)

  
  # Blocks of population data start with 'Population' with more than one population, a
  # last block 'Metapopulation' exists, but is ignored
  locLn <- grep(pattern = "^\\$   Locus", lines)
  locN <- length(locLn)
  
  # Locus vector
  loc <- c(2:locN) # skipping locus 1 which is the default and more than 2 alleles
  
  # create data frame of only biallelic loci
  gen_data <- as.data.frame(matrix(NA, nrow=0, ncol=6))
  colnames(gen_data) <- c("Population", "Locus", "meanFrequency_A1", "meanFrequency_A2", 
                          "probPers_A1", "probPers_A2")
  
  # looping over each locus and extracting the information to build the new data.frame
  pop <- populations
  popMeta <- 1
  for (i in  loc) {
    print(i)
    # Locus Number
    Locus <- i
    
    if(pop != 1) {
      popMeta <- pop + 1 # adding meta population to the count
    }
    for (p in 1:popMeta) {
      print(p)
      # mean allele frequencies
      meanFreq <- unlist(strsplit(lines[(3 + 2*popMeta +3 + (i-2)*(2*(popMeta+1)) + p*2)], split = ";"))  # (3 [header] + 2*popMeta [skip locus 1] + 3 [identify line of interest]] + (i-2)*(2*(popMeta+1)) [skip previous loci] + p*2 [skip previous populations])
      # probability of persistence
      probPers <- unlist(strsplit(lines[(3 + 2*popMeta +4 + (i-2)*(2*(popMeta+1)) + p*2)], split = ";"))  # (3 [header] + 2*popMeta [skip locus 1] + 3 [identify line of interest] + (i-2)*(2*(popMeta+1)) [skip previous loci] + p*2 [skip previous populations])
      # putting it all together
      gen_data[nrow(gen_data) + 1,] = c(meanFreq[1], Locus, meanFreq[2],
                                        meanFreq[3], probPers[2], probPers[3])
    }
  }
  gen_data$meanFrequency_A1 <- as.numeric(gen_data$meanFrequency_A1)
  gen_data$meanFrequency_A2  <- as.numeric(gen_data$meanFrequency_A2)
  gen_data$probPers_A1 <- as.numeric(gen_data$probPers_A1)
  gen_data$probPers_A2 <- as.numeric(gen_data$probPers_A2)
  return(gen_data)
}

