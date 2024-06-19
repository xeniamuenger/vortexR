
collate_gen <- function(project,
                            populations, 
                            scenario = NULL, 
                            dir_in = NULL,
			    runs = NULL, 
                            #dec_sep = ".",
                            save2disk = TRUE,
                            dir_out = "ProcessedData", 
                            verbose = TRUE) {
  if (is.null(scenario)) {
    fname <- project
    pat <- paste0(fname, ".gen")
  } else {
    fname <- paste(project, scenario, sep = "_")
    pat <- paste0("^", fname, ".*\\.gen$")
  }
  
  if (is.null(dir_in)) dir_in <- getwd()
  setwd(dir_in)
  
  #lines <- readLines(pat)  ## need to adapt this to read multiple files (I think)
    
  files <- get_file_paths(path = dir_in,
                          pattern = pat,
                          fn_name = "collate_gen",
                          fname = fname,
                          verbose = verbose)
  
  d <- data.frame()
  if (verbose) message("vortexR::collate_dat is parsing:")
  scen <- 0
  for (filename in files) {
    if (verbose) message(filename, "\r")
    scen <- scen + 1
    one_gen <- collate_one_gen(filename, runs, populations)
    one_gen$scenario <- scen
    d <- rbind(d, one_gen)
  }
  if (save2disk) df2disk(d, dir_out, fname, "_data")
  return(d)
}