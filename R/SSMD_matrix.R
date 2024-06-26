#' Generate a SSMD matrix with all possible pairwise comparisons
#'
#' \code{SSMD_matrix} conducts pairwise comparisons for all possible pairs
#' using strictly standardised mean difference (SSDM, Zhang 2007).
#'
#' When \code{yrs='max'} (default), VortexR automatically sets  \code{yrs} to
#' the last year of the simulation .
#'
#' @param dir_out The local path to store the output. Default: DataAnalysis/SSMD_matrix
#' @inheritParams pairwise
#' @return A list where each element is a matrix of SSMD (belowe the diagonal)
#'         and related p-values (above the diagonal) for each combination of 'yrs',
#'         population and 'params'
#' @references Zhang, X. D. 2007. A pair of new statistical parameters for quality control
#' in RNA interference high-throughput screening assays. Genomics 89:552-561.
#'
#' @import data.table
#' @export
#' @examples
#' # Using Campbell et al. and Pacioni et al. example data.
#' # See ?pacioni and ?campbell for more details on example data.
#' require(vortexRdata)
#'
#' data("pac.clas")
#'
#' SSMD_matrix(data=pac.clas, project="Pacioni_et_al",
#' scenario="ST_Classic",
#' params = c("PExtinct", "Nextant", "Het", "Nalleles"),
#' yrs = c(60, 120), ST = FALSE, save2disk = FALSE)
#'
#' data(sta.main)
#' ssmd_mat <- SSMD_matrix(data=sta.main, project="test",
#'                        scenario="test",
#'                        params = c("PExtant", "Nextant"),
#'                        yrs = c(25, 50), ST = FALSE, save2disk = FALSE)
SSMD_matrix <- function(data,
                        project,
                        scenario,
                        params=c("PExtinct", "Nextant", "Het", "Nalleles"),
                        yrs="max",
						plotpops = c("Nall"),					 
                        ST=FALSE,
                        save2disk=TRUE,
                        dir_out="DataAnalysis/SSMD_matrix") {
    # Dealing with no visible global variables
    Year <- NULL
    scen.name <- NULL
    J <- NULL

    data <- data.table(data)

    # Function definitions
    SEname <- function(par) paste("SE.", par, ".", sep="")
    SDname <- function(parSD) paste("SD.", parSD, ".", sep="")
    # Error handling
    if(is.character(yrs)) {
        if(length(yrs) == 1) {
            if(yrs != "max") stop("invalid value(s) for 'yrs' ")
        } else {
            stop("invalid value(s) for 'yrs' ")
        }
    } else {
        if(!is.numeric(yrs)) stop("invalid value(s) for 'yrs' ")
    }

    fname <- if (ST) {paste(project, scenario, sep="_")} else {project}

    # set yrs to max
    if(is.character(yrs))
        {yrs <- data[, max(Year)]}
    # Set up headings for params and SE and SD
    params <- make.names(params)
    SE <- sapply(params, SEname)
    if ("stoch.r" %in% params) {SE["stoch.r"] <- "SE.r."}

    SD <- sapply(params, SDname)
    if ("stoch.r" %in% params) {SD["stoch.r"] <- "SD.r."}

    data$pop.name <- as.character(data$pop.name) # to select certain populations they need to be character instead of factor
    data$pop.name <- str_trim(data$pop.name, "left")  # random whitespace inserted in string, to remove																														 																								 
    if(plotpops == "Nall") {
	    pops.name <- unique(data$pop.name)
    } else pops.name <- plotpops
	
    results <- vector("list", length(pops.name) * length(params) * length(yrs))

    df_nms <- expand.grid(pops.name, params, yrs)
    nms <- paste(df_nms[, 1], df_nms[, 2], df_nms[, 3])
    names(results) <- nms

    for (popName in pops.name) {
        for (param in params) {
            for(yr in yrs)  {
                data.table::setkeyv(data, c("Year", "pop.name"))
                substdat <- data[J(yr, popName), ]
                data.table::setkey(substdat, scen.name)
                sottra <- outer(substdat[[param]], substdat[[param]], "-")
                sumsq <- outer(substdat[[SD[param]]]^2,
                             substdat[[SD[param]]]^2, "+")

                triang_matrix <- sottra / sqrt(sumsq)
                pval_matrix <- vortexR::pval(triang_matrix)
                triang_matrix[upper.tri(triang_matrix)] <-
                    pval_matrix[upper.tri(pval_matrix)]
                colnames(triang_matrix) <- substdat$scen.name
                rownames(triang_matrix) <- substdat$scen.name

                results[[paste(popName, param, yr)]] <- triang_matrix

                if (save2disk) {
                    df2disk(triang_matrix, dir_out, fname, row_names=TRUE,
                            postfix=paste(popName, param, yr, sep="_"))
                }
            }

        }
    }
return(results)
}
