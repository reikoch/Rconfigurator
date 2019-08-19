 #' create Dockerfile for desired R configuration
#'
#' @param R_version character vector of desired R version 
#' @param packages character vector of desired package names 
#' @param versions character vector of desired package versions
#'
#' @return Dockerfile based on debian:stretch for desired R configuration 
#' @export
#'
#' @examples
#'     rockerfile('3.1.2', 'Rcpp', '1.0.2')
rockerfile <- function(R_version, packages, versions) {
  # check consistency and completeness of R
  rc <- consistent(R_version, packages, versions)
  if (all(rc)) {
    # create Dockerfile similar to rocker/r-ver
    dockerfile <- paste0(readr::read_file(system.file('Dockerfile1', package = 'Rconfigurator')),
                         R_version,
                         readr::read_file(system.file('Dockerfile2', package = 'Rconfigurator')),
                         paste('  && /tmp/dwnld', packages, versions, '\\', collapse='\n'),
                         readr::read_file(system.file('Dockerfile.end', package = 'Rconfigurator'))
    )
    dockerfile <- gsub('\r\n', '\n', dockerfile)   # important on Windows
  } else {
    dockerfile <- NULL
    message('Problem: the following packages were not included in the package list,\n',
            'at least not in a conforming version:\n', paste(names(rc[!rc]), '\n'))
  }
  dockerfile
}