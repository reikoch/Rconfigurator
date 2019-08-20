# hardcoded set of base R packages
base_packages <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel",
                   "splines", "stats", "stats4", "tcltk", "tools", "utils")

# check existence of package versions on CRAN
cranurl <- function(pkg, version) {
  url <- paste0('https://cloud.r-project.org/src/contrib/', pkg, '_', version, '.tar.gz')
  if (httr::HEAD(url)$status_code == 200) url else {
    url <- paste0('https://cloud.r-project.org/src/contrib/Archive/', pkg, '/', pkg, '_', version, '.tar.gz')
    if (httr::HEAD(url)$status_code == 200) url else NA_character_
  }
}


#' check consistency of package versions
#'
#' @param R_version string indicating desired R version 
#' @param packages character vector of desired package names 
#' @param versions character vector of desired package versions
#'
#' @return
#'     A logical indicating consistence and completeness of submitted package versions.
#' @export
#'
#' @examples
#'     consistent('3.1.2', 'Rcpp', '1.0.2')
consistent <- function(R_version, packages, versions) {
  R_url <- paste0('https://cloud.r-project.org/src/base/R-3/R-', R_version, '.tar.gz')
  if (httr::HEAD(R_url)$status_code != 200) {
    stop('R version ', R_version, ' could not be found under https://cloud.r-project.org/src/base/R-3/\n')
  }
  ##TODO: check existence of R_version R    https://cloud.r-project.org/src/base/R-3/R-3.1.2.tar.gz
  # download the indicated package versions
  tdir <- tempfile(pattern = 'tdir')
  dir.create(tdir)
  urls <- mapply(cranurl, packages, versions)
  if (any(is.na(urls))) {
    names(urls) <- paste(packages, versions, '\n')
    stop('Some package versions could not be found on CRAN:\n', names(urls[is.na(urls)]))
  }
  downloads <- mapply(curl::curl_download, urls, file.path(tdir, basename(urls)))
  # extract DESCRIPTION files and clear download area
  rc <- lapply(downloads, utils::untar, files='*/DESCRIPTION', exdir=tdir)
  deps <- lapply(dir(list.dirs(tdir, recursive=FALSE), pattern = 'DESCRIPTION', recursive = FALSE, full.names = TRUE),
               function(X) desc::desc(X)$get_deps()
               )
  unlink(tdir, recursive = TRUE)
  # build dependency dataframe
  dependencies <- unique(do.call(rbind, deps))
  dependencies <- dependencies[! dependencies$package %in% base_packages,]
  op_ver <- strsplit(dependencies$version, '\\s+')
  dependencies$op <- unlist(lapply(op_ver, function(X) if(X[[1]]=='*') '>' else X[[1]]))
  dependencies$ver <- unlist(lapply(op_ver, function(X) X[[length(X)]]))
  
  # requested package set
  reqs <- data.frame(package=c('R',packages), version=c(R_version, versions), stringsAsFactors = FALSE)
  conditions <- merge(reqs, dependencies[dependencies$type %in% c('Depends', 'Imports', 'LinkinTo'), 
                                         c('package', 'op', 'ver')
                                         ],
                      by='package', all.y = TRUE
                      )
  rc <- ifelse(conditions$ver=='*',
               is.package_version(conditions$version),
               eval(parse(text = paste0('package_version(conditions$version, strict=FALSE)', 
                                        conditions$op,
                                        'package_version(conditions$ver, strict=FALSE)'
               )))
  )
  names(rc) <- conditions$package
  rc
}