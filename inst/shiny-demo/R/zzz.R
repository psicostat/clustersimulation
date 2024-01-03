#' @importFrom stats cor cov dist kmeans optimize quantile runif sd var
NULL

.onAttach <- function(libname, pkgname) {
    msg <- sprintf("Welcome in the %s package (version: %s)", 
                   cli::col_blue(pkgname), 
                   utils::packageVersion(pkgname))
    packageStartupMessage(cli::rule(line = 2))
    packageStartupMessage(msg)
    packageStartupMessage(cli::rule(line = 2))
}