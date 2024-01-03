#' print.clustsim
#' @inheritParams base::print
#' @export
#' 
print.clustsim <- function(x, ...){
    
    if(!is.null(x$params$udata)){
        n <- x$params$udata$n
        p <- x$params$udata$nind
    }else{
        n <- x$params$n
        p <- x$params$nind
    }
    
    iters <- sprintf("Effective iterations: %s (%s %% failed)", x$niter, x$p_error)
    details <- sprintf("Sample Size: %s, Number of Variables: %s", n, p)
    title <- sprintf("Clustering Simulation with %s", cli::col_blue(x$method))
    nclust <- sprintf("Average number of clusters: %.2f (SD = %.2f)",
                      mean(x$nc), sd(x$nc))
    randindex <- sprintf("Average Adjusted Rand Index: %.2f (SD = %.2f)",
                         mean(x$rand_index), sd(x$rand_index))
    
    if(x$type == "type1"){
        result <- sprintf("Type-1 Error: %.3f (H0 = 1 cluster)", 
                          x$type1)
    }else{
        result <- sprintf("Power: %.3f (H1 = 2 clusters) P(> 2 clusters): %.3f",
                          x$power, x$morethan2)
    }
    
    # Printing
    
    cat("\n")
    cli::rule()
    cli::cli_h1(title)
    cat("\n")
    cat(iters)
    cat("\n")
    cat(details)
    cat("\n")
    cat(result)
    cat("\n")
    cat(nclust)
    cat("\n")
    cat(randindex)
    cat("\n")
    cli::cli_h1("")
    invisible(x)
}

