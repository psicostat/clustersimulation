#' Summarize a dataframe extracting relevant statistics for the simulation
#' @description
#' The function takes a dataframe and return a list of properties used in the simulation, the `udata` object. In particular, it returns the number of observation, number of indicators,
#' summary statistics, the variance-covariance and correlation matrices.
#' 
#' @param data a dataframe
#'
#' @return a list
#' @export
#'
#' @examples
#' X <- sapply(1:10, function(x) rnorm(100, 0, 1))
#' X <- data.frame(X)
#' prep_user_data(X)
#' 
prep_user_data <- function(data){
    data <- .check_data(data)
    
    # percentage of NA
    n <- nrow(data)
    nNA <- sapply(data, function(x) sum(is.na(x)))
    sumNA <- sprintf("%.0f (%.2f %%)", nNA, (nNA/n)*100)
    
    
    # keep only complete cases
    data <- data[stats::complete.cases(data), ]
    
    # calculate summary stats
    # for mean, sd etc. same as doing na.rm = TRUE
    # for cor, cov, etc. same as doing cor(data, use = "complete.obs")
    
    sums <- get_summary_stats(data)
    VCOV <- .get_vcov(data)
    Sigma <- VCOV$vcov
    
    # renaming sigma to uniform names when generating data
    nn <- paste0("x", 1:ncol(Sigma))
    rownames(Sigma) <- nn
    colnames(Sigma) <- nn
    
    CorT <- VCOV$cor
    mus <- sums["mean", ]
    skews <- sums["skewness", ]
    kurts <- sums["kurtosi", ]
    list(
        n = nrow(data),
        nind = ncol(data),
        mus = unlist(mus),
        Sigma = Sigma,
        CorT = CorT,
        skews = unlist(skews),
        kurts = unlist(kurts),
        sums = sums,
        sumNA = sumNA
    )
}

#' Generate a variance-covariance matrix
#' @description
#' Generate a random variance-covariance matrix sampling the correlation values
#' from a uniform distribution. The function does not guarantee that the matrix
#' is positive definite. The variance is fixed to 1 for simulating standardized data.
#' 
#' @param p number of variables/indicators
#' @param rmin minimum correlation
#' @param rmax maximum correlation. Default to `NULL`. If `NULL` the
#' minimum is used thus using a single value.
#'
#' @return a matrix
#' @export
#'
#' @examples
#' gen_sigma(3, 0.5) # compound symmetry
#' gen_sigma(5, 0.5, 0.7)
gen_sigma <- function(p, rmin, rmax = NULL){
    if(is.null(rmax)) rmax <- rmin
    ncors <- ((p^2) - p) / 2
    rs <- runif(ncors, rmin, rmax)
    S <- 0 + diag(1 - 0, p)
    S[upper.tri(S)] <- rs
    S[lower.tri(S)] <- rs
    return(S)
}

#' Simulate a dataset from a multivariate non-normal distribution
#' @description
#' Simulate a dataset coming from a multivariate non-normal distribution specifying the vector of means, variance-covariance matrix, vector of skewnesses and kurtoses. The function use the [semTools::mvrnonnorm()] function.
#' 
#' @param n number of observations
#' @param d the effect size (i.e., the vector of means of the multivariate distribution). If \code{d} is a vector
#' of length 1, the value is recycled creating a vector of length `nrow(Sigma)`, otherwise \code{d} need to
#' be a vector of length `Sigma`. The function randomize the sign of `d` for each dimension.
#' @param Sigma the variance-covariance matrix
#' @param skew vector of skewnesses
#' @param kurt vector of kurtoses
#'
#' @return a dataframe with simulated data
#' @export
#'
#' @examples
#' Sigma <- gen_sigma(4, 0.5)
#' sim_data(100, 0.5, Sigma)
sim_data <- function(n, d, Sigma, skew = NULL, kurt = NULL){
    nind <- ncol(Sigma)
    
    if(length(d) == 1){
        d <- rep(d, nind)
    }
    
    # randomizing the sign of the effect size
    mu <- d * sample(x = c(-1, 1), size = nind, replace = TRUE)
    data <- semTools::mvrnonnorm(n = n, 
                                 mu = mu, 
                                 Sigma = Sigma, 
                                 skewness = skew, 
                                 kurtosis = kurt)
    data.frame(data)
}

# NULL for the upper bound refer to using the same upper/lower
# thus using a single value


#' Simulate (un)clustered data assuming multivariate non-normal distributions
#' @description
#' Simulate 1 or 2 clusters assuming multivariate non-normal distributions (specifying correlations, skewness, kurtosis, and the number of indicators) and the degree of separation (effect size) in standard deviation units (standardized data).
#' The function can works in two different ways.
#' * When `udata` is `NULL`: need to be specified or left to the default value
#' * When `udata` is provided: parameters are taken from `udata` that is the result of calling the [prep_user_data] function.
#' @param nclust The number of clusters. Can be 1 or 2, default to 1
#' @param n The total number of observations. If \code{nclust} is 1, \code{n} is the number of observations, 
#' if \code{nclust} is 2, \code{n * (1 - pn1)} is the number of observations for the cluster 2.
#' @param dmin the lower bound for the range of effect sizes (in ~Cohen's d like scale).
#' @param dmax the lower bound for the range of effect sizes (in ~Cohen's d like scale). If not specified, \code{dmax} takes the
#' value of \code{dmin} thus the effect size is the same for all indicators.
#' @param rmin the lower bound for the range of correlations between indicators. TODO The correlation is assumed to be the same within each cluster.
#' @param rmax the upper bound for the range of correlations between indicators. If not specified, \code{rmax} takes the value of
#' \code{rmin} thus using a single value.
#' @param nind the number of indicators
#' @param skewmin the lower bound for the range of skewness.
#' @param skewmax the upper bound for the range of skewness. If not specified, \code{skewmax} takes the value of \code{skewmin} thus using a single value.
#' @param kurtmin the lower bound for the range of kurtosi
#' @param kurtmax the upper bound for the range of kurtosi. If not specified, \code{kurtmax} takes the value of \code{kurtmin} thus using a single value.
#' @param pn1 the proportion of observations belonging to the 1 cluster. Default to \code{0.5}.
#' @param debug logical indicating if returning the vectors of true values when sampling from a range.
#' @param udata list of summary statistics to use in the simulation. When specified, previous arguments are ignored.
#'
#' @return a dataframe with the simulated values
#' @export
#'
#' @examples
#' # simulating with parameters
#' X <- sim_clust(nclust = 2, n = 100, dmin = 2, rmin = 0.5, nind = 5)
#' X
#' # with external data
#' udata <- prep_user_data(X)
#' sim_clust(udata = udata)
sim_clust <- function(nclust = 1,
                      n = NULL, 
                      dmin = 0, # dmin default to 0
                      dmax = NULL,
                      rmin = NULL,
                      rmax = NULL,
                      nind = NULL, 
                      skewmin = 0,
                      skewmax = NULL,
                      kurtmin = 0,
                      kurtmax = NULL,
                      pn1 = 0.5,
                      debug = FALSE,
                      udata = NULL){
    
    if(nclust > 2){
        stop("nclust must be 1 (no clusters) or 2")
    }
    
    if(is.null(dmax)) dmax <- dmin
    
    if(is.null(udata)){
        if(is.null(rmin)) rmax <- rmin
        if(is.null(skewmax)) skewmax <- skewmin
        if(is.null(kurtmax)) kurtmax <- kurtmin
        Sigma <- gen_sigma(nind, rmin, rmax)
        skews <- runif(nind, skewmin, skewmax)
        kurts <- runif(nind, kurtmin, kurtmax)
    }else{
        n <- udata$n
        nind <- udata$nind
        Sigma <- udata$CorT # using correlation matrix
        skews <- udata$skews
        kurts <- udata$kurts
    }
    
    ds <- runif(nind, dmin, dmax)
    
    if(nclust == 1){
        n1 <- n
        n2 <- 0
    }else{
        n1 <- round(n * pn1)
    }
    
    sim <- sim_data(n1, ds, Sigma, skews, kurts)
    
    if(nclust == 2){
        n2 <- round(n * (1 - pn1))
        sim0 <- sim_data(n2, d = 0, Sigma, skews, kurts)
        sim <- rbind(sim, sim0)
        sim$group <- rep(1:nclust, c(n1, n2))
    }else{
        sim$group <- rep(1, n1)
    }
    
    names(sim) <- tolower(names(sim))
    
    if(debug){
        pop <- list(
            ds = ds,
            skews = skews,
            kurts = skews,
            Sigma = Sigma
        )
        sim <- list(data = sim, debug = pop)
    }
    
    return(sim)
    
}

#' multiD
#' @description
#' Calculate the multivariate \code{D} based on the Mahalanobis distance as described in Del-Giudice2009-wn. This can be interpret as the
#' distance between the clusters on the n-dimensional space where \code{n} is the number of indicators.
#' 
#' @param d the vector of effect sizes. If \code{length(d)} is 1, the value is used for each dimension (indicator).
#' @param Sigma the variance-covariance matrix
#'
#' @return the Mahalanobis distance
#' @export
#'
#' @examples
#' S <- gen_sigma(4, 0.7)
#' multiD(c(0.4, -0.3, 0.5, 0), S)
multiD <- function(d, Sigma){
    if(length(d) == 1){
        d <- rep(d, nrow(Sigma))
    }
    (sqrt(d %*% solve(Sigma) %*% d))[1]
}

#' get_d_from_D
#' @description
#' Find the \code{d} that when used in all dimensions give the desired \code{D}
#' value. It use the \code{optimize} function using several \code{d} values and
#' minimizing the difference with the desired \code{D}.
#' @param D the desired multivariate \code{D} value
#' @param Sigma the variance-covariance matrix
#'
#' @return the \code{d} value to be used in all dimensions.
#' @export
#'
#' @examples
#' D <- 2
#' S <- gen_sigma(3, 0.4)
#' d <- get_d_from_D(D, S)
#' d
#' multiD(d, S)
get_d_from_D <- function(D, Sigma){
    optimD <- function(d, Sigma, D){
        abs(multiD(d, Sigma) - D)
    }
    optimize(optimD, interval = c(0, 1e5), Sigma = Sigma, D = D)$minimum
}

#' .get_inds
#' @description
#' Get all indicators from a dataset as columns starting with "x" and ending with
#' a number. Only for internal use.
#' 
#' @param data a dataframe
#'
#' @return a dataframe
#' @keywords internal
.get_inds <- function(data){
    idx <- grepl("^x[0-9]+$", names(data))
    data[, idx]
}

#' model_based_clust
#' @description
#' Estimate the number of clusters using the \code{mclust::Mclust()} function.
#' 
#' @param data a dataframe with only the indicators to be used in the clustering
#' as columns.
#' @param criteria which criteria to select the best number of clusters. One of
#' \code{BIC} or \code{lrt} (likelihood ratio test).
#' @param cmin the minimum number of cluster to test. Default to 1
#' @param cmax the maximum number of clusters to test. Default to 5. Be careful that
#' increasing \code{cmax} will greatly increase the computation time.
#' @importFrom mclust Mclust mclustBIC 
#'
#' @return the estimated number of clusters 
#' @export
#'
#' @examples
#' X <- sim_clust(2, 100, dmin = 0.5, rmin = 0.3, nind = 10)
#' X <- X[, 1:(ncol(X) - 1)] # excluding the last column
#' model_based_clust(X)
model_based_clust <- function(data, criteria = "BIC", cmin = 1, cmax = 5){
    criteria <- match.arg(criteria)
    
    res <- mclust::Mclust(data = data, G = cmin:cmax, verbose = FALSE)
    preds <- res$classification
    nclust <- res$G
    
    list(
        nclust = nclust,
        preds = preds
    )
}

#' kmeans_clust
#' @description
#' Estimate the number of clusters using the \code{K-Means clustering} selecting the
#' best solutions using the silhouette value. Before finding the best solutions, the
#' Duda-Hart test is used to check if there are more at least two clusters.
#' 
#' @param data a dataframe with only the indicators to be used in the clustering
#' as columns.
#' @param cmin the minimum number of cluster to test. Default to 2
#' @param cmax the maximum number of clusters to test. Default to 5. Be careful that
#' increasing \code{cmax} will greatly increase the computation time.
#' @param alpha the alpha value for the Duda-Hart test. Default to \code{0.05}
#' @param debug logical indicating if silhouette values for each model should be returned 
#' @return the number of estimated clusters
#' @export
#'
#' @examples
#' X <- sim_clust(2, 100, dmin = 0.5, rmin = 0.3, nind = 10)
#' X <- X[, 1:(ncol(X) - 1)] # excluding the last column
#' kmeans_clust(X)
kmeans_clust <- function(data, cmin = 2, cmax = 5, alpha = 0.05, debug = FALSE){
    # Duda-Hart test
    km2 <- kmeans(data, centers = 2, nstart = 25)
    less_than_2 <- fpc::dudahart2(data, 
                                  clustering = km2$cluster, 
                                  alpha = alpha)$cluster1
    if(less_than_2){
        nclust <- 1
        preds <- km2$cluster
    }else{
        clusts <- 2:cmax
        SS <- rep(NA, length(clusts))
        ddata <- dist(data, method = "euclidean")
        fits <- vector(mode = "list", length = length(clusts))
        for(i in seq_along(clusts)){
            G <- clusts[i]
            km <- kmeans(data, centers = G, nstart = 25)
            ss <- cluster::silhouette(km$cluster, ddata)
            SS[i] <- mean(ss[, 3])
            fits[[i]] <- km
        }
        better <- which.max(SS)
        nclust <- clusts[better]
        preds <- fits[[better]]$cluster
        
    }
    res <- list(nclust = nclust, preds = preds)
    if(debug){
        res$silhouette = data.frame(clusters = clusts, y = SS)
    }
    return(res)
}

#' .check_data
#' @description
#' Performs a series of checks on a dataframe. For internal use. It returns an
#' error if some of the checks are not passed, otherwise return the \code{data}
#' argument.
#' 
#' @param data a dataframe
#'
#' @return a dataframe (in case of no errors)
#' @keywords internal
.check_data <- function(data){
    all_nums <- all(sapply(data, is.numeric))
    if(!all_nums){
        stop("All columns need to be numeric!")
    }
    return(data)
}

#' get_summary_stats
#' @description
#' Compute all relevant summary statistics given a dataframes of all numerical columns.
#' 
#' @param data a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' X <- sim_clust(2, 100, dmin = 0.5, rmin = 0.3, nind = 10)
#' get_summary_stats(X)
get_summary_stats <- function(data){
    funs <- list(
        mean = mean,
        sd = sd,
        min = min,
        max = max,
        skewness = psych::skew,
        kurtosi = psych::kurtosi
    )
    res <- sapply(data, function(col) sapply(funs, function(fun) fun(col)))
    res <- data.frame(res)
    qs <- sapply(data, quartiles)
    res <- rbind(res, qs)
    class(res) <- c(class(res), "sdata")
    return(res)
}

#' .get_vcov
#' @description
#' Compute the variance-covariance and correlation matrix. All columns are assumed to be numerical 
#' and will be included in the matrix.
#' 
#' @param data a dataframe
#'
#' @return a list
#' @keywords internal
#'
.get_vcov <- function(data){
    data <- .check_data(data)
    list(
        vcov = cov(data),
        cor = cor(data),
        vi = sapply(data, var)
    )
}

#' quartiles
#' @description
#' compute the 1st, 2nd (median) and 3rd quartiles.
#' 
#' @param x a numeric vector
#' @param probs the quantiles to be computed. Default to \code{c(0.25, 0.5, 0.75)}.
#'
#' @return a named numeric vector. Quantiles are expressed in percentage.
#' @export
#' @examples
#' x <- rnorm(100)
#' quartiles(x)
quartiles <- function(x, probs = c(0.25, 0.5, 0.75)){
    qs <- quantile(x, probs)
    names(qs) <- paste0("q", probs*100)
    return(qs)
}

#' .within_time
#' @description
#' Internal function to check the simulation status. Every time the function is called
#' compare the current time with the simulation starting time. If the elapsed time is 
#' beyond the maximum time the function return \code{FALSE}, otherwise \code{TRUE}.
#' For internal use.
#' 
#' @param start the starting time. A \code{POSIXct} \code{POSIXt} object returned by
#' calling \code{Sys.time()}.
#' @param maxt the maximum duration in seconds.
#'
#' @return logical
#' @keywords internal
.within_time <- function(start, maxt){
    if(!is.null(maxt)){
        Sys.time() <= (start + maxt)
    }else{
        TRUE
    }
}

#' .get_sim_pars
#' @description
#' Capture simulation parameters as function arguments from \code{...} or \code{udata} when
#' using the \code{run_simulation} function. For internal use.
#' 
#' @param sargs list of simulation parameters.
#' @keywords internal
#' @return a list
#'
.get_sim_pars <- function(sargs){
    fargs <- formals(sim_clust)
    eargs <- fargs[!names(fargs) %in% names(sargs)]
    sargs <- c(sargs, eargs)
    sargs[!names(sargs) == "..."]
}

#' run_simulation
#' @description
#' Function to run the simulation for estimating the type-1 error or the statistical power
#' given a set of parameters.
#' 
#' @param niter number of iterations.
#' @param maxt the maximum time. If the number of iterations is not finished within the maximum time the simulations ends.
#' @param type character indicating the simulation type. One of \code{type1} or \code{power}. 
#' @param method character indicating the method to use for estimating the number of cluster. 
#' One of \code{mclust} or \code{kmeans}. \code{mclust} will call the \code{model_based_clust()} function and \code{kmeans}
#' the \code{kmeans_clust()} function.
#' @param cmax the maximum number of clusters to test. Default to 5. Be careful that
#' increasing \code{cmax} will significantly increase the computation time.
#' @param dmin the lower bound for the range of effect sizes (in ~Cohen's d like scale).
#' @param dmax the lower bound for the range of effect sizes (in ~Cohen's d like scale). If not specified, \code{dmax} takes the
#' value of \code{dmin} thus the effect size is the same for all indicators.
#' @param udata a list. The output of the \code{prep_user_data()} function. Default to \code{NULL}. If \code{udata} is not
#' \code{NULL} all the parameters passed to \code{...} are ignored.
#' @param progress logical indicating whether a progress bar (using the \code{progress} package) should be displayed.
#' @param ... named arguments that will be passed to the \code{sim_clust()} function. If \code{udata} is \code{null}, these
#' parameters are used for the simulation.
#' @param updateProgress a function used fo displaying a progress bar in the Shiny app. Default to \code{NULL}
#' @return a list containing the estimated power or type-1 error, the number of effective iterations, the proportion of 
#' iterations with errors, a vector with the estimated number of clusters and a list with all simulation parameters.
#' @export
#'
#' @examples
#' \dontrun{
#' run_simulation(niter = 100, maxt = 50, type = "type1", method = "mclust", 
#' nclust = 2, n = 100, dmin = 0.5, rmin = 0.5, nind = 3)
#' }

run_simulation <- function(niter,
                           maxt = NULL,
                           type = c("power", "type1"),
                           method = c("mclust", "kmeans"),
                           cmax = 5,
                           dmin = 0, # default to 0 (no effect)
                           dmax = NULL,
                           udata = NULL,
                           progress = TRUE,
                           updateProgress = NULL,
                           ...){
    dots <- list(...)
    
    if(is.null(dmax)) dmax <- dmin
    
    if(is.null(udata)){
        sargs <- dots
    }else{
        sargs <- list(udata = udata)
    }
    
    method <- match.arg(method)
    type <- match.arg(type)
    
    if(type == "type1"){
        if(dmin != 0){
            warning("when type = type1 the argument dmin is fixed to 0, ignoring the inserted value!")
        }
        sargs$nclust <- 1
        sargs$dmin <- 0 # fixing to 0, still ignored in sim_clust()
    }else{
        if(dmin == 0){
            warning("when type = power the result is not meaningful using dmin = 0")
        }
        sargs$nclust <- 2
        sargs$dmin <- dmin
        sargs$dmax <- dmax
    }
    
    clust_fun <- switch(method,
                        "mclust" = .safe_wrappers()$model_based_clust,
                        "kmeans" = .safe_wrappers()$kmeans_clust)
    
    nc <- rep(NA, niter)
    rand_index <- rep(NA, niter) # vector of rand indexes
    m_rand_index <- NA # average rand index
    type1 <- 0
    power <- 0
    typeM <- 0
    is_error <- rep(NA, niter)
    
    # Timing Setup
    
    start <- Sys.time()
    
    # setup the progress bar steps when maxt is used, otherwise the progress is based on the iterations
    if(!is.null(maxt)){
        tunit <- maxt / 10
        pmaxt <- start + maxt
        steps <- seq(start, pmaxt, tunit)
    }
    
    if(progress){
        pb <- progress::progress_bar$new(total = niter)
    }
    
    for(i in 1:niter){
        if(!.within_time(start, maxt)){
            break
        }
        sim <- do.call(.safe_wrappers()$sim_clust, sargs)
        sim_inds <- .get_inds(sim) # data without group
        res <- clust_fun(sim_inds, cmax = cmax)
        nc[i] <- res$nclust
        
        # rand index
        truth <- sim$group
        preds <- res$preds
        rand_index[i] <- .safe_wrappers()$rand_index(truth, preds)
        m_rand_index <- mean(rand_index, na.rm = TRUE)
        
        # computing both type1 and power, returning only the relevant one
        ## -- type = "type1" (sargs$nclust == 1)
        #       - type1: proportions of wrong conclusions when H0 is true
        #       - power: not meaningful
        ## -- type = "power" (sargs$nclust == 2)
        #       - type1: proportions of wrong conclusions similar to type-M error (more clusters but H1 true)
        #       - power: proportions of correct conclusions (finding the correct number of clusters when H1 true)
        
        type1 <- mean(nc > sargs$nclust, na.rm = TRUE)
        power <- mean(nc == sargs$nclust, na.rm = TRUE)
        
        # Check Errors
        is_error[i] <- all(is.na(sim)) | all(is.na(res))
        
        if(progress){
            pb$tick()
        }
        if (is.function(updateProgress)) {
            text <- "I am working for you! %s iterations!"
            idx <- findInterval(Sys.time(), steps) - 1
            updateProgress(idx/10, detail = sprintf(text, i))
        }
    }
    
    end <- Sys.time()
    
    sargs <- .get_sim_pars(sargs)
    
    sim_res <- list(
        niter = i,
        nc = nc,
        rand_index = rand_index,
        m_rand_index = m_rand_index,
        type1 = type1,
        power = power,
        morethan2 = mean(nc > 2, na.rm = TRUE),
        p_error = mean(is_error),
        params = sargs,
        type = type,
        method = method,
        elapsed = as.numeric(end - start)
    )
    
    if(type == "type1"){
        sim_res$power <- NA
    }else{
        sim_res$type1 <- NA
    }
    
    # uniform the list structure when using udata
    if(!is.null(udata)){
        sim_res <- .udata_as_params(sim_res)
    }
    
    class(sim_res) <- "clustsim"
    
    return(sim_res)
}

#' .flatten_udata
#' @description
#' Flatten the \code{params} list when using \code{run_simulation()} with \code{udata}
#' 
#' @param x a list
#'
#' @return the flattened version of the list without the \code{udata.} prefix
#' @keywords internal
.flatten_udata <- function(x){
    x <- unlist(x, recursive = FALSE)
    names(x) <- gsub("udata.", "", names(x))
    return(x)
}

#' .udata_as_params
#' @description
#' Uniform the simulation result structure with and without \code{udata}
#' 
#' @param x a list
#'
#' @return a list
#' @keywords internal
.udata_as_params <- function(x){
    udata_as_params <- x$params$udata[names(x$params)]
    udata_as_params <- udata_as_params[!sapply(udata_as_params, is.null)]
    x$params[names(udata_as_params)] <- udata_as_params
    x$params$skewmin <- min(x$params$udata$skews)
    x$params$skewmax <- max(x$params$udata$skews)
    x$params$kurtmin <- min(x$params$udata$kurts)
    x$params$kurtmax <- max(x$params$udata$kurts)
    x$params$rmin <- min(x$params$udata$CorT[upper.tri(x$params$udata$CorT)])
    x$params$rmax <- max(x$params$udata$CorT[upper.tri(x$params$udata$CorT)])
    return(x)
}

#' check_sigma
#' @description
#' Check how many times Sigma produce error when simulating data
#' 
#' @param nsim number of simulations
#' @param n sample size
#' @param Sigma the variance-covariance matrix
#' @param skew skewness
#' @param kurt kurtosis
#' @keywords internal
#'
.check_sigma <- function(nsim = 1000, 
                        n = 100, 
                        Sigma, 
                        skew = 0, 
                        kurt = 0){
    ssim_data <- purrr::possibly(sim_data, otherwise = NA)
    res <- replicate(nsim, {
        ssim_data(n = n, d = 0, Sigma = Sigma, skew = skew, kurt = kurt)
    })
    mean(is.na(res))
}