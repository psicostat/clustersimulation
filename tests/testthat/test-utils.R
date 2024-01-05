source("settings.R") # loading objects

# .tol objects contains the tolerance values for numeric comparison
# similarly to https://github.com/cran/metafor/blob/master/tests/testthat/settings.r

test_that("prep_user_data works referencing to mtcars", {
    actual <- prep_user_data(mtcars)
    
    dat <- mtcars
    n <- nrow(dat)
    nind <- ncol(dat)
    dat <- .check_data(dat)
    VCOV <- .get_vcov(dat)
    Sigma <- VCOV$vcov
    CorT <- VCOV$cor
    mus <- apply(dat, 2, mean)
    skews <- psych::skew(dat)
    kurts <- psych::kurtosi(dat)
    
    expected <- list(
        n = n,
        nind = nind,
        mus = mus,
        Sigma = Sigma,
        CorT = CorT,
        skews = skews,
        kurts = kurts
    )
    
    actual <- actual[names(actual) %in% names(expected)]
    
    expect_equal(
        actual,
        expected,
        ignore_attr = TRUE
    )
})

test_that("get_summary_stat() works with mtcars", {
    dat <- mtcars
    means <- apply(dat, 2, mean)
    sds <- apply(dat, 2, sd)
    mins <- apply(dat, 2, min)
    maxs <- apply(dat, 2, max)
    skews <- apply(dat, 2, psych::skew)
    kurts <- apply(dat, 2, psych::kurtosi)
    q25s <- apply(dat, 2, quantile, 0.25)
    q50s <- apply(dat, 2, quantile, 0.50)
    q75s <- apply(dat, 2, quantile, 0.75)
    
    actual <- list(mean = means, sd = sds, min = mins, max = maxs, skew = skews, kurt = kurts, 
                   q25 = q25s, q50 = q50s, q75 = q75s)
    actual <- data.frame(do.call(rbind, actual))
    expected <- get_summary_stats(dat)
    
    expect_equal(actual, get_summary_stats(dat), ignore_attr = TRUE)
})

test_that("gen_sigma() works with r = 0", {
    r <- 0
    p <- 5 # number of variables
    S <- r + diag(1 - r, 5)
    expect_equal(S, gen_sigma(p, r))
})

test_that("gen_sigma() works with r = 0.5", {
    r <- 0.5
    p <- 5 # number of variables
    S <- r + diag(1 - r, 5)
    expect_equal(S, gen_sigma(p, r))
})

test_that("sim_data() works for no skew and kurt", {
    # truth
    mu <- c(0, 0.5, 1)
    r <- 0.7
    p <- 3
    S <- gen_sigma(p, r)
    X <- sim_data(1e6, mu, S)
    
    muhat <- apply(X, 2, mean)
    Shat <- var(X)
    
    expect_equal(S, Shat, label = "vcov", tolerance = .tol["gen"], ignore_attr = TRUE)
    expect_equal(abs(muhat), mu, label = "means", tolerance = .tol["gen"], ignore_attr = TRUE)
})

test_that("multiD() is working (reference Del Giudice, 2009)", {
    # from Del Giudice (2009) On the Real Magnitude of Psychological Sex Differences
    d <- c(0.6, -0.4)
    r <- 0.5
    S <- gen_sigma(length(d), r)
    D <- 1.01
    Dhat <- round(multiD(d, S), 2)
    expect_equal(Dhat, D)
})

test_that("get_d_from_D() is returning the same as multiD()", {
    # from Del Giudice (2009) On the Real Magnitude of Psychological Sex Differences
    p <- 5
    d <- rep(0.7, p)
    S <- gen_sigma(p, rmin = 0.4, rmax = 0.7)
    D <- multiD(d, S)
    dhat <- get_d_from_D(D, S)
    expect_equal(dhat, unique(d), tolerance = .tol["gen"])
})

test_that(".get_inds() return the correct dataset", {
    p <- 10
    X <- sim_clust(2, 100, 0.5, 0.7, rmin = 0.7, nind = p)
    expect_identical(.get_inds(X), X[, 1:p], info = "columns already ordered from sim_clust()")
    
    X <- sim_clust(2, 100, 0.5, 0.7, rmin = 0.7, nind = p)
    names(X) <- paste0("y", 1:ncol(X))
    expect_equal(ncol(.get_inds(X)), 0, info = "with columns without x[0-9] no selection")
})

test_that("model_based_clust() is working on a simulated dataset", {
    set.seed(0)
    G <- 2
    # high n with known 2 clusters
    X <- sim_clust(G, 1e3, dmin = 10, rmin = 0, nind = 2)
    X <- .get_inds(X)
    Ghat <- model_based_clust(X, "BIC", cmin = 1, cmax = 5)
    expect_equal(Ghat$nclust, G)
})

test_that("kmeans_clust() is working on a simulated dataset (compared to factoextra::fviz_nbclust())", {
    set.seed(0)
    X <- sim_clust(2, 500, 5, rmin = 0, nind = 10)
    X <- .get_inds(X)
    
    # for no including extra dependencies, values are hard coded
    # EXP <- factoextra::fviz_nbclust(x = X, FUNcluster = kmeans, k.max = 5, nstart = 25)$data
    
    EXP <- data.frame(
        clusters = 1:5,
        y <- c(0, 0.73422800, 0.41584819, 0.08785141, 0.08280352)
    )
    EXP <- EXP[2:nrow(EXP), ] # removing k = 1 because already tested with fpc::dudahart2
    RES <- kmeans_clust(X, 1, 5, debug = TRUE)$silhouette
    expect_equal(RES, EXP, tolerance = .tol["gen"], info = "same silhouette", ignore_attr = TRUE)
    expect_equal(RES, EXP, tolerance = .tol["gen"], info = "same silhouette", ignore_attr = TRUE)
})

test_that("sim_clust() is returning the correct structure(n1, n2, cols and rows)", {
    n <- 400
    pn1 <- 0.7
    G <- 2
    p <- 10
    X <- sim_clust(nclust = 2, n = n, dmin = 10, rmin = 0, nind = p, pn1 = pn1)
    
    expect_equal(nrow(X), n, info = "number of observations")
    expect_equal((table(X$group)/nrow(X))[1], pn1, info = "proportions of n for g1", ignore_attr = TRUE)
    expect_equal(length(unique(X$group)), G, info = "number of clusters")
    expect_equal(ncol(.get_inds(X)), p, info = "number of indicators")
})

test_that("get_summary_stat() return correct value compared to semTools::mvrnonnorm()",{
    p <- 10
    S <- gen_sigma(p, 0.6)
    d <- runif(p, 0, 1)
    X <- semTools::mvrnonnorm(100, d, S, empirical = TRUE)
    X <- data.frame(X)
    RES <- data.frame(t(get_summary_stats(X)))
    expect_equal(RES$mean, d, tolerance = .tol["gen"], info = "mean", ignore_attr = TRUE)
    expect_equal(RES$sd, rep(1, p), tolerance = .tol["gen"], info = "sd", ignore_attr = TRUE)
    expect_equal(RES$skewness, psych::skew(X), tolerance = .tol["gen"], info = "skewness", ignore_attr = TRUE)
    expect_equal(RES$kurtosi, psych::kurtosi(X), tolerance = .tol["gen"], info = "kurtosi", ignore_attr = TRUE)
    expect_equal(RES$q25, sapply(X, quantile, probs = 0.25), tolerance = .tol["gen"], info = "25%", ignore_attr = TRUE)
    expect_equal(RES$q50, sapply(X, quantile, probs = 0.50), tolerance = .tol["gen"], info = "50%", ignore_attr = TRUE)
    expect_equal(RES$q75, sapply(X, quantile, probs = 0.75), tolerance = .tol["gen"], info = "75%", ignore_attr = TRUE)
})

test_that(".check_data() is correctly accepting/rejecting datasets", {
    p <- 10
    S <- gen_sigma(p, 0.6)
    X <- sim_data(100, 0.5, S)
    expect_s3_class(.check_data(X), "data.frame")
    X$ch <- sample(letters, nrow(X), replace = TRUE) 
    expect_error(.check_data(X), info = "dataset with characters")
})

test_that("sim_clust works with udata", {
    d <- 5
    p <- 7
    
    sim <- sim_clust(nclust = 2, 
                     n = 100, 
                     dmin = 0, 
                     rmin = 0, 
                     nind = 7)
    
    dat <- clustersimulation:::.get_inds(sim)
    dat <- clustersimulation:::.check_data(dat)
    udata <- prep_user_data(dat)
    
    udata$n <- 1e5 # increase n
    sim_from_udata_power <- sim_clust(nclust = 1, dmin = d, udata = udata)
    est_m_power <- apply(clustersimulation:::.get_inds(sim_from_udata_power), 2, mean)
    sim_from_udata_type1 <- sim_clust(nclust = 1, dmin = 0, udata = udata)
    est_m_type1 <- apply(clustersimulation:::.get_inds(sim_from_udata_type1), 2, mean)
    expect_equal(abs(est_m_power), rep(d, p), info = "power (d = 5)", tolerance = .tol["gen"], ignore_attr = TRUE)
    expect_equal(abs(est_m_type1), rep(0, p), info = "type 1 (d = 0)", tolerance = .tol["gen"], ignore_attr = TRUE)
})

test_that(".get_vcov() works with mtcars", {
    expected <- list(
        vcov = cov(mtcars),
        cor = cor(mtcars),
        vi = apply(mtcars, 2, var)
    )
    
    actual <- .get_vcov(mtcars)
    
    expect_equal(actual, expected)
})
