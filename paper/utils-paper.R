trim_df <- function(data, n = 4, digits = 3){
    data <- lapply(data, function(x) if(is.factor(x)) as.character(x) else x)
    data <- data.frame(data)
    data <- data.frame(sapply(data, function(x) if(is.numeric(x)) round(x, digits) else x))
    dots <- data[1, ]
    dots[1, ] <- "..."
    nrows <- nrow(data)
    if(nrows <= 5){
        trimmed <- data
    } else{
        if(nrows <= n*2){
            n <- floor(n/2)
        }
        trimmed <- rbind(
            data[1:n,],
            dots,
            data[(nrows-(n - 1)):nrows, ]
        )
    }
    rownames(trimmed) <- NULL
    return(trimmed)
}