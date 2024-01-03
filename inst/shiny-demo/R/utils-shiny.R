#' shiny_table
#' @param data a dataframe
#' @param caption a character vector with the caption. Default to \code{NULL} thus no caption.
#' @noRd
shiny_table <- function(data, exclude_cols = NULL, caption = NULL){
    if(!is.null(exclude_cols)){
        exclude_cols <- which(colnames(data) %in% exclude_cols)
        data <- data[, -exclude_cols]
    }
    data |> 
        kableExtra::kable(align = "c",
                          caption = caption) |> 
        kableExtra::kable_styling(bootstrap_options = c("striped", "condensed"),
                                  full_width = FALSE) |> 
        kableExtra::column_spec(1, bold = TRUE) |> 
        kableExtra::column_spec(nrow(data) + 1, border_right = TRUE)
}

#' plot_summary
#' @param data a dataframe
#' @param ... other named arguments passed to \code{psych::pairs.panels()}
#' @noRd
plot_summary <- function(data, ...){
    psych::pairs.panels(data,
                        method = "pearson", # correlation method
                        hist.col = "#00AFBB",
                        smooth = FALSE,
                        density = TRUE,
                        ellipses = FALSE,
                        ...
    ) 
}

#' .prep_user_data_shiny
#' @description wrapper of the [prep_user_data] function to use within the shiny app.
#' @noRd
.prep_user_data_shiny <- function(data, 
                                  n = 100, 
                                  type = c("df1", "df2")){
    type <- match.arg(type)
    udata <- prep_user_data(data)
    if(type == "df2"){
        udata$n <- n
    }
    return(udata)
}