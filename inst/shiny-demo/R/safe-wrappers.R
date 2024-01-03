#' safe_wrappers
#'
#' @keywords internal 
.safe_wrappers <- function(){
    list(
        sim_clust = purrr::possibly(sim_clust, otherwise = NA),
        sim_data = purrr::possibly(sim_data, otherwise = NA),
        kmeans_clust = purrr::possibly(kmeans_clust, otherwise = NA),
        model_based_clust = purrr::possibly(model_based_clust, otherwise = NA),
        rand_index = purrr::possibly(mclust::adjustedRandIndex, otherwise = NA)
    )
}