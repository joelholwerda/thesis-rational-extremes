# Aggregate utilities and correct for biased sampling (Eq. 12)

#' @param samples Must be an array of sampled utilities. `samples` is usually `output$utilities` from `sample_utilities()`.
#' @param distances Must be an array of distances from the average utility. `distances` is usually `output$distances` from `sample_utilities()`.
#' @param bias_correction If `bias_correction == TRUE` (default), utilities are reweighted to correct for biased sampling according to Eq. 12 from Lieder et al. (2018). This algorithm is equivalent to using Eq. 4 and Eq. 5 but is more efficient. The outcome probabilities in the weighting equation (Eq. 4) cancel out because the distances are multiplied by these same probabilities used to calculate `q` (Eq. 11). If `bias_correction = FALSE`, outcomes are aggregated using their mean.
#' @return A list of options that each contain an array of utility-weighted sampling estimates. 

aggregate_samples <- function(samples, distances, bias_correction = TRUE) {
# If `bias_correction = TRUE`, apply correction for biased sampling (Eq. 12). 
    if (bias_correction) {
        estimates <- map2(.x = samples, .y = distances, ~sum(.x / .y) / sum(1 / .y))
    
    } else {
        # Otherwise, use the mean of the sampled outcomes
        estimates <- map(samples, ~ mean(.))
    }
    
    return(estimates)
}
