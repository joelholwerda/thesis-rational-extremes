
# Combine the utility-weighted sampling functions (simplified and excluding encoding)

#' @param utilities Must be a list (name optional) containing an array of the utilities for each option, e.g., `utilities = list(rnorm(100))`
#' @param n_samples Parameter specified in `sample_utilities()`.
#' @param average_utility Parameter specified in `aggregate_samples()`.
#' @param utility_weighted Parameter specified in `sample_utilities()`.
#' @param bias_correction Parameter specified in `aggregate_samples()`.
#' @return A tibble that contains:
#'     - `option` The name of the option that was simulated.
#'     - `n_samples` The value of the sample size parameter of utility-weighted sampling.
#'     - `average_utility` The specified value of the `average_utility` argument.
#'     -  `estimates` The utility-weighted sampling estimates on the encoded scale.

uws_no_encoding <- function(utilities, n_samples, average_utility, utility_weighted = TRUE, bias_correction = TRUE) {
    
    samples <- sample_utilities(
        utilities = utilities,
        n_samples = n_samples,
        average_utility = average_utility,
        utility_weighted = utility_weighted
    )
    
    estimates <- aggregate_samples(
        samples = samples$utilities, 
        distances = samples$distances, 
        bias_correction = bias_correction
    )

    output <- tibble(
        option = names(estimates),
        n_samples = n_samples,
        average_utility = average_utility,
        estimates = unlist(estimates)
    )
    
    return(output)
        
}
