# Sample outcomes using utility-weighted sampling (Eq. 11)

#' @param utilities Must be a named list with the utilities for each option, e.g., `utilities = list(safe = .5, risky = c(0, 1))`. `utilities` is usually `output$utilities` from `encode_outcomes()`.
#' @param probabilities Must be `NULL` or a named list with the probabilities for each option. `probabilities` is `NULL` by default and only needs to be specified when outcome probabilities are not equal. When outcomes are generated using `generate_experienced_outcomes()`, probabilities are incorporated into the experienced outcomes and specifying `probabilities` is not necessary. It is primarily useful for decisions involving described outcomes.
#' @param n_samples The number of outcomes sampled from memory (parameter in UWS)
#' @param average_utility The global average utility of the outcomes for all options. If `average_utility == NULL` (default), this parameter is calculated using the average of `utilities`
#' @param utility_weighted If `utility_weighted == TRUE` (default), outcomes are sampled using utility-weighted sampling according to Eq. 11 from Lieder et al. (2018). # If `utility_weighted == FALSE`, representative (unbiased, naive Monte Carlo) sampling is used instead.
#' @return A list that contains:
#'    - `utilities` A list of options that each contain an array of sampled utilities.
#'    - `distances` A list of options that each contain an array of distances between each sampled utility and the average utility.
#'    - `indices` A list of options that each contain an array of indices used to identify the outcome (before encoding) associated with each sampled utility.

sample_utilities <- function(utilities, n_samples, average_utility = NULL, probabilities = NULL, utility_weighted = TRUE) {
    
    # If average_utility is not specified, use the average of utilities
    if (is.null(average_utility)) {
        average_utility <- unlist(utilities) %>% mean()
    }
    
    # If probabilities is not specified, assign each outcome equal probability
    if (is.null(probabilities)) {
        probabilities <- map(utilities, ~ rep(1 / length(.), length(.)))
    }
    
    # Calculate the distance between each outcome and the mean
    distances <- map(utilities, ~ abs(. - average_utility))
    
    # In the next step (.x * .y / sum(.x * .y)), utility-weighted sampling divides by zero when all distances associated with an option are 0 (e.g., `both_safe` in Experiment 4G and 4L of Ludvig et al. (2014)). This only occurs for safe options with no encoding noise. Therefore, given that all samples contain the same outcome, it seems reasonable to instead replace these 0s with another arbitrary value: 1
    distances <- map(
        distances, 
        ~ if (sum(.) == 0) {
            rep(1, length(.))
        } else {
            .
        } 
    )
    
    if (utility_weighted) {
        
        # Biased sampling distribution (`q`) determines the probability of sampling each outcome (Eq. 11). `q` is biased towards extremes by multiplying each probability by its distance from the mean. These values are then converted into the probabilities of sampling each outcome by dividing the `probability * distance` for each outcome by the sum of `probabilities * distances` for all options
        q <- map2(.x = probabilities, .y = distances, ~ .x * .y / sum(.x * .y))
    
    } else {
        # Use representative sampling if `utility_weighted == FALSE`
        q <- probabilities
    }
    
    # Sample n_samples indices using the probabilities specified in `q` (biased or representative)
    indices <- map2(.x = utilities, .y = q, ~ sample(seq_along(.x), size = n_samples, replace = TRUE, prob = .y))
    
    # Use sampled indices to select utilities and their corresponding distances
    samples <- list(
        utilities = map2(.x = utilities, .y = indices, ~ .x[.y]),
        distances = map2(.x = distances, .y = indices, ~ .x[.y]),
        indices = indices
    )
    
    return(samples)

}