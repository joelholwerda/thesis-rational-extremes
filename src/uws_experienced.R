# Combine the utility-weighted sampling functions.

#' @param experienced_outcomes Usually the result of `generate_experienced_outcomes()`.
#' @param sd_noise Parameter specified in `encode_outcomes()`.
#' @param n_samples Parameter specified in `sample_utilities()`.
#' @param zero_to_one Parameter specified in `encode_outcomes()`.
#' @param noise_first Parameter specified in `encode_outcomes()`.
#' @param utility_weighted Parameter specified in `sample_utilities()`.
#' @param bias_correction Parameter specified in `aggregate_samples()`.
#' @return If `extended_output == FALSE` (default) returns a tibble that contains:
#'     - `option` The name of the option that was simulated.
#'     - `sd_noise` The value of the encoding noise parameter of utility-weighted sampling.
#'     - `n_samples` The value of the sample size parameter of utility-weighted sampling.
#'     -  `estimates` The utility-weighted sampling estimates on the encoded scale.
#'     - `original_scale` The utility-weighted sampling estimates on the original outcome scale.
#'     - `bias_corrected` A boolean indicating whether all samples were from the same outcome (`FALSE`) or whether the aggregation mechanism weighted samples to (imperfectly) correct for the biased sampling (`TRUE`).
#'     - `n_intermediate` For the Ludvig experiments, this heuristically gives the number of intermediate samples. Used for plotting where correctness is verified. Should not be used for other purposes unless validity is assessed.

# Define the utility-weighted sampling function using Equations 11, 12, and 16 from Lieder et al. (2018)
uws_experienced <- function(experienced_outcomes, sd_noise, n_samples, zero_to_one = FALSE, noise_first =  FALSE, utility_weighted = TRUE, bias_correction = TRUE) {

    utilities <- encode_outcomes(
        outcomes = experienced_outcomes, 
        sd_noise = sd_noise, 
        zero_to_one = zero_to_one, 
        noise_first = noise_first
    )

    samples <- sample_utilities(
        utilities = utilities$utilities,
        n_samples = n_samples,
        utility_weighted = utility_weighted
    )

    estimates <- aggregate_samples(
        samples = samples$utilities, 
        distances = samples$distances, 
        bias_correction = bias_correction
    )

    # Translate the estimated utility to the original scale
    original_scale <- map(estimates, ~ . * (utilities$max - utilities$min) + utilities$min * zero_to_one)

    # Check whether bias corrected (cannot correct when all samples from same outcome)
    bias_corrected <- map2(.x = experienced_outcomes, .y = samples$indices, ~ .x[.y] %>% unique() %>% length > 1)

    # For the Ludvig experiments, this heuristically gives the number of intermediate samples. Used for plotting where correctness is verified. Should not be used for other purposes unless validity is assessed.
    n_intermediate <- map2(
        .x = experienced_outcomes, 
        .y = samples$indices, 
        ~ .x[.y] %>% table() %>% min() %>% ifelse(. == n_samples, 0, .)
    )

    output <- tibble(
        option = names(estimates),
        sd_noise = sd_noise, 
        n_samples = n_samples,
        estimates = unlist(estimates), 
        original_scale = unlist(original_scale), 
        bias_corrected = unlist(bias_corrected),
        n_intermediate = unlist(n_intermediate)
    )

    return(output)

}
