
# Run simulations by combining the using utility-weighed sampling and experienced outcome generation functions.

#' @param n_simulations The number of estimates to simulate for each option
#' @param outcomes Parameter specified in `generate_experienced_outcomes()`.
#' @param probabilities Parameter specified in `generate_experienced_outcomes()`.
#' @param n_experienced Parameter specified in `generate_experienced_outcomes()`.
#' @param sd_noise Parameter specified in `encode_outcomes()`.
#' @param n_samples Parameter specified in `sample_utilities()`.
#' @param zero_to_one Parameter specified in `encode_outcomes()`.
#' @param noise_first Parameter specified in `encode_outcomes()`.
#' @param utility_weighted Parameter specified in `sample_utilities()`.
#' @param bias_correction Parameter specified in `aggregate_samples()`.
#' @param file Must either be `NULL` (default) or a character string. In the latter case, the simulation is saved using `write_parquet()` in a file named after the string supplied in `file`. The .parquet extension is added automatically. If the file already exists, it will be loaded instead of rerunning the simulations
#' @return A tibble that contains:
#'     - `id` A unique identifier for each simulation.
#'     - `option` The name of the option that was simulated.
#'     - `sd_noise` The value of the encoding noise parameter of utility-weighted sampling.
#'     - `n_samples` The value of the sample size parameter of utility-weighted sampling.
#'     -  `estimates` The utility-weighted sampling estimates on the encoded scale.
#'     - `original_scale` The utility-weighted sampling estimates on the original outcome scale.
#'     - `bias_corrected` A boolean indicating whether all samples were from the same outcome (`FALSE`) or whether the aggregation mechanism weighted samples to (imperfectly) correct for the biased sampling (`TRUE`).
#'     - `n_intermediate` The number of samples that were not the best or worst outcomes.
#'     - `expected values` The expected value of the sampled option (original scale).

simulate_uws_experienced <- function(n_simulations, outcomes, probabilities, n_experienced, sd_noise, n_samples, zero_to_one = FALSE, noise_first = FALSE, utility_weighted = TRUE, bias_correction = TRUE, file = NULL) {

    # Load saved simulation if `file` specified and saved simulation exists
    if (!is.null(file)) {
        file_location <- here::here("output", "cache", paste0(file, ".parquet"))
        if (file.exists(file_location) && getOption("save_simulations") != "overwrite") {
            return(read_parquet(file_location))
        }
    }

    # Calculate expected value of each option
    expected_values <- map_dbl(outcomes, ~ mean(.))
    
    # Generate n_simulations estimates for each option (parallel using `future`)
    simulations <- future_map2_dfr(
        .x = rep(sd_noise, each = length(n_samples)) %>% rep(n_simulations),
        .y = rep(n_samples, length(sd_noise)) %>% rep(n_simulations),
        # Generate estimates using the utility-weighted sampling functions
        ~ {
            generate_experienced_outcomes(
                outcomes = outcomes,
                probabilities = probabilities,
                n_experienced = n_experienced
            ) %>% 
            uws_experienced(
                sd_noise = .x,
                n_samples = .y,
                zero_to_one = zero_to_one, 
                noise_first = noise_first, 
                utility_weighted = utility_weighted, 
                bias_correction = bias_correction
            ) %>% 
            mutate(expected_values = expected_values)
        },
        .options = furrr_options(seed = TRUE),
        .id = "id"
    )
    
    # Save simulation if `file` is specified and `save_simulations` is not `FALSE`
    if (!is.null(file) && getOption("save_simulations") != FALSE) {
        write_parquet(simulations, file_location)
    }
    
    return(simulations)
}
