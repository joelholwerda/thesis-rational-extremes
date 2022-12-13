# Run simulations by combining the using utility-weighed sampling functions (no encoding) for the complete model and representative sampling.

#' @param n_simulations The number of estimates to simulate for each option.
#' @param utilities Must be an array of the utilities or a function that generates an array (e.g., `rnorm(100)`). If a random function is provided, a different set of utilities will be generated for each simulation.
#' @param sd_noise The standard deviation of the noise added to each utility. Note that utilities are not normalised before adding noise (SD based on original scale). 
#' @param n_samples Parameter specified in `sample_utilities()`.
#' @param average_utility Parameter specified in `aggregate_samples()`.
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

simulate_error_simple <- function(n_simulations, utilities, average_utility, sd_noise, n_samples) {
    
    # Quoted utilities to be evaluated in each simulation (allows utilities to be a function)
    quoted_utilities <- enquo(utilities)
    
    # Create a tibble of parameters used in the simulation. Each parameter is combined with the other parameters and then repeated `n_simulations` times. `average_utility` is not included in the representative sampling simulations because it does not affect the estimates. To increase the accuracy of the representative sampling estimates, the number of simulations is multiplied by 10.
    parameters_uws <- tibble(
        n_samples = n_samples %>% 
            rep(length(sd_noise) * length(average_utility) * n_simulations),
        sd_noise = sd_noise %>% 
            rep(each = length(.env$n_samples)) %>% 
            rep(length(average_utility) * n_simulations),
        average_utility = average_utility %>%
            rep(each = length(.env$n_samples) * length(.env$sd_noise)) %>% 
            rep(n_simulations)
    )
    
    parameters_representative <- tibble(
        n_samples = n_samples %>% 
            rep(length(sd_noise) * n_simulations),
        sd_noise = sd_noise %>% 
            rep(each = length(.env$n_samples)) %>% 
            rep(n_simulations)
    )
    
    simulations  <- list()
    
    # Generate n_simulations estimates using the full utility-weighted sampling model
    simulations$uws <- pmap_dfr(
        parameters_uws,
        # Generate estimates using the utility-weighted sampling functions
        function(average_utility, n_samples, sd_noise) {
            # Generate utilities if `utilities` contains a random function
            evaluated_utilities <- eval_tidy(quoted_utilities)
            
            # Add encoding noise to each utility
            noisy_utilities <- evaluated_utilities + 
                rnorm(length(evaluated_utilities), sd = sd_noise)
            
            # Sample and aggregate utilities
            uws_no_encoding(
                list(noisy_utilities), 
                n_samples,
                average_utility, 
                utility_weighted = TRUE, 
                bias_correction = TRUE
            ) %>% 
                mutate(sd_noise = sd_noise)
        },
        .id = "id"
    )
    
    # Generate n_simulations estimates using representative sampling
    simulations$representative <- pmap_dfr(
        parameters_representative,
        # Generate estimates using the utility-weighted sampling functions
        function(n_samples, sd_noise) {
            # Generate utilities if `utilities` contains a random function
            evaluated_utilities <- eval_tidy(quoted_utilities)
            
            # Add encoding noise to each utility
            noisy_utilities <- evaluated_utilities + 
                rnorm(length(evaluated_utilities), sd = sd_noise)
            
            # Sample and aggregate utilities
            uws_no_encoding(
                list(noisy_utilities), 
                n_samples,
                average_utility = 0, 
                utility_weighted = FALSE, 
                bias_correction = FALSE
            ) %>% 
                mutate(sd_noise = sd_noise)
        },
        .id = "id"
    )
    
    # Generate graphing summaries for binomial distributions
    simulations$summary <- bind_rows(
        list(
            uws = simulations$uws, 
            representative = simulations$representative
        ),
        .id = "algorithm"
    ) %>%
        rename(original_scale = estimates) %>% 
        group_by(algorithm, n_samples, sd_noise, average_utility) %>% 
        summarise_error(0) %>% 
        ungroup()
    
    return(simulations$summary)
}
