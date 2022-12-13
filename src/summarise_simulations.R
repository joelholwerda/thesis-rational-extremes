
# Summarise the simulations for graphing

#' @param simulation The simulation to be summarised. Usually the output of `simulate_uws_experienced()`.
#' @return A tibble that contains:
#'     - `pair` The name of the pair options in the choice (e.g., "high" or "low").
#'     - `sd_noise` The value of the encoding noise parameter of utility-weighted sampling.
#'     - `n_samples` The value of the sample size parameter of utility-weighted sampling.
#'     - `mean_original_scale` The mean of the utility-weighted sampling estimates on the original outcome scale.
#'     - `prop_bias_corrected` The proportion of estimates that sampled from multiple outcome and allowed the aggregation mechanism weighted samples to (imperfectly) correct for the biased sampling.
#'     - The proportion of simulations where the estimate for the risky option was better than the safe option (encoding noise was added to each experience safe outcome and these encoded utilities are sampled identically to the risky outcomes).
#'     - The proportion of simulations where the estimate for the risky option was better than the expected value of the safe value of the safe option (no encoding noise).

summarise_simulations <- function(simulation) {
    simulation %>% 
        # Separate option column into columns indication pair and safe or risky 
        separate(col = option, into = c("pair", "option"), sep = "_") %>% 
            # Pivot simulations wider with columns for safe and risky options
            pivot_wider(
                id_cols = c("id", "pair", "sd_noise", "n_samples"), 
                names_from = option, 
                values_from = c("estimates", "original_scale", "bias_corrected", "expected_values")
            ) %>% 
            mutate(
                # Determine whether the estimate for the risky option is better than the safe option
                risky_better = original_scale_risky > original_scale_safe,
                risky_better_ev = original_scale_risky > expected_values_safe
            ) %>% 
            # Generate summaries for each option and parameter
            group_by(pair, sd_noise, n_samples) %>% 
            summarise(
                mean_original_scale = mean(original_scale_risky),
                prop_bias_corrected = mean(bias_corrected_risky),
                prop_risky_better = mean(risky_better),
                prop_risky_better_ev = mean(risky_better_ev)
            ) %>% 
            ungroup()
}
