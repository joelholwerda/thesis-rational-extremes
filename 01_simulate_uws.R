# Setup ------------------------------------------------------------------------

set.seed(1234)

library(tidyverse)
library(arrow)
library(furrr)
library(parallel)
library(cowplot)
library(ggtext)
library(rlang)
library(here)
library(sn)

# The number of simulations for each value of the free parameters
n_simulations <- 10000

# Experiments 2-4 of Ludvig et al. (2014) were simulated to examine the generalisability of our analysis using Experiment 1. Setting `simulate_all_experiments` to `FALSE` reproduces only the published values
simulate_all_experiments <- FALSE

# The number of cores used for the parallel simulations, etc. will be the number of available cores minus the value of the reserved_cores variable (or one if the number of reserved cores is greater than or equal to the number of available cores).
reserved_cores <- 2
workers  <- max(c(1, detectCores() - reserved_cores))
plan(multisession, workers = workers)

# Options for saving figures using `save_figures()`
save_figures_to_file <- TRUE
figure_width <- 80
figure_height <- 200

# Functions based on Lieder et al. (2018)
source(here::here("src", "encode_outcomes.R")) # (Equation 16)
source(here::here("src", "sample_utilities.R")) # (Equation 11)
source(here::here("src", "aggregate_samples.R")) # (Equation 12)

# Simulation helper functions
source(here::here("src", "generate_experienced_outcomes.R"))
source(here::here("src", "summarise_simulations.R"))
source(here::here("src", "summarise_error.R"))
source(here::here("src", "uws_experienced.R"))
source(here::here("src", "simulate_uws_experienced.R"))
source(here::here("src", "uws_no_encoding.R"))
source(here::here("src", "simulate_error_comparisons.R"))
source(here::here("src", "simulate_error_simple.R"))
source(here::here("src", "graph_error.R"))
source(here::here("src", "graph_heatmap.R"))

# Information about each experiment in Ludvig et al. (2014)
source(here::here("src", "ludvig_designs.R"))

# ggplot themes shared across multiple graphs
source(here::here("src", "ggplot_themes.R"))

# Run simulations for Ludvig et al. (2014) experiments -------------------------
simulations <- list()

# Utility-weighted sampling ----------------------------------------------------
simulations$ludvig_uws <- map(
    experiments,
    ~ simulate_uws_experienced(
        outcomes = .$outcomes,
        probabilities = .$probabilities,
        n_experienced = .$n_experienced,
        sd_noise = c(0.05, 0.1, 0.2),
        n_samples = 1:50,
        n_simulations = n_simulations
    )
)

# Generate graphing summaries for each experiment (for Figure 2.2 in thesis)
simulations$summaries$ludvig_uws <- future_map(
    simulations$ludvig_uws,
    summarise_simulations,
    .options = furrr_options(seed = TRUE)
)

# Utility-weighted sampling (no bias correction) -------------------------------
simulations$ludvig_uncorrected <- map(
    experiments,
    ~ simulate_uws_experienced(
        outcomes = .$outcomes, 
        probabilities = .$probabilities, 
        n_experienced = .$n_experienced, 
        sd_noise = c(0.05, 0.1, 0.2), 
        n_samples = 1:50, 
        n_simulations = n_simulations,
        bias_correction = FALSE
    )
)

# Representative sampling ------------------------------------------------------
simulations$ludvig_representative <- map(
    experiments,
    ~ simulate_uws_experienced(
        outcomes = .$outcomes, 
        probabilities = .$probabilities, 
        n_experienced = .$n_experienced, 
        sd_noise = c(0.05, 0.1, 0.2), 
        n_samples = 1:50, 
        n_simulations = n_simulations,
        utility_weighted = FALSE,
        bias_correction = FALSE
    )
)

# Generate graphing summaries for each experiment (for Figure 2.3 in thesis)
simulations$summaries$ludvig_error <- pmap(
    list(
        uws = simulations$ludvig_uws,
        uncorrected = simulations$ludvig_uncorrected,
        representative = simulations$ludvig_representative,
        # The average for the high-value risky option in Ludvig et al. (2014)
        average = ifelse(simulate_all_experiments, c(20, 25, 40, 60, -20), 20) 
    ),
    function(uws, uncorrected, representative, average) {
        bind_rows(
            list(
                uws = uws, 
                uncorrected = uncorrected, 
                representative = representative
            ), .id = "algorithm") %>% 
        filter(option == "high_risky") %>% 
        group_by(algorithm, n_samples, sd_noise) %>% 
        summarise_error(average) %>% 
        ungroup()
    }
)

# Create graphs for each experiment in Ludvig et al. 2014 ----------------------
graphs <- list()

# Average estimates of the risky options for each sample size and noise level (Figure 2.2a in thesis)
graphs$uws$estimates <- map2(
    .x = simulations$summaries$ludvig_uws,
    .y = experiments,
    ~ .x  %>% 
        mutate(sd_noise = paste("SD = ", sd_noise)) %>% 
        ggplot(aes(x = n_samples, y = mean_original_scale, colour = pair)) +
            geom_hline(
                # Add hline for the EV of each safe option
                yintercept = keep(.y$outcomes, ~ length(.) == 1) %>% unlist(),
                colour = "grey80"
            ) +
            geom_smooth(span = 0.2, se = FALSE) +
            geom_point(alpha = 0.5, size = 1) +
            labs(
                title = "<b>A) Estimates</b><br>
                    <span style = 'font-size:6pt'>The average utility-weighted sampling estimate for the <span style = 'color:#0072B2;'>positive</span> and <span style = 'color:#D55E00;'>negative</span> risky options.</span>",
                x = "Sample size",
                y = "Estimated value of risky option"
            ) +
            themes$shared +
            themes$colours +
            scale_y_continuous(
            # Set limits at min(estimates, option values) and max(estimates, option values)
                limits = c(
                    min(c(unlist(.x$mean_original_scale), unlist(.y$outcomes))), 
                    max(c(unlist(.x$mean_original_scale), unlist(.y$outcomes)))
                ),
                breaks = unlist(.y$outcomes) %>% unique()
            ) +
            facet_grid(rows = vars(factor(sd_noise))
    )
)

# Proportion of choices for the risky option for each sample size and noise level (Figure 2.2b in thesis)
graphs$uws$choices <- map(
simulations$summaries$ludvig_uws,
~ .x  %>% 
    mutate(sd_noise = paste("SD = ", sd_noise)) %>% 
    ggplot(aes(x = n_samples, y = prop_risky_better, colour = pair)) +
        geom_hline(yintercept = 0.5, colour = "grey80") +
        geom_smooth(span = 0.2, se = FALSE) +
        geom_point(alpha = 0.5, size = 1) +
        labs(
            title = "<b>B) Choices</b><br>
                <span style = 'font-size:6pt'>The proportion of choices for the risky option predicted by utility-weighted sampling for the <span style = 'color:#0072B2;'>positive</span> and <span style = 'color:#D55E00;'>negative</span> risky options.</span>",
            x = "Sample size",
            y = "Proportion of choices for the risky option"
        ) +
        themes$shared +
        themes$colours +
        scale_y_continuous(
            limits = c(0, 1),
            breaks = c(0, 0.25, 0.50, 0.75, 1),
            labels = c("0", ".25", ".5", ".75", "1") 
        ) +
        facet_grid(rows = vars(factor(sd_noise)))
)

# Distribution of non-aggregated estimates for sample size == 2 (not included in thesis)
graphs$uws$distributions <- simulations$ludvig_uws$mixed %>% 
    filter(n_samples == 2, option == "high_risky") %>% 
    mutate(sd_noise = paste("SD = ", sd_noise)) %>% 
    ggplot(aes(
        x = original_scale, 
        y = after_stat(count), 
        fill = bias_corrected
    )) +
        geom_vline(xintercept = 20, colour = "grey80") +
        geom_vline(xintercept = c(0, 40), linetype = "dashed", colour = "grey80") +
        geom_density(position = "stack", colour = "black") + 
        geom_density(position = "stack", colour = NA) + 
        labs(
            title = "<b>C) Distributions</b><br>
                    <span style = 'font-size:6pt'>The number of utility-weighted sampling estimates that sampled <span style = 'color:#0072B2;'>both possible outcomes</span> or <span style = 'color:#56B4E9;'>the same outcome twice</span> for each value of the positive risky option.</span>",
            x = "Estimated value of risky option",
            y = "Number of simulations"
            ) +
        facet_grid(rows = vars(sd_noise)) + 
        scale_x_continuous(breaks = c(-20, 0, 20, 40, 60)) +
        coord_cartesian(xlim = c(-35, 75)) +
        themes$shared +
        themes$fill

# Combine elements of Figure 2.2 in thesis
graphs$uws$mixed <- plot_grid(
    graphs$uws$estimates$mixed, 
    graphs$uws$choices$mixed,
    nrow = 2, 
    align = "h", 
    axis = "lr"
)

# Save to figures folder
ggsave(
    graphs$uws$mixed, 
    file = here::here("output", "figures", "uws_combined.pdf"), 
    width = figure_width, 
    height = figure_height * 2 / 3, 
    units = "mm"
)

# Graph of UWS and representative bias (original scale) by sample size (Figure 2.3a in thesis)
graphs$ludvig_error$bias <- map(
    simulations$summaries$ludvig_error,
    ~ .x  %>% 
        mutate(sd_noise = paste("SD = ", sd_noise)) %>% 
        ggplot(aes(x = n_samples, y = bias, colour = algorithm)) +
            geom_hline(yintercept = 0, colour = "grey80") +
            geom_smooth(span = 0.12, se = FALSE) +
            geom_point(alpha = 0.5, size = 1) +
            labs(
                title = "<b>A) Bias</b><br>
                    <span style = 'font-size:6pt'>The bias for the <span style = 'color:#0072B2;'>utility-weighted sampling</span>, <span style = 'color:#56B4E9;'>no bias correction</span>, and <span style = 'color:#009E73;'>standard Monte Carlo</span> estimates for the positive risky option.</span>",
                x = "Sample size",
                y = "Bias"
            ) +
            themes$shared +
            themes$colours +
            facet_grid(rows = vars(factor(sd_noise)), scales = "free")
)

# Graph of UWS and representative variance (original scale) by sample size (Figure 2.3b in thesis)
graphs$ludvig_error$variance <- map(
    simulations$summaries$ludvig_error,
    ~ .x  %>% 
        mutate(sd_noise = paste("SD = ", sd_noise)) %>% 
        ggplot(aes(x = n_samples, y = variance, colour = algorithm)) +
            geom_hline(yintercept = 0, colour = "grey80") +
            geom_smooth(span = 0.12, se = FALSE) +
            geom_point(alpha = 0.5, size = 1) +
            labs(
                title = "<b>B) Variance</b><br>
                    <span style = 'font-size:6pt'>The variance for the <span style = 'color:#0072B2;'>utility-weighted sampling</span>, <span style = 'color:#56B4E9;'>no bias correction</span>, and <span style = 'color:#009E73;'>standard Monte Carlo</span> estimates for the positive risky option.</span>",
                x = "Sample size",
                y = "Variance"
            ) +
            themes$shared +
            themes$colours +
            facet_grid(rows = vars(factor(sd_noise)), scales = "free")
)

# Graph of UWS and representative mean squared error (original scale) by sample size (Figure 2.3c in thesis)
graphs$ludvig_error$error <- map(
    simulations$summaries$ludvig_error,
    ~ .x %>% 
        mutate(sd_noise = paste("SD = ", sd_noise)) %>% 
        ggplot(aes(x = n_samples, y = error, colour = algorithm)) +
            geom_hline(yintercept = 0, colour = "grey80") +
            geom_smooth(span = 0.12, se = FALSE) +
            geom_point(alpha = 0.5, size = 1) +
            labs(
                title = "<b>C) Error</b><br>
                    <span style = 'font-size:6pt'>The error for the <span style = 'color:#0072B2;'>utility-weighted sampling</span>, <span style = 'color:#56B4E9;'>no bias correction</span>, and <span style = 'color:#009E73;'>standard Monte Carlo</span> estimates for the positive risky option.</span>",
                x = "Sample size",
                y = "Mean squared error"
            ) +
            themes$shared +
            themes$colours +
            facet_grid(rows = vars(factor(sd_noise)), scales = "free")
)

# Combine elements of Figure 2.3 in thesis
graphs$ludvig_error$combined <- plot_grid(
    graphs$ludvig_error$bias$mixed, 
    graphs$ludvig_error$variance$mixed, 
    graphs$ludvig_error$error$mixed, 
    nrow = 3, 
    align = "h", 
    axis = "lr"
)

# Save to figures folder
ggsave(
    graphs$ludvig_error$combined, 
    file = here::here("output", "figures", "ludvig_error_combined.pdf"), 
    width = figure_width, 
    height = figure_height, units = "mm"
)

# Simulations for other distributions ------------------------------------------

# Specify values for free parameters
simulations$sd_noise <- 0.05
simulations$n_samples <- c(2, 10, 20)

# Run simulations using binomial distributions (Figure 2.4a in thesis) ---------
simulations$binomial <- future_map_dfr(
    simulations$n_samples,
    ~ simulate_error_comparisons(
        n_simulations = n_simulations, 
        utilities = sample(c(-1, 1), size = 100, replace = TRUE), 
        average_utility = seq(0, 3, by = 0.05), 
        sd_noise = simulations$sd_noise, 
        n_samples = .x
    ),
    .options = furrr_options(seed = TRUE)
)

# Graph error by sample size and average utility for binomial distributions
graphs$binomial <- graph_error(
    simulations$binomial, 
    "A) Binomial", 
    "binomial distributions"
)

# Run simulations using normal distributions (Figure 2.4b in thesis) -----------
simulations$normal <- future_map_dfr(
    simulations$n_samples,
    ~ simulate_error_comparisons(
        n_simulations = n_simulations, 
        utilities = rnorm(100), 
        average_utility = seq(0, 3, by = 0.05), 
        sd_noise = simulations$sd_noise, 
        n_samples = .x
    ),
    .options = furrr_options(seed = TRUE)
)

# Graph error by sample size and average utility for normal distributions 
graphs$normal <- graph_error(
    simulations$normal, 
    "B) Normal", 
    "normal distributions"
)

# Run simulations using uniform distributions (Figure 2.4c in thesis) ----------
simulations$uniform <- future_map_dfr(
    simulations$n_samples,
    ~ simulate_error_comparisons(
        n_simulations = n_simulations, 
        utilities = runif(100, -1.75, 1.75), 
        average_utility = seq(0, 3, by = 0.05), 
        sd_noise = simulations$sd_noise, 
        n_samples = .x
    ),
    .options = furrr_options(seed = TRUE)
)

# Graph error by sample size and average utility for uniform distributions 
graphs$uniform <- graph_error(
    simulations$uniform, 
    "C) Uniform", 
    "uniform distributions"
)

# Run simulations using exponential distributions (Figure 2.4d in thesis) ------
simulations$exponential <- future_map_dfr(
    simulations$n_samples,
    ~ simulate_error_comparisons(
        n_simulations = n_simulations, 
        utilities = rexp(100, rate = 1) - 1, 
        average_utility = seq(-5, 5, by = 0.1), 
        sd_noise = simulations$sd_noise, 
        n_samples = .x
    ),
    .options = furrr_options(seed = TRUE)
)

# Graph error by sample size and average utility for exponential distributions 
graphs$exponential <- graph_error(
    simulations$exponential, 
    "D) Exponential", 
    "exponential distributions"
)

# Run simulations using skewed binomial distributions (Figure 2.4e in thesis) ----
simulations$skewed_binomial <- future_map_dfr(
    simulations$n_samples,
    ~ simulate_error_comparisons(
        n_simulations = n_simulations, 
        utilities = sample(
            c(-1/3, 3), 
            size = 100, 
            replace = TRUE, 
            prob = c(0.9, 0.1)
        ), 
        average_utility = seq(-5, 5, by = 0.1), 
        sd_noise = simulations$sd_noise, 
        n_samples = .x
    ),
    .options = furrr_options(seed = TRUE)
)

# Graph error by sample size and average utility for binomial distributions 
graphs$skewed_binomial <- graph_error(
    simulations$skewed_binomial, 
    "E) Skewed binomial", 
    "skewed binomial distributions"
)

# Run simulations using skewed normal distributions (Figure 2.4f in thesis) ----
simulations$skewed_normal <- future_map_dfr(
    simulations$n_samples,
    ~ simulate_error_comparisons(
        n_simulations = n_simulations, 
        utilities = rsn(100, dp = cp2dp(c(0, 1, .95), family = "SN")), 
        average_utility = seq(-5, 5, by = 0.1), 
        sd_noise = simulations$sd_noise, 
        n_samples = .x
    ),
    .options = furrr_options(seed = TRUE)
)

# Graph error by sample size and average utility for skewed normal distributions 
graphs$skewed_normal <- graph_error(
    simulations$skewed_normal, 
    "F) Skewed normal", 
    "skewed normal distributions"
)

# Combine elements of Figure 2.4 in thesis
graphs$distributional_error$combined <- plot_grid(
    graphs$binomial$error,
    graphs$normal$error, 
    graphs$uniform$error, 
    graphs$exponential$error, 
    graphs$skewed_binomial$error, 
    graphs$skewed_normal$error, 
    nrow = 3, 
    align = "h", 
    axis = "lr"
)

# Save to figures folder
ggsave(
    graphs$distributional_error$combined, 
    file = here::here("output", "figures", "distributional_error_combined.pdf"), 
    width = figure_width * 2, 
    height = figure_height, units = "mm"
)


# Simulations for error heatmaps ----

# Run simulations using binomial distributions for heatmap (Figure 2.5a in thesis) ----
simulations$heatmap$binomial <- 1:20 |> 
    future_map_dfr(
        ~ simulate_error_simple(
            n_simulations = 10000, 
            utilities = sample(c(-1, 1), size = 100, replace = TRUE),
            average_utility = seq(0, 1, by = 0.05), 
            sd_noise = 0, 
            n_samples = .x
        ),
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
    )

graphs$heatmap$binomial <- graph_heatmap(
    simulations$heatmap$binomial, 
    "A) Binomial", 
    "binomial distributions"
)

# Run simulations using normal distributions for heatmap (Figure 2.5b in thesis) ----
simulations$heatmap$normal <- 1:20 |> 
    future_map_dfr(
        ~ simulate_error_simple(
            n_simulations = 10000, 
            utilities = rnorm(100), 
            average_utility = seq(0, 1, by = 0.05), 
            sd_noise = 0, 
            n_samples = .x
        ),
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
    )

graphs$heatmap$normal <- graph_heatmap(
    simulations$heatmap$normal, 
    "B) Normal", 
    "normal distributions"
)

# Run simulations using uniform distributions for heatmap (Figure 2.5c in thesis) ----
simulations$heatmap$uniform <- 1:20 |> 
    future_map_dfr(
        ~ simulate_error_simple(
            n_simulations = 10000, 
            utilities = runif(100, -1.75, 1.75),
            average_utility = seq(0, 1, by = 0.05), 
            sd_noise = 0, 
            n_samples = .x
        ),
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
    )

graphs$heatmap$uniform <- graph_heatmap(
    simulations$heatmap$uniform, 
    "C) Uniform", 
    "uniform distributions"
)

# Run simulations using exponential distributions for heatmap (Figure 2.5d in thesis) ----
simulations$heatmap$exponential <- 1:20 |> 
    future_map_dfr(
        ~ simulate_error_simple(
            n_simulations = 10000, 
            utilities = rexp(100, rate = 1) - 1,
            average_utility = seq(-2, 2, by = 0.2), 
            sd_noise = 0, 
            n_samples = .x
        ),
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
    )

graphs$heatmap$exponential <- graph_heatmap(
    simulations$heatmap$exponential, 
    "D) Exponential", 
    "exponential distributions"
)

# Run simulations using skewed binomial distributions for heatmap (Figure 2.5e in thesis) ----
simulations$heatmap$skewed_binomial <- 1:20 |> 
    future_map_dfr(
        ~ simulate_error_simple(
            n_simulations = 10000, 
            utilities = sample(
                c(-1/3, 3), 
                size = 100, 
                replace = TRUE, 
                prob = c(0.9, 0.1)
            ), 
            average_utility = seq(-2, 2, by = 0.2), 
            sd_noise = 0, 
            n_samples = .x
        ),
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
    )

graphs$heatmap$skewed_binomial <- graph_heatmap(
    simulations$heatmap$skewed_binomial, 
    "E) Skewed binomial", 
    "skewed binomial distributions"
)

# Run simulations using skewed normal distributions for heatmap (Figure 2.5f in thesis) ----
simulations$heatmap$skewed_normal <- 1:20 |> 
    future_map_dfr(
        ~ simulate_error_simple(
            n_simulations = 10000, 
            utilities = rsn(100, dp = cp2dp(c(0, 1, .95), family = "SN")),
            average_utility = seq(-2, 2, by = 0.2), 
            sd_noise = 0, 
            n_samples = .x
        ),
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
    )

graphs$heatmap$skewed_normal <- graph_heatmap(
    simulations$heatmap$skewed_normal, 
    "F) Skewed normal", 
    "skewed normal distributions"
)

# Combine elements of Figure 2.5 in thesis
graphs$heatmap$combined <- plot_grid(
    graphs$heatmap$binomial,
    graphs$heatmap$normal, 
    graphs$heatmap$uniform, 
    graphs$heatmap$exponential, 
    graphs$heatmap$skewed_binomial, 
    graphs$heatmap$skewed_normal, 
    nrow = 3, 
    align = "h", 
    axis = "lr"
)

# Save to figures folder
ggsave(
    graphs$heatmap$combined, 
    file = here::here("output", "figures", "heatmap_combined.pdf"), 
    width = figure_width * 2, 
    height = figure_height, units = "mm"
)
