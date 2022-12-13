
# Information about each experiment in Ludvig et al. (2014). Each simulation reflects a choice made on the first trial of the final block in the experiment. All simulations assume that an option was selected randomly on each preceding trial except for `experiments$mixed_better` in which the better option was always selected on catch trials and otherwise an option was selected randomly.

experiments <- list()

# 1. Mixed problems --- selected an option randomly on each trial
experiments$mixed <- list(
    name = "1_mixed",
    outcomes = list(
        low_safe = -20, 
        low_risky = c(-40, 0), 
        high_safe = 20, 
        high_risky = c(0, 40)
    ), 
    probabilities = list(
        low_safe = 1, 
        low_risky = c(.5, .5), 
        high_safe = 1, 
        high_risky = c(.5, .5)
    ), 
    n_experienced = list(
        low_safe = 48, 
        low_risky = 48, 
        high_safe = 48, 
        high_risky = 48
    )
)

# Experiments 2-4 were included in an appendix to assess the generalisability of our evaluation of Experiment 1 in the main text. Setting `simulate_all_experiments` to `TRUE` reproduces these published values
if (exists("simulate_all_experiments")) {

    if (simulate_all_experiments) {
        # 1. Mixed problems --- always selected the better option on catch trials
        experiments$mixed_better <- list(
            name = "1_mixed",
            outcomes = list(
                low_safe = -20, 
                low_risky = c(-40, 0), 
                high_safe = 20, 
                high_risky = c(0, 40)
            ), 
            probabilities = list(
                low_safe = 1, 
                low_risky = c(.5, .5), 
                high_safe = 1, 
                high_risky = c(.5, .5)
            ), 
            n_experienced = list(
                low_safe = 32, 
                low_risky = 32, 
                high_safe = 64, 
                high_risky = 64
            )
        )

        # 2. No zeroes  
        experiments$no_zeroes <- list(
            name = "2_no_zeroes",
            outcomes = list(
                low_safe = -25, 
                low_risky = c(-45, -5), 
                high_safe = 25, 
                high_risky = c(5, 45)
            ), 
            probabilities = list(
                low_safe = 1, 
                low_risky = c(.5, .5), 
                high_safe = 1, 
                high_risky = c(.5, .5)
            ), 
            n_experienced = list(
                low_safe = 60, 
                low_risky = 60, 
                high_safe = 60, 
                high_risky = 60
            )
        )

        # 3. Magnitude
        experiments$magnitude <- list(
            name = "3_magnitude",
            outcomes = list(
                lowX_safe = -40, 
                lowX_risky = c(-80, 0), 
                highX_safe = 40, 
                highX_risky = c(0, 80),
                lowNX_safe = -20, 
                lowNX_risky = c(-40, 0), 
                highNX_safe = 20, 
                highNX_risky = c(0, 40)
            ), 
            probabilities = list(
                lowX_safe = 1, 
                lowX_risky = c(.5, .5), 
                highX_safe = 1, 
                highX_risky = c(.5, .5),
                lowNX_safe = 1, 
                lowNX_risky = c(.5, .5), 
                highNX_safe = 1, 
                highNX_risky = c(.5, .5)
            ), 
            n_experienced = list(
                lowX_safe = 36, 
                lowX_risky = 36, 
                highX_safe = 36, 
                highX_risky = 36,
                lowNX_safe = 36, 
                lowNX_risky = 36, 
                highNX_safe = 36, 
                highNX_risky = 36
            )
        )

        # 4G. All gains
        experiments$gains <- list(
            name = "4g_gains",
            outcomes = list(
                low_safe = 20, 
                low_risky = c(0, 40), 
                high_safe = 60, 
                high_risky = c(40, 80), 
                both_safe = 40, 
                both_risky = c(0, 80)
            ), 
            probabilities = list(
                low_safe = 1, 
                low_risky = c(.5, .5), 
                high_safe = 1, 
                high_risky = c(.5, .5), 
                both_safe = 1, 
                both_risky = c(.5, .5)
            ), 
            n_experienced = list(
                low_safe = 40, 
                low_risky = 40, 
                high_safe = 40, 
                high_risky = 40, 
                both_safe = 40, 
                both_risky = 40
            )
        )

        # 4L. All losses
        experiments$losses <- list(
            name = "4l_losses",
            outcomes = list(
                low_safe = -60, 
                low_risky = c(-40, -80), 
                high_safe = -20, 
                high_risky = c(-40, 0), 
                both_safe = -40, 
                both_risky = c(-80, 0)
            ), 
            probabilities = list(
                low_safe = 1, 
                low_risky = c(.5, .5), 
                high_safe = 1, 
                high_risky = c(.5, .5), 
                both_safe = 1, 
                both_risky = c(.5, .5)
            ), 
            n_experienced = list(
                low_safe = 40, 
                low_risky = 40, 
                high_safe = 40, 
                high_risky = 40, 
                both_safe = 40, 
                both_risky = 40
            )
        )
    }
}
