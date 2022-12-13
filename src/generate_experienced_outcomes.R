
# Generate experienced outcomes

# Simulate the experience of participants in each experiment by generating an array of experienced outcomes based on specified outcomes and probabilities. 
#' @param outcomes Must be a named list, e.g., `outcomes = list(safe = 20, risky = c(0, 40)`.
#' @param probabilities Must be a named list, e.g., `probabilities = list(safe = 1, risky = c(0.5, 0.5)`.
#' @param n_experienced The number of times the simulated participant chooses each option. Unless the number of experienced outcomes is small, the probability of retrieving the same instance twice in the sampling phase is relatively low and the exact values of `n_experienced` have little impact on the model predictions. Must be a named list, e.g., `n_experienced = list(safe = 50, risky = 50)`.
#' @return A list of options that each contain an array of experienced outcomes.

generate_experienced_outcomes <- function(outcomes, probabilities, n_experienced) {
    
    # Retrieve option names
    option_names <- names(outcomes)
    
    # Initialise list to store experienced outcomes
    experienced_outcomes <- list()

    # Generate a sample of experienced outcomes based on outcome probabilities for each option
    for (option in option_names) {

        if (length(outcomes[[option]]) > 1) {
            experienced_outcomes[[option]] <- sample(
                outcomes[[option]], 
                size = n_experienced[[option]], 
                replace = TRUE, 
                prob = probabilities[[option]]
            )
        } else {
            # If there is only one outcome associated with the option, repeat for each experience
            experienced_outcomes[[option]] <- rep(outcomes[[option]], n_experienced[[option]])
        }
    }

    return(experienced_outcomes)
}
