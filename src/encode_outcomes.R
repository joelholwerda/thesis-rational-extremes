# Recode outcomes using efficient coding (Eq. 16)

#' @param outcomes Must be a named list of the outcomes (experienced or described) for each option, e.g., `outcomes = list(safe = 20, risky = c(0, 40))`. Experienced outcomes can be generated using `generate_experienced_outcomes()`. 
#' @param sd_noise The standard deviation of the noise added to each encoded outcome (parameter in UWS).
#' @param min Used to normalise outcomes. If `min == NULL` (default), calculated using `outcomes`.
#' @param max Used to normalise outcomes. If `max == NULL` (default), calculated using `outcomes`.
#' @param zero_to_one If `zero_to_one == FALSE` (default), outcomes are recoded using Eq. 16 from Lieder et al. (2018). Outcomes are normalised so that the range == 1 but the max and min are not always 0 and 1. If `zero_to_one == TRUE`, outcomes are normalised so that the min == 0 and max == 1. This helps to keep the axis comparable when graphing and reflects the motivation of finite-bandwidth neural coding. The importance distribution is calculated using distances from the average rather than absolute magnitudes so `zero_to_one` makes no difference to the predictions of the model.
#' @param noise_first If `noise_first == FALSE` (default), encoding noise is added after normalising outcomes according to Eq. 16 from Lieder et al. (2018). If `noise_first == TRUE`, noise is added before normalising the outcomes. When `zero_to_one` and `noise_first` are both `TRUE`, the normalised utilities (including noise) range from 0 to 1. Unlike `zero_to_one`, `noise_first` changes the predictions of the model. This is because when `noise_first == TRUE`, `sd_noise` is set relative to the original scale rather than the normalised scale.
#' @return A list that contains:
#'    - `utilities` A list of options that each contain and array of encoded utilities.
#'    - `min` A list of options that each contain the min of the encoded utilities.
#'    - `max` A list of options that each contain the min of the encoded utilities.

encode_outcomes <- function(outcomes, sd_noise, min = NULL, max = NULL, zero_to_one = FALSE, noise_first =  FALSE) {

    # If `noise_first` is `TRUE`, add independent noise to each outcome before normalising
    if (noise_first) {
        outcomes <- map(outcomes, ~ . + rnorm(length(.), mean = 0, sd = sd_noise))
    }
    
    # If `min` and `max` not specified, calculate based on `outcomes`
    min <- ifelse(is.null(min), unlist(outcomes) %>% min(), min)
    max <- ifelse(is.null(max), unlist(outcomes) %>% max(), max)
    
    # Normalise `outcomes` to have range of one. If `zero_to_one` is `TRUE`, range = [0, 1]
    utilities <- map(outcomes, ~ (. - min * zero_to_one) / (max - min))
    
    # If `noise_first` is `FALSE`, add independent to each outcome noise after normalising
    if (!noise_first) {
        utilities <- map(utilities, ~ . + rnorm(length(.), mean = 0, sd = sd_noise))
    }
    
    output <- list(
        utilities = utilities,
        min = min,
        max = max
    )
    
    return(output)
}
