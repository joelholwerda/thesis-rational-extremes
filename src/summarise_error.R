# Function to calculate the bias, variance, and mean squared error
summarise_error <- function(simulation, average) {
    simulation %>% 
        summarise(
            bias = abs(mean(original_scale) - average),
            variance = var(original_scale),
            error = mean((original_scale - average)^2)
        )
}