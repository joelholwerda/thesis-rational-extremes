
graph_error <- function(data, title, distribution){

    graphs <- list()

    graphs$bias <- data %>%
        filter(algorithm != "representative") %>%
        ggplot(aes(x = average_utility, y = bias, colour = algorithm)) +
            geom_hline(yintercept = 0, colour = "grey80") +
            geom_hline(data = data %>% filter(algorithm == "representative"), aes(yintercept = bias), colour = "#009E73", linewidth = 1) +
            geom_smooth(span = 0.12, se = FALSE) +
            geom_point(alpha = 0.5, size = 1) +
            labs(
                title = paste0("<b>Bias</b><br>
                    <span style = 'font-size:6pt'>The bias for the <span style = 'color:#0072B2;'>utility-weighted sampling</span>, <span style = 'color:#56B4E9;'>no bias correction</span>, and <span style = 'color:#009E73;'>standard Monte Carlo</span> estimates for the ", distribution, ".</span>"),
                x = "Average outcome",
                y = "Bias"
            ) +
            themes$shared +
            themes$colours +
            facet_grid(
                rows = vars(factor(n_samples)), 
                cols = vars(factor(sd_noise)), 
                scales = "free"
            )

    # Graph of UWS and representative variance (binomial distribution) by standardised distance from average

    graphs$variance <- data %>%
        filter(algorithm != "representative") %>%
        ggplot(aes(x = average_utility, y = variance, colour = algorithm)) +
            geom_hline(yintercept = 0, colour = "grey80") +
            geom_hline(data = data %>% filter(algorithm == "representative"), aes(yintercept = variance), colour = "#009E73", linewidth = 1) +
            geom_smooth(span = 0.12, se = FALSE) +
            geom_point(alpha = 0.5, size = 1) +
            labs(
                title = paste0("<b>Variance</b><br>
                    <span style = 'font-size:6pt'>The variance for the <span style = 'color:#0072B2;'>utility-weighted sampling</span>, <span style = 'color:#56B4E9;'>no bias correction</span>, and <span style = 'color:#009E73;'>standard Monte Carlo</span> estimates for the ", distribution, ".</span>"),
                x = "Average outcome",
                y = "Variance"
            ) +
            themes$shared +
            themes$colours +
            facet_grid(
                rows = vars(factor(n_samples)), 
                cols = vars(factor(sd_noise)), 
                scales = "free"
            )

    # Graph of UWS and representative mean squared error (binomial distribution) by standardised distance from average

    graphs$error <- data %>% 
        filter(algorithm != "representative") %>%
        ggplot(., aes(x = average_utility, y = error, colour = algorithm)) +
            geom_hline(yintercept = 0, colour = "grey80") +
            geom_hline(data = data %>% filter(algorithm == "representative"), aes(yintercept = error), colour = "#009E73", linewidth = 1) +
            geom_smooth(span = 0.12, se = FALSE) +
            geom_point(alpha = 0.5, size = 1) +
            labs(
                title = paste0("<b>", title, "</b><br>
                    <span style = 'font-size:6pt'>The error for the <span style = 'color:#0072B2;'>utility-weighted sampling</span>, <span style = 'color:#56B4E9;'>no bias correction</span>, and <span style = 'color:#009E73;'>standard Monte Carlo</span> estimates for the ", distribution, ".</span>"),
                x = "Average outcome",
                y = "Mean squared error"
            ) +
            themes$shared +
            themes$colours +
            facet_grid(
                rows = vars(factor(n_samples)), 
                cols = vars(factor(sd_noise)), 
                scales = "free"
            )

    return(graphs)

}
