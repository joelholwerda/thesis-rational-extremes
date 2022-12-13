graph_heatmap <- function(data, title, distribution) {
    
    # Allocate different breakpoints for symmetrical and skewed distributions
    if (max(data$average_utility) == 1) {
        x_breaks <- seq(0, 1, 0.2)
    } else {
        x_breaks <- seq(-2, 2, 1)
    }
    
    # Create heatmap
    graph <- data |> 
        select(-variance, -bias) |> 
        pivot_wider(names_from = algorithm, values_from = error) |> 
        arrange(n_samples) |> 
        fill(representative) |> 
        mutate(
            difference = representative - uws, 
            percent = difference / representative,
            percent_label = ifelse(abs(percent) > 2, "white", "black")
        ) |> 
        ggplot(aes(x = factor(average_utility), y = factor(n_samples), fill = percent)) +
        geom_tile() +
        geom_text(
            aes(label = round(percent, 2), colour = percent_label),
            size = 1
        ) +
        labs(
            title = paste0("<b>", title, "</b><br>
                    <span style = 'font-size:6pt'>The amount that the utility-weighted sampling estimates are <span style = 'color:#1818FF;'>better</span> or <span style = 'color:#FF1818;'>worse</span> than the standard Monte estimates for the ", distribution, ".</span>"),
            x = "Average outcome",
            y = "Sample size"
        ) +
        themes$shared +
        scale_color_manual(values = c("white" = "white", "black" = "black")) +  
        scale_x_discrete(breaks = x_breaks) +
        scale_y_discrete(breaks = c(1, 5, 10, 15, 20)) +
        themes$fill_continuous
        
    
    return(graph)
}
