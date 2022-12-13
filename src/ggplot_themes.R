# Shared graphing themes -----------------------------------------------------------

themes <- NULL

themes$shared <- theme_bw() +
    theme(
        # Remove legend and grid
        legend.position = "none",
        panel.grid = element_blank(),
        # Change facet colour
        strip.background = element_rect(fill = "#e8e8e8"),
        # Change font sizes
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(4, "pt"),
        plot.margin = margin(2, 2, 2, 2),
        # Edit title appearance with ggtext
        plot.title.position = "plot",
        plot.title = element_textbox_simple(
            size = 8,
            lineheight = 1,
            padding = margin(2, 4, 2, 4),
            margin = margin(0, 0, 4, 0),
            fill = "#e8e8e8"
        )
    )

themes$n_samples <- NULL

# Colour blind friendly palette based on cookbook-r.com/Graphs/Colors_(ggplot2)

themes$colours <- scale_colour_manual(
    values = c(
        `low` = "#D55E00", 
        `high` = "#0072B2", 
        `both` = "#009E73", 
        `lowNX` = "#E08640", 
        `highNX` = "#4095C5",
        `lowX` = "#BA5200", 
        `highX` = "#00649C",
        `uws` = "#0072B2",
        `uncorrected` = "#56B4E9",
        `representative` = "#009E73"
    )
)

themes$fill  <- scale_fill_manual(
    values = c(
        `TRUE` = "#0072B2",
        `FALSE` = "#56B4E9"
    )
)

themes$fill_continuous <- scale_fill_gradientn(
    colors = c("#000000", "#320000", "#ff1818", "#ffffff", "#1818FF", "#000032", "#000000"),
    values = c(0, 0.42, 0.49, 0.5, 0.51, 0.52, 1),
    limits = c(-50, 50)
)
