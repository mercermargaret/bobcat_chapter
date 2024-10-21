# differences between simulated and real road crossing values
# margaret mercer
# Oct 16, 2024

library(ggtext)
library(colorspace)
library(ragg)

# cartoons
# url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/man/figures/lter_penguins.png"
# img <- magick::image_read((url))
# pic <- grid::rasterGrob(img, interpolate = TRUE)

# load results
results <- read.csv("results/results.csv")

# density plots ####

results$line_color <- ifelse(results$diff_all > 0, "red", "green")

real <- results$real_crossings_all
simulated <- results$simulated_crossings_all

long <- results %>%
  pivot_longer(cols = c(real_crossings_all, simulated_crossings_all), 
               names_to = "type", 
               values_to = "value")

ggplot(long, aes(x = type, y = value)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, 2.9), clip = "off") +
  coord_flip()
# it would be good if I could get the violin to go out to the left rather than right


ggplot(long, aes(x = type, y = value)) + 
  geom_boxplot(
    width = .25,
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(seed = 1, width = .1)
  ) + 
  geom_line(aes(group = name, color = line_color), alpha = .5, lwd = 1) +
  coord_cartesian(xlim = c(1.2, 2.9), clip = "off") + 
  labs(title = "Paired Crossings by Type", x = "Type", y = "Value") +
  scale_color_identity()


# Create the KDE plot
ggplot(long, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Kernel Density Estimation",
       x = "Value",
       y = "Density") +
  theme_minimal()

# density plots major raods only ####
long_maj <- results %>%
  pivot_longer(cols = c(real_crossings_maj, simulated_crossings_maj), 
               names_to = "type", 
               values_to = "value")

ggplot(long_maj, aes(x = type, y = value)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, 2.9), clip = "off") +
  coord_flip()

ggplot(long_maj, aes(x = type, y = value)) + 
  geom_boxplot(
    width = .25,
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(seed = 1, width = .1)
  ) + 
  geom_line(aes(group = name, color = line_color), alpha = .5, lwd = 1) +
  coord_cartesian(xlim = c(1.2, 2.9), clip = "off") + 
  labs(title = "Paired Crossings by Type", x = "Type", y = "Value") +
  scale_color_identity()


# Create the KDE plot
ggplot(long_maj, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Kernel Density Estimation",
       x = "Value",
       y = "Density") +
  theme_minimal()


# lollipop all ####
# Define custom colors
my_colors <- c("Increase" = "#0B5401", 
               "Slight Increase" = "#77A87C", 
               "No Change" = "steelblue", "Slight Decrease" = "#C67976", 
               "Decrease" = "#8B0000", 
               "White" = "white")

# Create the lollipop chart with legend title removed and custom colors
lol <- ggplot(results, aes(x = name, y = diff_all,
                           fill = ifelse(diff_all > 0, "Increase",
                                         ifelse(diff_all < 0, "Decrease", "No Change")),
                           color = ifelse(diff_all > 0, "Increase",
                                          ifelse(diff_all < 0, "Decrease", "No Change"))
)) +
  geom_segment(aes(xend = name, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c((min((results$diff_all)) - 0.15), 
                                (max((results$diff_all)) + 0.1)),) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, 
       y = "Difference Between Real and Simulated Crossings", 
       main = "Difference Between Real and Simulated Crossings") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = name, 
                y = (min((diff_all)) - 0.14), 
                label = name), 
            hjust = 0, 
            vjust = 0.5, 
            color = "black")
lol



# lollipop major roads only ####
# Define custom colors
my_colors <- c("Increase" = "#0B5401", 
               "Slight Increase" = "#77A87C", 
               "No Change" = "steelblue", "Slight Decrease" = "#C67976", 
               "Decrease" = "#8B0000", 
               "White" = "white")

# Create the lollipop chart with legend title removed and custom colors
lol_maj <- ggplot(results, aes(x = name, y = diff_maj,
                           fill = ifelse(diff_maj > 0, "Increase",
                                         ifelse(diff_maj < 0, "Decrease", "No Change")),
                           color = ifelse(diff_maj > 0, "Increase",
                                          ifelse(diff_maj < 0, "Decrease", "No Change"))
)) +
  geom_segment(aes(xend = name, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c((min((results$diff_maj)) - 0.15), 
                                (max((results$diff_maj)) + 0.1)),) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, 
       y = "Difference Between Real and Simulated Crossings", 
       main = "Difference Between Real and Simulated Crossings") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = name, 
                y = (min((diff_maj)) - 0.14), 
                label = name), 
            hjust = 0, 
            vjust = 0.5, 
            color = "black")
lol_maj

