# differences between simulated and real road crossing values
# margaret mercer
# Oct 16, 2024

library(ggplot2)
library(gridExtra)
library(grid)


# clear workspace
rm(list=ls())


# load results
results <- read.csv("results/results.csv")

# density plots ####

# results$line_color <- ifelse(results$real_crossings_vs_simulated == "higher", "red", 
#                              ifelse(results$real_crossings_vs_simulated == "lower", "green",
#                              "gray"))

long <- results %>%
  pivot_longer(cols = c(real_crossings_all, simulated_crossings_all), 
               names_to = "type", 
               values_to = "value")

long$type <- factor(long$type, levels = c("simulated_crossings_all", "real_crossings_all"))

all <- ggplot(long, aes(x = type, y = value, fill = type)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    trim = FALSE,
    width = .6,
    .width = 0,
    justification = -.3,
    point_colour = NA) +
  geom_boxplot(
    width = .25,
    outlier.shape = NA,
    lwd = 1
  ) +
  geom_point(
    size = 3,
    alpha = .75,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) +
  coord_cartesian(xlim = c(1.4, 2.2), clip = "off") +
  labs(title = NULL,
       y = "Number Crossings",
       x = NULL)  +
  labs(title = "(A) All Roads") +
  scale_x_discrete(labels = c("Simulated", "Observed")) +
  scale_fill_manual(values = c("real_crossings_all" = "#FF988E", "simulated_crossings_all" = "#A0B3C1")) +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), 
    axis.text.x = element_text(face = "bold", color = "black", size = 25),
    axis.title.y = element_text(size = 30, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    legend.position = 'none'
  )
  # + ylim(-1000, 12500)
all


# ggplot(long, aes(x = type, y = value)) +
#   geom_boxplot(
#     width = .25,
#     outlier.shape = NA
#   ) +
#   geom_point(
#     size = 1.5,
#     alpha = .2,
#     position = position_jitter(seed = 1, width = .1)
#   ) +
#   geom_line(aes(group = name, color = line_color), alpha = .5, lwd = 1) +
#   coord_cartesian(xlim = c(1.2, 2.9), clip = "off") +
#   labs(title = "Paired Crossings by Type", x = "Type", y = "Value") +
#   scale_color_identity()


# # Create the KDE plot
# ggplot(long, aes(x = value, fill = type)) +
#   geom_density(alpha = 0.5) +  # Use alpha for transparency
#   labs(title = "Kernel Density Estimation",
#        x = "Value",
#        y = "Density") +
#   theme_minimal()

# density plots major roads only ####
long_maj <- results %>%
  pivot_longer(cols = c(real_crossings_maj, simulated_crossings_maj), 
               names_to = "type", 
               values_to = "value")

long_maj$type <- factor(long_maj$type, levels = c("simulated_crossings_maj", "real_crossings_maj"))

maj <- ggplot(long_maj, aes(x = type, y = value, fill = type)) +
  ggdist::stat_halfeye(
    adjust = 3,
    width = .6,
    .width = 0,
    justification = -.3,
    point_colour = NA,
    ) +
  geom_boxplot(
    width = .25,
    outlier.shape = NA,
    lwd = 1
  ) +
  geom_point(
    size = 3,
    alpha = .75,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) +
  coord_cartesian(xlim = c(1.4, 2.2), clip = "off") +
  labs(title = NULL,
       y = NULL,
       x = NULL) +
  labs(title = "(B) Major Roads") +
  scale_x_discrete(labels = c("Simulated", "Observed")) +
  scale_fill_manual(values = c("real_crossings_maj" = "#FF988E", "simulated_crossings_maj" = "#A0B3C1")) +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), 
    axis.text.x = element_text(face = "bold", color = "black", size = 25),
    axis.title.y = element_text(size = 30, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    legend.position = 'none'
  )
  # + ylim(-1000, 12500)
maj

# ggplot(long_maj, aes(x = type, y = value)) + 
#   geom_boxplot(
#     width = .25,
#     outlier.shape = NA
#   ) +
#   geom_point(
#     size = 1.5,
#     alpha = .2,
#     position = position_jitter(seed = 1, width = .1)
#   ) + 
#   geom_line(aes(group = name, color = line_color), alpha = .5, lwd = 1) +
#   coord_cartesian(xlim = c(1.2, 2.9), clip = "off") + 
#   labs(title = "Paired Crossings by Type", x = "Type", y = "Value") +
#   scale_color_identity()


# # Create the KDE plot
# ggplot(long_maj, aes(x = value, fill = type)) +
#   geom_density(alpha = 0.5) +  # Use alpha for transparency
#   labs(title = "Kernel Density Estimation",
#        x = "Value",
#        y = "Density") +
#   theme_minimal()


# density plots minor roads only ####
long_min <- results %>%
  pivot_longer(cols = c(real_crossings_min, simulated_crossings_min), 
               names_to = "type", 
               values_to = "value")

long_min$type <- factor(long_min$type, levels = c("simulated_crossings_min", "real_crossings_min"))

min <- ggplot(long_min, aes(x = type, y = value, fill = type)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    width = .6,
    trim = FALSE,
    .width = 0,
    justification = -.3,
    point_colour = NA) +
  geom_boxplot(
    width = .25,
    outlier.shape = NA,
    lwd = 1
  ) +
  geom_point(
    size = 3,
    alpha = .75,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) +
  coord_cartesian(xlim = c(1.4, 2.2), clip = "off") +
  labs(title = NULL,
       y = NULL,
       x = NULL)  +
  scale_x_discrete(labels = c("Simulated", "Observed")) +
  labs(title = "(C) Minor Roads") +
  scale_fill_manual(values = c("real_crossings_min" = "#FF988E", "simulated_crossings_min" = "#A0B3C1")) +
  theme(axis.text.x = element_text(face = "bold", color = "black")) +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), 
    axis.text.x = element_text(face = "bold", color = "black", size = 25),
    axis.title.y = element_text(size = 30, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    legend.position = 'none'
  )
  #+ ylim(-1000, 12500)
min

# put density plots together ####

grid.arrange(all, maj, min, ncol = 3)

# g <- arrangeGrob(all, maj, min, ncol = 3, top = title_grob)
# plot(g)
# ggsave(file="real_simulated_crossings.jpg", g)
# 
# # lollipop all ####
# # Define custom colors
# my_colors <- c("Increase" = "#0B5401", 
#                "Slight Increase" = "#77A87C", 
#                "No Change" = "steelblue", "Slight Decrease" = "#C67976", 
#                "Decrease" = "#8B0000", 
#                "White" = "white")
# 
# # Create the lollipop chart with legend title removed and custom colors
# lol <- ggplot(results, aes(x = name, y = diff_all,
#                            fill = ifelse(diff_all > 0, "Increase",
#                                          ifelse(diff_all < 0, "Decrease", "No Change")),
#                            color = ifelse(diff_all > 0, "Increase",
#                                           ifelse(diff_all < 0, "Decrease", "No Change"))
# )) +
#   geom_segment(aes(xend = name, yend = 0)) +
#   geom_point(shape = 21, size = 3) +
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c((min((results$diff_all)) - 0.15), 
#                                 (max((results$diff_all)) + 0.1)),) +
#   coord_flip() +
#   theme_classic () +
#   theme(axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         legend.position = "none",
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         text = element_text(family = "Helvetica", size = 15)) +
#   geom_hline(yintercept = 0, color = "darkgray") +
#   labs(x = NULL, 
#        y = "Difference Between Real and Simulated Crossings", 
#        main = "Difference Between Real and Simulated Crossings") +
#   scale_color_manual(values = my_colors) +  # Set custom colors
#   guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
#   scale_fill_manual(values = my_colors) +
#   geom_text(aes(x = name, 
#                 y = (min((diff_all)) - 0.14), 
#                 label = name), 
#             hjust = 0, 
#             vjust = 0.5, 
#             color = "black")
# lol
# 
# 
# 
# # lollipop major roads only ####
# # Define custom colors
# my_colors <- c("Increase" = "#0B5401", 
#                "Slight Increase" = "#77A87C", 
#                "No Change" = "steelblue", "Slight Decrease" = "#C67976", 
#                "Decrease" = "#8B0000", 
#                "White" = "white")
# 
# # Create the lollipop chart with legend title removed and custom colors
# lol_maj <- ggplot(results, aes(x = name, y = diff_maj,
#                            fill = ifelse(diff_maj > 0, "Increase",
#                                          ifelse(diff_maj < 0, "Decrease", "No Change")),
#                            color = ifelse(diff_maj > 0, "Increase",
#                                           ifelse(diff_maj < 0, "Decrease", "No Change"))
# )) +
#   geom_segment(aes(xend = name, yend = 0)) +
#   geom_point(shape = 21, size = 3) +
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c((min((results$diff_maj)) - 0.15), 
#                                 (max((results$diff_maj)) + 0.1)),) +
#   coord_flip() +
#   theme_classic () +
#   theme(axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         legend.position = "none",
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         text = element_text(family = "Helvetica", size = 15)) +
#   geom_hline(yintercept = 0, color = "darkgray") +
#   labs(x = NULL, 
#        y = "Difference Between Real and Simulated Crossings", 
#        main = "Difference Between Real and Simulated Crossings") +
#   scale_color_manual(values = my_colors) +  # Set custom colors
#   guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
#   scale_fill_manual(values = my_colors) +
#   geom_text(aes(x = name, 
#                 y = (min((diff_maj)) - 0.14), 
#                 label = name), 
#             hjust = 0, 
#             vjust = 0.5, 
#             color = "black")
# lol_maj
# 
