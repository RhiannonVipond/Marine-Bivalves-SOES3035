library(ggplot2)
library(ggpubr)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(car)
library(fANCOVA)
library(cowplot)
library(patchwork)
library(xlsx)
library(tidyverse)
library(purrr)

# set wd
setwd("~/Uni/Year3/Semester2/SOES3035_Research_Training/Assessments/Assignment2_Group_Project/Code")

# import dataset
base <- read.csv("./data/base_data.csv")

# calculate depth ranges
base$depth_range <- base$max_depth - base$min_depth

########################
## Checking Assumptions
########################

# group data
by_fam <- base %>%
  na.omit(base) %>%
  group_by(family)

# summary statistics
summary <- by_fam %>%
  summarise(mean_mid = mean(mid_lat), 
            mean_lat = mean(lat_range),
            mean_max = mean(max_lat),
            mean_min = mean(min_lat),
            mean_depth = mean(depth_range))


# qqplots
model1 <- aov(mid_lat ~ family, data = by_fam)
model2 <- aov(depth_range ~ family, data = by_fam)
model3 <- aov(lat_range ~ family, data = by_fam)

par(mfrow = c(1, 3))

qqnorm(model1$residuals,
       main = "Mid Latitude")
qqline(model1$residuals)  # clearly non-normal

qqnorm(model2$residuals,
       main = "Depth Range")
qqline(model2$residuals)  # clearly non-normal

qqnorm(model3$residuals,
       main = "Latitude Range")
qqline(model3$residuals)  # slightly non-normal


# checking how transformations affect normality
cube_mids <- sign(by_fam$mid_lat) * (abs(by_fam$mid_lat))^(1/3)
cube_mids_df <- as.data.frame(cube_mids)

shapiro.test(by_fam$mid_lat)  # can't assume normal, w = 0.93542, p = <2.2e-16
shapiro.test(cube_mids)  # can't assume normal, w = 0.67294, p = <2.2e-16

cube_depth <- sign(by_fam$depth_range) * (abs(by_fam$depth_range))^(1/3)
cube_depth_df <- as.data.frame(cube_depth)

shapiro.test(by_fam$depth_range)  # non-normal, w = 0.50029, p = <2.2e-16
shapiro.test(cube_depth)  # non-normal, p = <2.2e-16

cube_range <- sign(by_fam$lat_range) * (abs(by_fam$lat_range))^(1/3)
cube_range_df <- as.data.frame(cube_range)

shapiro.test(by_fam$lat_range)  # non-normal, w = 0.95571, p = <2.2e-16
shapiro.test(cube_range)  # non-normal, w = 0.85696, p = <2.2e-16



# anova model - mid latitude
hist1 <- ggplot(by_fam, aes(x = mid_lat)) + 
  geom_histogram(
    fill = "royalblue4", 
    colour = "white",
    binwidth = 4) + 
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 250)) +
  scale_x_continuous(
    limits = c(-60, 80),
    breaks = seq(-60, 80, 20)) +
  labs(caption = bquote("p-value " <= " 2.2x10"^-16),
       x = "Mid Latitude",
       y = "Frequency")

hist2 <- ggplot(cube_mids_df, aes(x = cube_mids)) + 
  geom_histogram(
    fill = "royalblue4", 
    colour = "white",
    binwidth = 0.25) +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 250)) +
  scale_x_continuous(
    limits = c(-5, 5)) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(c(0, 10, 0, 0))) +
  labs(
    caption = bquote("p-value " <= " 2.2x10"^-16),
    x = expression("(Mid Latitude)"^(1/3)))


# anova model - depth range
hist3 <- ggplot(by_fam, aes(x = depth_range)) + 
  geom_histogram(
    fill = "royalblue4", 
    colour = "white",
    binwidth = 150) + 
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 150)) +
  scale_x_continuous(
    limits = c(0, 6000),
    breaks = seq(0, 6000, 1000)) +
  labs(
    caption = bquote("p-value " <= " 2.2x10"^-16),
    x = "Depth Range",
    y = "Frequency")

hist4 <- ggplot(cube_depth_df, aes(x = cube_depth)) + 
  geom_histogram(
    fill = "royalblue4", 
    colour = "white",
    binwidth = 0.5) +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 150)) +
  scale_x_continuous(
    limits = c(0, 20)) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(c(0, 10, 0, 0))) +
  labs(
    caption = bquote("p-value " <= " 2.2x10"^-16),
    x = expression("(Depth Range)"^(1/3)))


# anova model - latitude range
hist5 <- ggplot(by_fam, aes(x = lat_range)) + 
  geom_histogram(
    fill = "royalblue4", 
    colour = "white",
    binwidth = 3) + 
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 250)) +
  scale_x_continuous(
    limits = c(-20, 100),
    breaks = seq(-20, 100, 20)) +
  labs(
    caption = bquote("p-value " <= " 2.2x10"^-16),
    x = "Latitude Range",
    y = "Frequency")

hist6 <- ggplot(cube_range_df, aes(x = cube_range)) + 
  geom_histogram(
    fill = "royalblue4", 
    colour = "white",
    binwidth = 0.125) +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 250)) +
  scale_x_continuous(
    limits = c(0, 5)) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(c(0, 10, 0, 0))) +
  labs(caption = bquote("p-value " <= " 2.2x10"^-16),
       x = expression("(Latitude Range)"^(1/3))
)

# untransformed label
untran_title <- ggdraw() + 
  draw_label(
    "Untransformed",
    fontface = 'bold',
    x = 0,
    hjust = -1.2
  )

# transformed label
tran_title <- ggdraw() + 
  draw_label(
    "Transformed",
    fontface = 'bold',
    x = 0,
    hjust = -1.1
  )


titles <- plot_grid(untran_title, tran_title)

# form rows
row1 <- hist1 + hist2
row2 <- hist3 + hist4
row3 <- hist5 + hist6

# plot together rows
hists <- plot_grid(row1, row2, row3, 
                   align = "hv",
                   nrow = 3,
                   ncol = 1,
                   labels = c("A", "B", "C"),
                   label_size = 13)

# add titles to rows
plot_grid(titles, hists, 
          ncol = 1,
          rel_heights = c(0.06, 1))


# Transformations unsuccessful, and while data-set is large, 
# plots show that residuals are still very skewed

model4 <- aov(cube_mids ~ family, data = by_fam)
model5 <- aov(cube_depth ~ family, data = by_fam)
model6 <- aov(cube_range ~ family, data = by_fam)

par(mfrow = c(1, 3))

# replot q-q plots
qqnorm(model4$residuals,
       main = substitute(paste(bold("Mid Latitude"^(1/3)))))
qqline(model4$residuals)  # clearly non-normal

qqnorm(model5$residuals,
       main = substitute(paste(bold("Depth Range"^(1/3)))))
qqline(model5$residuals)  # clearly non-normal

qqnorm(model6$residuals,
       main = substitute(paste(bold("Latitude Range"^(1/3)))))
qqline(model6$residuals)  # slightly non-normal

# levene test results
result1 = leveneTest(lat_range ~ family, by_fam)
result2 = leveneTest(depth_range ~ family, by_fam)
result3 = leveneTest(mid_lat ~ family, by_fam)
result4 = leveneTest(cube_range ~ family, by_fam)
result5 = leveneTest(cube_depth ~ family, by_fam)
result6 = leveneTest(cube_mids ~ family, by_fam)

print(result1) # F = 1.7881, p = 2.613e-05
print(result2) # F = 3.5756, p = <2.2e-16
print(result3) # F = 1.9837, p = 6.914e-07
print(result4) # F = 1.4288, p = 0.007638
print(result5) # F = 3.1470, p = <2.2e-16
print(result6) # F = 1.5829, p = 0.0008065 

## NEED NON-PARAMETRIC TESTS
# non-normal data with unequal variances before and after transformations


########################
## Non-parametric tests
########################

# kruskal wallis tests
kruskal.test(depth_range ~ family, data = na.omit(base)) # significant, p = <2.2e-16
kruskal.test(lat_range ~ family, data = na.omit(base)) # significant, p = 7.147e-05
kruskal.test(mid_lat ~ family, data = na.omit(base)) # significant, p = 3.587e-14

# is there an interaction between latitude range and depth range
base_nona <- na.omit(base)
base_nona <- base_nona[, c(2, 9, 17, 19)]
base_nona$family = as.integer(factor(base_nona$family))

aov1 <- T.aov(x = c(base_nona$depth_range + base_nona$lat_range), 
              y = base_nona$mids, 
              group = base_nona$family, B = 1000, user.span = 1000)
# result is statistically significant, p-value = 0.001998


########################
## Calculating Error
########################
# clear environment
rm(list=ls())

# import dataset
base <- read.csv("./data/base_data.csv")

# make columns into numeric
base$max_depth <- as.numeric(base$max_depth)
base$min_depth <- as.numeric(base$min_depth)
base$mid_lat <- as.numeric(base$mid_lat)
# calculate depth ranges
base$depth_range <- base$max_depth - base$min_depth


# make data-set grouped by family
by_fam <- base %>%
  na.omit(base) %>%
  group_by(family)
  

# split dataset by family
dat_list = split(by_fam, by_fam$family)

# get df of family names
families <- by_fam %>%
  group_by(family) %>%
  summarise()

# set significance level
alpha = 0.05

# make template df with empty column
conf_int <- data.frame(XXX = rep(NA, 90))


# function for calculating the conf interval for each family
r1 = for(i in 1:length(dat_list)) {
  n <- nrow(dat_list[[i]])
  standard_deviation <- sd(dat_list$i$mid_lat)
  standard_error <- standard_deviation / sqrt(n)
  degrees_of_freedom = n - 1
  t_score = qt(p = alpha/2, df = degrees_of_freedom, lower.tail = F)
  .env = environment()
  conf_int[, i] <- t_score
  colnames(conf_int)[i] <- paste0("ci", i)
  }

# remove extra rows for each family
conf_int <- conf_int[1, 1:90]
  # NaNs produced where there is only one value present


# transpose data
conf_int <- as.data.frame(t(conf_int))
# change confidence interval column's name
names(conf_int)[names(conf_int) == '1'] <- 'ci'
# make column of family names from rownames
conf_int$family <- families$family
# change rownames back to numbers
rownames(conf_int) <- 1:90


# summary statistics - mean
mean <- by_fam %>%
  group_by(family) %>%
  summarise(mean_mid = mean(mid_lat),
            mean_depth = mean(depth_range),
            mean_lat = mean(lat_range))

mean <- merge(mean, conf_int, by = "family")
mean[is.na(mean)] <- 0


########################
## Visualisation
########################
# make colour-way 
fun_color_range <- colorRampPalette(c("royalblue3", "salmon"))
my_colors <- fun_color_range(20)


# group ranges by where they fall in each ecoregion
dists <- by_fam %>%
  group_by(family) %>%
  summarise(max_lat = max(max_lat),
            min_lat = min(min_lat), 
            AVml = mean(mid_lat),
            mid_lat = (max_lat + min_lat)/2) %>%
  mutate(region = case_when(-5.5 < mid_lat & mid_lat < 29.5 ~ "TEP",
                            mid_lat > 29.5 ~ "NEP",
                            mid_lat < -5.5 ~ "SEP"))


df_full <- merge(mean, dists, by = "family")


# plot graph of ranges and ecoregions
plot1 <- ggplot(df_full[1:45, ], aes(x = family)) +
  geom_errorbar(
    aes(ymin = mean_mid - ci, ymax = mean_mid + ci)
  ) +
  geom_linerange(linewidth = 1,
    aes(ymax = max_lat,
        ymin = min_lat,
        colour = region)
    ) +
  geom_point(
    alpha = 0.5,
    aes(y = AVml,
        size = mean_depth)
             ) +
  scale_y_continuous(
    breaks = seq(-60, 80, 20),
    limits = c(-60, 80)) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black", size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
    axis.title = element_text(size = 20),
    axis.title.x = element_blank(),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.justification = c(1, 0),
    legend.position = "top"
  ) +
  scale_colour_manual(values = c("royalblue1", "#FA8072", "#a15eb8")) +
  labs(
    x = "Family",
    y = "Latitude (°N)",
    colour = "Ecoregion",
    size = "Mean Depth (m)"
  )


plot2 <- ggplot(df_full[46:90, ], aes(x = family)) +
  geom_errorbar(
    aes(ymin = mean_mid - ci, ymax = mean_mid + ci)
  ) +
  geom_linerange(linewidth = 1,
                 aes(ymax = max_lat,
                     ymin = min_lat,
                     colour = region)
  ) +
  geom_point(
    alpha = 0.5,
    aes(y = AVml,
        size = mean_depth)
  ) +
  scale_radius(range = c(2, 7),
               limits = c(0, 2000)
  ) +
  scale_y_continuous(
    breaks = seq(-60, 80, 20),
    limits = c(-60, 80)) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black", size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.justification = c(1, 0),
    legend.position = "none"
  ) +
  scale_colour_manual(values = c("royalblue1", "#FA8072", "#a15eb8")) +
  labs(
    x = "Family",
    y = "Latitude (°N)",
    colour = "Ecoregion",
    size = "Mean Depth (m)"
  )

# plot then together
plot_grid(plot1, plot2, 
          nrow = 2, 
          align = "v")



# ecoregion checks: latitude range
ecoplot1 <- ggplot(dists, aes(x = region, y = range)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  geom_jitter() +
  theme_bw() +
  scale_y_continuous(limits = c(0, 150), 
                     breaks = seq(0, 150, 25)) +
  theme(
    axis.text = element_text(colour = "black", size = 11),
    axis.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
    ) +
  labs(
    x = "Ecoregion",
    y = "Latitudinal Range (°)"
  )


# ecoregion checks: average mid latitude
ecoplot2 <- ggplot(dists, aes(x = region, y = AVml)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  geom_jitter() +
  theme_bw() +
  scale_y_continuous(limits = c(-75, 75), 
                     breaks = seq(-75, 75, 25)) +
  theme(
    axis.text = element_text(colour = "black", size = 11),
    axis.title = element_text(size = 14)
  ) +
  labs(
    x = "Ecoregion",
    y = "Average Midpoint (°)"
  )


plot_grid(ecoplot1, ecoplot2,
          nrow = 2, 
          align = "v",
          labels = c("A", "B"))



