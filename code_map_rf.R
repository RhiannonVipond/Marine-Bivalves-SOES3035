library(readr)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(tidyr)
library(forcats)
library(patchwork)
library(randomForest)
library(GGally)
library(rpart)
library(caret)
library(cowplot)
library(ggpubr)

setwd("~/Uni/Year3/Semester2/SOES3035_Research_Training/Assessments/Assignment2_Group_Project/Code")

########################
## Map
########################
# Set limits
ypos <- c(-60., 80.)
xpos <- c(-180, -60)
w <- map_data("worldHires", ylim = ypos, xlim = xpos)


# Plot map
map1 <- ggplot(data = w, aes(x = long, y = lat, group = group)) +
  geom_polygon(
    fill = "seashell",
    colour = "black"
    ) +   # map outline
  geom_hline(
    yintercept = 0, 
    linetype = "dashed", 
    colour = "slategray",
    linewidth = 0.7
    ) +   # equator line
  geom_hline(
    yintercept = 30, 
    linetype = "dashed", 
    colour = "slategray",
    linewidth = 0.5
  ) +   # tropic line 1
  geom_hline(
    yintercept = -30, 
    linetype = "dashed", 
    colour = "slategray",
    linewidth = 0.5
  ) +   # tropic line 2
  ggtitle("A") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11, 
                             colour = "black")
    ) +
  scale_y_continuous(
    breaks = seq(-60, 80, 20),
    limits = c(-60, 80)
    ) +   # y-axis scale
  scale_x_continuous(
    breaks = seq(-180, -60, 30)
    ) +   # x-axis scale
  labs(
    x = "Longitude (°E)",
    y = "Latitude (°N)"
    ) +
  coord_fixed(
    1.2,
    xlim = xpos,  
    ylim = ypos
)


## Richness graph
# Import data
richness <- read.csv("data/edited_lat_matrix.csv")


# Plot richness
map2 <- ggplot() +
  ylab("Latitude (°N)") +
  xlab("Species Richness") +
  geom_point(
    data = richness, 
    aes(x = RICHNESS.TOTAL, y = Latitude), 
    colour = "royalblue",
    size = 2.5,
    alpha = 0.5
    ) +   # data points
  ggtitle("B") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
    ) +
  scale_y_continuous(
    breaks = seq(-60, 80, 20),
    limits = c(-60, 80)
    ) +   # y-axis scale
  geom_hline(
    yintercept = 0, 
    linetype = "dashed", 
    colour = "slategray",
    linewidth = 0.7
    ) +   # equator line
  geom_hline(
    yintercept = 30, 
    linetype = "dashed", 
    colour = "slategray",
    linewidth = 0.5
  ) +   # tropic line 1
  geom_hline(
    yintercept = -30, 
    linetype = "dashed", 
    colour = "slategray",
    linewidth = 0.5
  )   # tropic line 2


map1 + map2   # may take a while to render
# saved as 844x600 px
# added tropic lines


########################
## Random Forest
########################
###################
## Processing
###################
# environment data
env <- read.csv("./data/env_richness.csv")

# visualise
ggplot(env, aes(x = richness)) + 
  geom_histogram() +
  theme_bw()
# clearly not normally distributed
# not required for random forest


# remove ID, Latitude, and ecoregion (categorical)
env_proc <- env[1:92, c(4, 6:18)]

# remove richness to check variable correlation
environmental <- env_proc[, -14]

# check correlations and distributions
pairs.panels(environmental,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             scale = TRUE, #scales correlation font values
             cex.cor = 3, #sets default font size of correlation values
             density = TRUE, # show density plots
             ellipses = FALSE #don't show correlation ellipses
)


# find correlation coefficients
sink("cor_coefficients.txt")
environ.cor = cor(environmental)
environ.cor
cors1 <- environ.cor > 0.9 & environ.cor < 1.0
cors2 <- environ.cor > 0.5 & environ.cor < 0.9
summary(cors1) # find TRUE values where coefficient is above 0.9      
summary(cors2) # find TRUE values where coefficient is above 0.5      
sink()


###################
## Train/Test
###################

# form training and testing sets
trainSet_rf <- round(0.80 * nrow(env_proc))  # 80/20 split
set.seed(1)  # ensures same RNG w/ 1 seed
trainIndex_rf <- sample(nrow(env_proc), trainSet_rf)

trainDF_rf <- env_proc %>% slice(trainIndex_rf)
testDF_rf <- env_proc %>% slice(-trainIndex_rf)


## Decision tree
# make model
rwtree <- rpart(richness ~ ., data = trainDF_rf, method = "anova")
sink("dec_tree_call.txt")
rwtree
sink()
predictedRich <- predict(rwtree, trainDF_rf)


# calculate RMSE for decision tree
errors <- predictedRich - trainDF_rf$richness
decTreeRMSE <- sqrt(mean(errors^2))
sink("dec_tree_RMSE.txt")
decTreeRMSE
sink()

## Tuning Random Forest model
# find optimal mtry
control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3, 
                        search = "random"
                        )
set.seed(2)
mtry <- sqrt(ncol(trainDF_rf))
rf_random <- train(richness~., 
                   data = trainDF_rf, 
                   method = "rf", 
                   metric = "RMSE", 
                   tuneLength = 15, 
                   trControl = control
                   )
sink("mtry_test.txt")
print(rf_random)
sink()
plot(rf_random)
# mtry = 6


## Running model on training set 
set.seed(2)
rwfor <- randomForest(richness ~ ., 
                      data = trainDF_rf, 
                      importance = T, 
                      ntree = 500,
                      mtry = 6)
predQualRF <- predict(rwfor, trainDF_rf) 
rfErrors <- predQualRF - trainDF_rf$richness
rfRMSE <- sqrt(mean(rfErrors^2))
sink("RMSE_train.txt")
rfRMSE  # lower than decision tree RMSE, so outperforms it
sink()

## Running model on testing set
set.seed(2)
rwforTEST <- randomForest(richness ~ ., 
                      data = testDF_rf, 
                      importance = T, 
                      ntree = 500,
                      mtry = 6)
predQualTEST <- predict(rwforTEST, testDF_rf) 
rfErrorsTEST <- predQualTEST - testDF_rf$richness
rfRMSET <- sqrt(mean(rfErrorsTEST^2))


rwforTEST
# large difference likely due to the fairly small number of rows
# 20% of 92 is only 18, random forest relies on having a large data set

###################
## W/O Train/Test
###################

## Decision tree
# make model
rwtree2 <- rpart(richness ~ ., 
                 data = env_proc, 
                 method = "anova"
                 )#
sink("dec_tree_call.txt")
rwtree2
sink()
predictedRich2 <- predict(rwtree2, env_proc)


# calculate RMSE for decision tree
errors2 <- predictedRich2 - env_proc$richness
decTreeRMSE2 <- sqrt(mean(errors2^2))
sink("dec_tree_RMSE")
decTreeRMSE2 # 49.658
sink()

## Tuning Random Forest model
# find optimal mtry
control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3, 
                        search = "random")
set.seed(2)
mtry2 <- sqrt(ncol(env_proc))
rf_random2 <- train(richness~., 
                    data = env_proc, 
                    method = "rf", 
                    metric = "RMSE", 
                    tuneLength = 15, 
                    trControl = control
                    )
sink("rf_mtry.txt")
print(rf_random2)
sink()
plot(rf_random2)
# mtry = 6


## Running RF model 
set.seed(2)
rwfor2 <- randomForest(richness ~ ., 
                       data = env_proc, 
                       importance = T, 
                       ntree = 500,
                       mtry = 6
                       )

# check RMSE
predQualRF2 <- predict(rwfor2, env_proc) 
rfErrors2 <- predQualRF2 - env_proc$richness
rfRMSE2 <- sqrt(mean(rfErrors2^2))
sink("rf_RMSE.txt")
rfRMSE2  # lower than decision tree RMSE (24.48201), so outperforms it
sink()

###################
## Visualisation
###################

# take model values and convert to a data frame
rich_imp <- as.data.frame(importance(rwfor2))

# add variable names
rich_imp$var_names <- c("SST", "Calcite", "Chlorophyll-a", "Dissolved Oxygen", 
                        "Nitrate", "PAR", "Primary Production", "Salinity", 
                        "Silicate", "Phosphate", "Average Tidal Amplitude", 
                        "Rocky Shore Habitat", "pH"
                        )


## plot importance
rich_imp %>%
  mutate(var_names = fct_reorder(var_names, `%IncMSE`)) %>%
  ggplot(aes(x = var_names, y = `%IncMSE`)) +
  geom_segment(
    aes(x = var_names, 
        xend = var_names, 
        y = 0, 
        yend = `%IncMSE`), 
    color = "rosybrown1",
    linewidth = 1
    ) +
  geom_point(
    aes(size = IncNodePurity), 
    color = "royalblue", 
    alpha = 0.7
    ) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 30)) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    plot.title = element_text(hjust = 0.5),
    plot.margin = ggplot2::margin(8, 12, 8, 12, "points")
    ) +
  labs(
    x = "Variable Name",
    size = "Node Purity",
    title = bquote("Pseudo"~R^2~" = 0.91")
  )

rich_imp <- rich_imp[ -c(2, 3, 4, 6, 7, 9, 13), ]

## Partial dependence plots
imp_pd1 <- rownames(rich_imp)[order(rich_imp[, 1], decreasing=TRUE)]
imp_pd2 <- rich_imp$var_names[order(rich_imp[, 1], decreasing=TRUE)]

grid1 <- par(mfrow = c(3, 2))

for (i in seq_along(imp_pd1)) {
  partialPlot(rwfor2, env_proc, imp_pd1[i], 
              main = element_blank(),
              xlab = imp_pd2[i],
              ylab = paste("Partial Dependence"),
              ylim = c(200, 500),
              col = "royalblue")
  }
par(grid1)

