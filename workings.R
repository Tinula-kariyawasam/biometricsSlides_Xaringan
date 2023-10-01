library(ggplot2)
library(dplyr)
library(tidyr)
library(theme.dpird)
set.seed(82)
n <- 10000
mcHist <- data_frame(Poisson = rpois(n, 3), 
                     NegBinom = rnbinom(n, 5, .5)) %>%
  mutate(Simulation = Poisson + NegBinom) %>%
  gather(Distribution, Value) %>%
  mutate(Distribution = sub("NegBinom", "Negative Binomial", Distribution))

mcSample <- mcHist %>%
  group_by(Distribution) %>%
  slice(1:200) %>%
  gather(Distribution, Value) %>%
  group_by(Distribution) %>%
  mutate(rowNum = row_number(Distribution))



g <- ggplot(mcHist) +
  geom_histogram(aes(Value, ..density.., color = Distribution), 
                 binwidth = 1, alpha = .3, fill = "transparent") +
  facet_grid(~Distribution) +
  coord_cartesian(ylim = c(0, .23)) +
  labs(x = "", y = "Frequency") +
  theme_dpird(colourway = "dpird blue", border = T) + 
  theme(panel.grid = element_blank(), 
        strip.text = element_text(size = 16, hjust  = 0.1),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "#003F51"), panel.background = element_rect(fill = "#003F51"),
        legend.position = "none") +
  geom_text(x = rep(21, 3*n), y = rep(.1, 3*n), 
            label = rep(c("=", "+", ""), each = n), size = 24)


# df[df$iteration %% 10 == 0, ]
# mcSample <- mcSample[mcSample$rowNum %% 8 == 0, ]

for (i in 1:length(unique(mcSample$rowNum))) {
  dataUpdate <- mcSample %>%
    group_by(Distribution) %>%
    filter(rowNum %in% 1:i) %>%
    group_by(Distribution) %>%
    mutate(Last = rowNum == i)
  gUpdate <- g +
    geom_dotplot(data = dataUpdate,
                 aes(Value, fill = Last), color = NA, 
                 binwidth = 1, method = "histodot",
                 dotsize = .6) +
    scale_fill_manual(guide = FALSE, values = c("black", "red")) 
  #gUpdate  
  ggsave(filename = paste0('frames/plot', sprintf("%03d",i) , '.jpg'),
         plot = gUpdate, device = 'jpeg')
}


imgs <- list.files("frames/", full.names = T)

library(magick)
img_list <- lapply(imgs, function(img) {
  image_read(img) 
})

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, delay = 20, )

## view animated image
# img_animated

## save to disk
image_write(image = img_animated,
            path = "mcmc.gif")




getwd()

library(rpart)
library(rpart.plot)
library(animation)

library(MASS)

data(MASS::Boston)



# Function to build and plot a tree to a certain depth
plot_tree_to_depth <- function(depth) {
  fit <- rpart(MASS::Boston$rm ~ ., data = MASS::Boston, control = rpart.control(maxdepth = depth))
  rpart.plot(fit, main = paste("Tree Depth:", depth))
}

# Function to build and plot a tree to a certain depth
plot_tree_to_depth <- function(depth) {
  fit <- rpart(digit ~ ., data = mnist_data, control = rpart.control(maxdepth = depth))
  rpart.plot(fit, main = paste("Tree Depth:", depth))
}

plot_tree_to_depth(1)
plot_tree_to_depth(2)
plot_tree_to_depth(3)
plot_tree_to_depth(4)
plot_tree_to_depth(5)
plot_tree_to_depth(6)


# Use the animation package to create a gif of the growing tree
saveGIF({
  for (i in 1:5) {
    plot_tree_to_depth(i)
    ani.pause()
  }
}, movie.name = "tree_growth.gif", ani.width = 800, ani.height = 600)



### regression tree -------
set.seed(123) # Setting a seed for reproducibility

# Number of rows
n <- 1000

# lwd
lwd <- rnorm(n, mean = 7.2, sd = 4)
lwd[lwd < 0] <- 0
lwd[lwd > 24] <- 24
zero_indices <- sample(1:n, 0.4*n)
lwd[zero_indices] <- 0

# minTemp
minTemp <- rnorm(n, mean = (15+25)/2, sd = (25-15)/6)
minTemp[minTemp < 15] <- 15
minTemp[minTemp > 25] <- 25

# maxTemp
maxTemp <- rnorm(n, mean = (25+35)/2, sd = (35-25)/6)
maxTemp[maxTemp < 25] <- 25
maxTemp[maxTemp > 35] <- 35

# rain
rain <- rnorm(n, mean = 8, sd = 6)
rain[rain < 0] <- 0
rain[rain > 30] <- 30
zero_indices_rain <- sample(1:n, 0.6*n)
rain[zero_indices_rain] <- 0

# Growingseason
Growingseason <- factor(sample(c('season 1', 'season 2', 'season 3'), n, replace = TRUE))

# relativeHumidity
relativeHumidity <- rnorm(n, mean = 80, sd = 15) # Adjusted mean to meet the 70% > 70 criteria
relativeHumidity[relativeHumidity < 40] <- 40
relativeHumidity[relativeHumidity > 100] <- 100

# crop
crop <- factor(sample(c('x', 'y'), n, replace = TRUE))

# month
month <- sample(3:10, n, replace = TRUE)

# Combine into a dataframe
df <- data.frame(lwd, minTemp, maxTemp, rain, Growingseason, relativeHumidity, crop, month)


# Function to build and plot a tree to a certain depth
plot_tree_to_depth <- function(depth) {
  fit <- rpart(lwd ~ ., data = df, control = rpart.control(maxdepth = depth))
  rpart.plot(fit, main = paste("Tree Depth:", depth))
}

plot_tree_to_depth(2)

mod <- rpart(lwd ~ ., df)
summary(mod)
rpart


plot_tree_to_depth <- function(depth) {
  fit <- rpart(MASS::Boston$rm ~ ., data = MASS::Boston, control = rpart.control(maxdepth = depth))
  rpart.plot(fit, main = paste("Tree Depth:", depth))
}
plot_tree_to_depth(5)
ggsave()




library(rstudioapi)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

lapply(1:6, function(i){
  jpeg(filename = paste0('plot', sprintf("%03d",i) , '.jpg'), width = 738, height =464)
  plot_tree_to_depth(i)
  dev.off()
})


imgs <- list.files("regTree/", full.names = T)

library(magick)
# img_list <- lapply(imgs, function(img) {
#   image_read(img) 
# })

img_list <- lapply(imgs, function(img) {
  image_read(img) %>%
    image_background('#003F51')  # Assuming white is the background color to make transparent
})

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, delay = 50)

## view animated image
# img_animated

## save to disk
image_write(image = img_animated,
            path = "regTree.gif")




plot_tree_to_depth <- function(depth) {
  fit <- rpart(MASS::Boston$rm ~ ., data = MASS::Boston, control = rpart.control(maxdepth = depth))
  rpart.plot(fit, main = paste("Tree Depth:", depth), box.palette = c("transparent", "transparent"))
}

lapply(1:6, function(i){
  png(filename = paste0('plot', sprintf("%03d", i), '.png'), width = 738, height = 464, bg = "transparent")
  plot_tree_to_depth(i)
  dev.off()
})


imgs <- list.files(pattern = "plot[0-9]+\\.png", full.names = TRUE)

img_list <- lapply(imgs, function(img) {
  image_read(img) %>%
    image_background('#003F51')  # Change transparent background to #003F51
})

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, delay = 50)

## save to disk
image_write(image = img_animated,
            path = "regTree.gif")
#clustering -------
library(ggplot2)
set.seed(1999) # Set seed for reproducibility

# Cluster 1
n1 <- 200
cluster1 <- data.frame(
  x = rnorm(n1, mean = 4, sd = 2),
  y = rnorm(n1, mean = 3, sd = 3)
)

# Cluster 2
n2 <- 22
cluster2 <- data.frame(
  x = rnorm(n2, mean = 6, sd = 1),
  y = rnorm(n2, mean = 5, sd = 2)
)

# Cluster 3
n3 <- 41
cluster3 <- data.frame(
  x = rnorm(n3, mean = 5, sd = 3),
  y = rnorm(n3, mean = 7, sd = 2)
)

n4 <- 50
cluster4 <- data.frame(
  x = rnorm(n3, mean = 7, sd = 2),
  y = rnorm(n3, mean = 4, sd = 1)
)

# Combine data
data <- rbind(cluster1, cluster2, cluster3, cluster4)


set.seed(123) # Set seed for reproducibility
kmeans_result <- kmeans(data, centers = 6)
data$kmeans <- as.factor(kmeans_result$cluster)



ggplot(data, aes(x, y)) + geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "") + theme_dpird(minor_grid = T) + 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave(filename = "clustering/plot005.png" ,device = "png", bg = "transparent")

ggplot(data, aes(x, y, color = kmeans)) + geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "") + 
  theme_dpird(minor_grid = T) + theme(legend.position = "none") + 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))
ggsave(filename = "clustering/plot006.png" ,device = "png", bg = "transparent")


imgs <- list.files("clustering/", full.names = T)

img_list <- lapply(imgs, function(img) {
  image_read(img) %>%
    image_background('#003F51')  # Flatten image with #003F51 background
})

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, delay = 75)

## save to disk
image_write(image = img_animated,
            path = "clus.gif")


# splines ------
n <- 101
x <- seq(0, 1, length.out = n)
fx <- sin(2 * pi * x)

# generate noisy data
set.seed(1)
y <- fx + rnorm(n, sd = 0.5)

spDF <- data.frame(x = x, y = y)
# plot data and f(x)
ggplot(aes(x,y), data = spDF) + 
  geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "") + theme_dpird(minor_grid = T) + 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))



library(mgcv)

n <- 101
x <- seq(0, 1, length.out = n)
fx <- sin(2 * pi * x)

# generate noisy data
set.seed(1)
y <- fx + rnorm(n, sd = 0.5)

spDF <- data.frame(x = x, y = y)

# Fit models
linear_model <- lm(y ~ x, data = spDF)
quadratic_model <- lm(y ~ x + I(x^2), data = spDF)
cubic_model <- lm(y ~ x + I(x^2) + I(x^3), data = spDF)
quartic_model <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = spDF)

gam_model <- gam(y ~ s(x, bs = "cs"), data = spDF)

nls_model <- nls(y ~ A * sin(B * x + C), data = spDF,
                 start = list(A = 1, B = 2 * pi, C = 0))


# Predict using models
spDF$linear_pred <- predict(linear_model)
spDF$quadratic_pred <- predict(quadratic_model)
spDF$cubic_pred <- predict(cubic_model)
spDF$quartic_pred <- predict(quartic_model)
spDF$gam_pred <- predict(gam_model)
spDF$nls_pred <- predict(nls_model)

library(gganimate)
library(transformr)


# plot data and f(x)
ggplot(aes(x,y, color = "Raw"), data = spDF) + 
  geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "", color = "") + theme_dpird(minor_grid = T) +
  theme(legend.position = "bottom" ,plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_color_manual(values = c("Raw" = "Black"))
  
ggsave(filename = "pred/plot001.png" ,device = "png", bg = "transparent")

ggplot(aes(x,y, color = "Raw"), data = spDF) + 
  geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "", color = "") + theme_dpird(minor_grid = T) +
  geom_line(aes(y = linear_pred, color = "Linear"), size = 2) +
  theme(legend.position = "bottom" ,plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_color_manual(values = c("Linear" = "#D27798"))

ggsave(filename = "pred/plot002.png" ,device = "png", bg = "transparent")

ggplot(aes(x,y, color = "Raw"), data = spDF) + 
  geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "", color = "") + theme_dpird(minor_grid = T) +
  geom_line(aes(y = linear_pred, color = "Linear"), size = 2, alpha = 0.4) +
  geom_line(aes(y = quadratic_pred, color = "Quadratic"), size = 2) +
  theme(legend.position = "bottom" ,plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_color_manual(values = c("Linear" = "#D27798", "Quadratic" = "#7E75AF"))

ggsave(filename = "pred/plot003.png" ,device = "png", bg = "transparent")

ggplot(aes(x,y, color = "Raw"), data = spDF) + 
  geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "", color = "") + theme_dpird(minor_grid = T) +
  geom_line(aes(y = linear_pred, color = "Linear"), size = 2, alpha = 0.4) +
  geom_line(aes(y = quadratic_pred, color = "Quadratic"), size = 2, alpha = 0.4) +
  geom_line(aes(y = cubic_pred, color = "Cubic"), size = 2) + 
  theme(legend.position = "bottom" ,plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_color_manual(values = c("Linear" = "#D27798", "Quadratic" = "#7E75AF", "Cubic" = "#BEFBFF"))

ggsave(filename = "pred/plot004.png" ,device = "png", bg = "transparent")


ggplot(aes(x,y, color = "Raw"), data = spDF) + 
  geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "", color = "") + theme_dpird(minor_grid = T) +
  geom_line(aes(y = linear_pred, color = "Linear"), size = 2, alpha = 0.4) +
  geom_line(aes(y = quadratic_pred, color = "Quadratic"), size = 2, alpha = 0.4) +
  geom_line(aes(y = cubic_pred, color = "Cubic"), size = 2, alpha = 0.4) + 
  geom_line(aes(y = quartic_pred, color = "Quartic"), size = 2) +
  theme(legend.position = "bottom" ,plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_color_manual(values = c("Linear" = "#D27798", "Quadratic" = "#7E75AF", "Cubic" = "#BEFBFF",
                                "Quartic" = "#93705B"))

ggsave(filename = "pred/plot005.png" ,device = "png", bg = "transparent")


ggplot(aes(x,y, color = "Raw"), data = spDF) + 
  geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "", color = "") + theme_dpird(minor_grid = T) +
  geom_line(aes(y = linear_pred, color = "Linear"), size = 2, alpha = 0.4) +
  geom_line(aes(y = quadratic_pred, color = "Quadratic"), size = 2, alpha = 0.4) +
  geom_line(aes(y = cubic_pred, color = "Cubic"), size = 2, alpha = 0.4) + 
  geom_line(aes(y = quartic_pred, color = "Quartic"), size = 2, alpha = 0.4) +
  geom_line(aes(y = gam_pred, color = "GAM"), size = 2) +
  theme(legend.position = "bottom" ,plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_color_manual(values = c("Linear" = "#D27798", "Quadratic" = "#7E75AF", "Cubic" = "#BEFBFF",
                                "Quartic" = "#93705B", "GAM" = "#00AC87"))

ggsave(filename = "pred/plot006.png" ,device = "png", bg = "transparent")

ggplot(aes(x,y, color = "Raw"), data = spDF) + 
  geom_point(size = 3, alpha = 0.7) + labs(x = "", y = "", color = "") + theme_dpird(minor_grid = T) +
  geom_line(aes(y = linear_pred, color = "Linear"), size = 2, alpha = 0.4) +
  geom_line(aes(y = quadratic_pred, color = "Quadratic"), size = 2, alpha = 0.4) +
  geom_line(aes(y = cubic_pred, color = "Cubic"), size = 2, alpha = 0.4) + 
  geom_line(aes(y = quartic_pred, color = "Quartic"), size = 2, alpha = 0.4) +
  geom_line(aes(y = gam_pred, color = "GAM"), size = 2, alpha = 0.4) +
  geom_line(aes(y = nls_pred, color = "NLS"), size = 2) +
  theme(legend.position = "bottom" ,plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_color_manual(values = c("Linear" = "#D27798", "Quadratic" = "#7E75AF", "Cubic" = "#BEFBFF",
                                "Quartic" = "#93705B", "GAM" = "#00AC87", "NLS" = "#46652B")) + 
  guides(color = guide_legend(ncol = 6))

ggsave(filename = "pred/plot007.png" ,device = "png", bg = "transparent")


imgs <- list.files("pred/", full.names = T)

img_list <- lapply(imgs, function(img) {
  image_read(img) %>%
    image_background('#003F51')  # Flatten image with #003F51 background
})

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, delay = 75)

## save to disk
image_write(image = img_animated,
            path = "pred.gif")

