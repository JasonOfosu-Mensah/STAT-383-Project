setwd("~/Desktop/formating data")

#The original author of this code is 

# Load libraries
library(tidyverse)
library(fmsb)
library(ggplot2)
# Data
name <- c("Iker Casillas","Petr Cech","Victor Valdes","Gianluigi Buffon","Julio Cesar","David",
          "Van de sar","Jan Oblak","Hugo Lloris","Thibaut Courtois",
          "Manuel Neuer","Oliver Kahn")

height <- c(185,196,183,192,170,192,197,188,188,199,193,188)
speed <- c(66,42,61,52,69,58,59,52,63,49,60,60)
passing <- c(74,79,85,76,75,87,75,78,68,72,91,62)
aggression <- c(23,57,47,81,74,38,77,34,31,23,29,95)
points <- c(197,154,82,199,76,75,116,85,96,130,183,99)
year <- c(2010,2010,2011,2007,2010,2018,2007,2019,2018,2018,2016,2007)

df_1 <- data.frame(name,year,height, speed, passing, aggression, points)

# Create a scatter plot with a linear regression line
plot_1 <- ggplot(df_1, aes(x = height, y = points, color = speed)) +
  geom_point(aes(size = aggression)) +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title="height vs. points", x="height", y="points")

# Calculate R-squared value
lm_model <- lm(speed ~ height, data = df_1)
rsquared <- summary(lm_model)$r.squared

# Add R-squared value as text annotation
plot_1 + annotate("text", x = max(df_1$height), y = max(df_1$speed), 
                  label = paste("R-squared =", round(rsquared, 3)),
                  hjust = 1, vjust = 3, color = "blue")




