setwd("~/Desktop/formating data")
# Load libraries
library(tidyverse)
library(ggpubr)  # Load ggpubr package for stat_cor


mydata <- read.csv("male_players.csv")

df_2 <- data.frame(mydata)

df_select <- df_2 %>% 
  filter(player_positions == "GK", fifa_version == "24", fifa_update == "2") %>% 
  select(long_name, age, overall, weight_kg, goalkeeping_speed, goalkeeping_kicking,mentality_aggression, height_cm, mentality_composure, potential, power_jumping, mentality_vision)

df_analysis <- df_select[sample(nrow(df_select), size = 1000),]




plot_2 <- ggplot(df_analysis, aes(x = goalkeeping_kicking, y = overall, color = goalkeeping_speed)) +
  geom_point(aes(size = age)) +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title="goalkeeping_kicking vs. overall", x="goalkeeping_kicking", y="overall")

# Calculate R-squared value
lm_model <- lm(overall ~ goalkeeping_kicking, data = df_analysis)
rsquared <- summary(lm_model)$r.squared

# Add R-squared value as text annotation
plot_2 + annotate("text", x = max(df_analysis$goalkeeping_kicking), y = max(df_analysis$overall), 
                  label = paste("R-squared =", round(rsquared, 3)),
                  hjust = 3.6, vjust = 3, color = "blue")

