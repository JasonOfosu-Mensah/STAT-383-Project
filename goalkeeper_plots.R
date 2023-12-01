setwd("C:/Users/JasonFred/OneDrive - clarkson.edu/Desktop/formating data")
# Load libraries
library(tidyverse)
library(ggpubr)  # Load ggpubr package for stat_cor


mydata <- read.csv("male_players.csv")

df_2 <- data.frame(mydata)

df_select <- df_2 %>% 
  filter(player_positions == "GK", fifa_version == "24", fifa_update == "2") %>% 
  select(long_name, age, overall, weight_kg, goalkeeping_speed, goalkeeping_kicking,mentality_aggression, height_cm, mentality_composure, potential, power_jumping, mentality_vision)

df_analysis <- df_select[sample(nrow(df_select), size = 2000),]


#vision
plot_2 <- ggplot(df_analysis, aes(x = mentality_vision, y = overall)) +
  geom_point(color = "magenta4")+
  geom_smooth(method='lm', color="blue", fill = "black") +
  theme_minimal() +
  labs(title="goalkeeping_kicking vs. overall", x="mentality_vision", y="overall")

# Calculate R-squared value
lm_model <- lm(overall ~ mentality_vision, data = df_analysis)
rsquared <- summary(lm_model)$r.squared

# Add R-squared value as text annotation
plot_2 + annotate("text", x = max(df_analysis$mentality_vision), y = max(df_analysis$overall), 
                  label = paste("R-squared =", round(rsquared, 3)),
                  hjust = 2.7, vjust = 2, color = "blue")
#vison_end



