setwd("C:/Users/JasonFred/OneDrive - clarkson.edu/Desktop/formating data")
# Load libraries
library(tidyverse)
library(ggpubr)  # Load ggpubr package for stat_cor


mydata <- read.csv("male_players.csv")

df_2 <- data.frame(mydata)

df_select <- df_2 %>% 
  filter(player_positions == "GK", fifa_version == "24", fifa_update == "2") %>% 
  select(long_name, age, overall, weight_kg, goalkeeping_speed,mentality_aggression 
         ,height_cm, mentality_composure, mentality_vision,goalkeeping_kicking,goalkeeping_handling
         ,goalkeeping_diving,goalkeeping_reflexes,goalkeeping_positioning, preferred_foot)

#random sample of 1500 goalkeepers out of the 2045 in filtered dataset
df_analysis <- df_select[sample(nrow(df_select), size = 1500),]


#Function to draw plots, takes in x and y columns and axis titles
draw_plots <- function(x_var,y_var,x_title,y_title) {
  plot_2 <- ggplot(df_analysis, aes(x = x_var, y = y_var)) +
    geom_point(color = "magenta4")+
    geom_smooth(method='lm', color="blue", fill = "black") +
    theme_minimal() +
    labs(title=paste(x_title, y_title, sep = " vs ") , x=x_title, y=y_title)
  
  # vector of R-squared values
  r_squared_values <- c()
  #vector of R values
  r_values <- c()
  
  #loop appends R-squared and R value calculated for x and y to vectors
  for(i in 1:100){
    lm_model <- lm(y_var ~ x_var, data = df_analysis)
    rsquared <- summary(lm_model)$r.squared
    r_squared_values <- c(r_squared_values,rsquared)
    
    r <- cor(x_var, y_var,use = "complete.obs")
    r_values <- c(r_values,r)
    
  }
  
  
  #Mean R-squared and R value annotated to plots 
  plot_2 + annotate("text", x = max(x_var), y = max(y_var), 
                    label = paste("R-squared =", round(mean(r_squared_values), 3)),
                    hjust = 3.9, vjust = 3.5, color = "black")+
    annotate("text", x = max(x_var), y = max(y_var), 
             label = paste("R-value =", round(mean(r_values), 3)),
             hjust = 3.9, vjust = 1.2, color = "gray33")
}

#-----Innate-----------------------------------------------------
#height vs overall
draw_plots(df_analysis$height_cm,df_analysis$overall,"height", "overall")

#composure vs overall
draw_plots(df_analysis$mentality_composure,df_analysis$overall,"composure", "overall")

# aggression vs overall
draw_plots(df_analysis$mentality_aggression,df_analysis$overall,"aggression", "overall")

#vision vs overall
draw_plots(df_analysis$mentality_vision,df_analysis$overall,"vision", "overall")

#pace vs overall
draw_plots(df_analysis$goalkeeping_speed,df_analysis$overall,"pace", "overall")


#-----trainable------------------------------------------------
#kicking vs overall
draw_plots(df_analysis$goalkeeping_kicking,df_analysis$overall,"kicking", "overall")

#reflexes vs overall
draw_plots(df_analysis$goalkeeping_reflexes,df_analysis$overall,"reflexes", "overall")

#diving vs overall
draw_plots(df_analysis$goalkeeping_diving,df_analysis$overall,"diving", "overall")

#handling vs overall
draw_plots(df_analysis$goalkeeping_handling,df_analysis$overall,"handling", "overall")

#positioning vs overall
draw_plots(df_analysis$goalkeeping_positioning,df_analysis$overall,"positioning", "overall")


