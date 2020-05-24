## Final Graphics for Communication


source("analysis.R/analysing_player-data.R")
source("analysis.R/analysing_team-data.R")
source("modelling.R/modelling_player-data.R")
source("modelling.R/modelling_team-data.R")


salary_wins_final <- salary_wins +
  labs(title = "Does a Bigger Budget Translate to More Wins?",
       x = "Salary (Millions)",
       y = "Wins") +
  theme(plot.background = element_rect(fill = "#dfdae3"),
        panel.background = element_rect(fill = "#1c448c"),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  geom_point(colour = "#e398a4") +
  geom_smooth(method = "lm", colour = "#cc0f2f", se = FALSE)
  

teams_model_final <- teams_model +
  labs(title = "Testing Regression Model Accuracy - Wins vs Expected Wins",
       x = "Expected Wins",
       y = "Actual Wins") +
  theme(plot.background = element_rect(fill = "papayawhip"),
        panel.background = element_rect(fill = "orchid4"),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  geom_point(colour = "seashell") +
  geom_abline(linetype = "dashed", colour = "palegreen")


salary_war_final <- salary_war +
  labs(title = "Do Higher Paid Players have a Higher WAR?",
      x = "Salary (Millions)",
      y = "Wins Above Replacement") +
  theme(plot.background = element_rect(fill = "#dfdae3"),
        panel.background = element_rect(fill = "#1c448c"),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  geom_point(colour = "#e398a4") +
  geom_smooth(method = "lm", colour = "#cc0f2f", se = FALSE)


