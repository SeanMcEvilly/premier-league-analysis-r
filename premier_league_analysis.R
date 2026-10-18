# Exploratory Data Analysis - Premier League 2020/2021
# Name: Sean McEvilly

# Load Required libraries.
library(tidyverse)
library(plotly)

# Import the Premier League dataset.
premierleague <- read.csv("PremierLeague.csv")

# Inspect the data.
head(premierleague)
str(premierleague)
summary(premierleague)
names(premierleague)
dim(premierleague)

# The dataset contains 380 observations (matches) and 16 variables.
# It contains both character and integer variables.
# Team names and dates are stored as characters.
# Match statistics are stored as integers, while FTR is an integer-coded categorical variable.

# Check for any missing values in the dataset.
any(is.na(premierleague))
# Check for missing values in each variable.
colSums(is.na(premierleague))
# There are no missing values in the dataset.

# Question 1: Do home teams win more often?
# Count the number of each full time match result
premierleague%>%
  count(FTR) 
# Home wins = 144, Draws = 83, Away wins = 153.

# Calculate the percentage of each match result.
result_tbl <- premierleague%>%
  count(FTR) %>%
  mutate(
    FTR = case_when(
      FTR == 1 ~ "Home win",
      FTR == 2 ~ "Draw",
      FTR == 3 ~ "Away win"
    ),
    pct = round(n / sum(n) * 100, 2)
    )%>%
      rename(Matches = n)
# Change the order to Home Win - Draw - Away win.
result_tbl$FTR <- factor(result_tbl$FTR,
                         levels = c("Home win", "Draw", "Away win"))
# View table 
result_tbl

# Bar chart of match results in percentages.
resultchart <- ggplot(data = result_tbl, aes(x = FTR, y = pct)) +
  geom_col(colour = "black", fill = c("green", "yellow", "red")) +
  geom_text(aes( y = pct + 3,
                 label = paste0(pct, "%")))+
  ylim(c(0,100))+
  labs(
    title = "Premier League 2020/2021 Match Results",
    x = "Match Result",
    y = "Percentage of Matches (%)") +
  theme_bw()
ggplotly(resultchart)
# Conclusion: 
# Away wins were slightly more common than home wins.
# This suggests there was no clear home advantage based on match results.


# Question 2: Do home teams score more goals than away teams?
# Average Home team goals scored per game rounded to two decimal places.
premierleague %>%
  summarise(average_home_goals = round(mean(FTHG), 2))
# Average Away team goals scored per game rounded to two decimal places.
premierleague %>%
  summarise(average_away_goals = round(mean(FTAG), 2))
# Average Goals: Home = 1.35, Away = 1.34.

# Median Home team goals.
premierleague %>%
  summarise(median_home_goals = median(FTHG))
# Median Away team goals.
premierleague %>%
  summarise(median_away_goals = median(FTAG))
# Median Goals: Home = 1, Away = 1.

# Standard deviation of home goals rounded to two decimal places.
premierleague %>%
  summarise(sd_home_goals = round(sd(FTHG), 2))
# Standard deviation of away goals rounded to two decimal places.
premierleague %>%
  summarise(sd_away_goals = round(sd(FTAG), 2))
# Standard deviation of Goals: Home = 1.32, Away = 1.26.

# IQR of home goals.
premierleague %>%
  summarise(IQR_home_goals = IQR(FTHG))
# IQR of away goals.
premierleague %>%
  summarise(IQR_away_goals = IQR(FTAG))
# IQR of goals: Home = 2, Away = 2.

# Interactive Histogram of home team goals.
home_hist <- ggplot(premierleague, aes(x = FTHG)) +
  geom_histogram(binwidth = 1, colour = "black", fill = "green") +
  labs(
    title = "Distribution of Home Team Goals",
    x = "Home Team Goals",
    y = "Number of Matches") +
  theme_bw()
ggplotly(home_hist)

# Interactive Histogram of away team goals.
away_hist <- ggplot(premierleague, aes(x = FTAG)) +
  geom_histogram(binwidth = 1, colour = "black", fill = "red") +
  labs(
    title = "Distribution of Away Team Goals",
    x = "Away Team Goals",
    y = "Number of Matches") +
  theme_bw()
ggplotly(away_hist)

# Interactive Boxplot of home team goals.
home_box <- ggplot(premierleague, aes(x = "Home", y = FTHG)) +
  geom_boxplot(fill = "green") +
  labs(
    title = "Boxplot of Home Team Goals",
    y = "Home Team Goals",
    x = "Team"
  ) +
  theme_bw()
ggplotly(home_box)

# Interactive Boxplot of away team goals.
away_box <- ggplot(premierleague, aes(x = "Away", y = FTAG)) +
  geom_boxplot(fill = "red") +
  labs(
    title = "Boxplot of Away Team Goals",
    y = "Away Team Goals",
    x = "Team"
  ) +
  theme_bw()
ggplotly(away_box)

# Conclusion: 
# Home and away teams had very similar goal scoring statistics.
# Home teams scored only slightly more goals on average.
# Both teams had the same median and IQR.


# Question 3: Do home teams create more attacking chances than away teams?
# Average home team shots.
premierleague %>%
  summarise(average_home_shots = round(mean(HS), 2))
# Average away team shots.
premierleague %>%
  summarise(average_away_shots = round(mean(AS), 2))
# Average Shots: Home 12.82, Away 11.36.

# Average home team shots on target.
premierleague %>%
  summarise(average_home_shots_target = round(mean(HST), 2))
# Average away team shots on target.
premierleague %>%
  summarise(average_away_shots_target = round(mean(AST), 2))
# Average shots on target: Home 4.54, Away 4.08.

# Bar chart comparing average shots on target.
shots_target <- data.frame(
  Team = c("Home", "Away"),
  Average = c(4.54, 4.08))

shots_target$Team <- factor(shots_target$Team,
                            levels = c("Home","Away"))

shots_target_chart <- ggplot(shots_target,
                             aes(x = Team, y = Average, fill = Team)) +
  geom_col(colour = "black", fill = c("green", "red")) +
  labs(
    title = "Average Shots on Target: Home vs Away",
    x = "Team",
    y = "Average Shots on Target"
  ) +
# Add value at top of each bar
  geom_text(aes(y = Average + 0.2,
                label = Average)) +
  ylim(c(0, 6)) +
  theme_bw() +
  theme(legend.position = "none")

ggplotly(shots_target_chart)

# Conclusion:
# Home teams created more attacking chances than away teams.
# They averaged more total shots and more shots on target.

# Question 4: Are shots on target associated with the match result?
result_shots <- premierleague %>%
  mutate(
    FTR = case_when(
      FTR == 1 ~ "Home win",
      FTR == 2 ~ "Draw",
      FTR == 3 ~ "Away win"
    )) %>%
  group_by(FTR) %>%
  summarise(
    average_home_shots_target = round(mean(HST), 2),
    average_away_shots_target = round(mean(AST), 2))
# Fix order to Home win - Draw - Away win.
result_shots$FTR <- factor(
  result_shots$FTR,
  levels = c("Home win", "Draw", "Away win")
)
# Print Table
result_shots
  
# Bar chart of average shots on target by match result.
result_chart <- result_shots %>%
  pivot_longer(
    cols = c(average_home_shots_target, average_away_shots_target),
    names_to = "Team",
    values_to = "Average"
  ) %>%
  mutate(
    Team = case_when(
      Team == "average_home_shots_target" ~ "Home",
      Team == "average_away_shots_target" ~ "Away"
    )
  )

result_plot <- ggplot(result_chart, 
                       aes(x = FTR, y = Average, fill = Team)) +
  geom_col(position = "dodge",
           colour = "black") +
# Put the figures over the bar chart
  geom_text(aes(y = Average + 0.25, label = Average),
            position = position_dodge(width = 0.9)) +
  ylim(c(0, 7)) +
  labs(
    title = "Average Shots on Target by Match Result",
    x = "Match Result",
    y = "Average Shots on Target"
  ) +
  theme_bw()
result_plot
ggplotly(result_plot)

# Conclusion:
# Winning teams generally recorded more shots on target than losing teams.
# Home teams averaged 6.06 shots on target in home wins compared with 3.13 for away teams.
# In away wins, away teams averaged 5.15 compared with 3.31 for home teams.
# Draws showed the smallest difference between home and away teams.