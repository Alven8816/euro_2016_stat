# Load required packages

library(rio)
library(plyr)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(dummies)

#data from http://www.uefa.com/uefaeuro/season=2016/statistics/index.html
players_stat <- read.csv('players_stats.csv',sep = ";")
players_stat$Team <- tolower(players_stat$Team)
team_stat    <- read.csv('team_stats.csv'   ,sep = ";")
team_stat$Team <- tolower(team_stat$Team)
possession_stat    <- read.csv('possession.csv', sep = ";")
possession_stat$team <- tolower(possession_stat$team)
#sum up fouls data from player view to team view
by_team <- group_by(players_stat,Team)
team_sums <- summarise(by_team, sum(Yellow.Cards),
                       sum(Red.Cards),
                       sum(Fouls.Committed),
                       sum(Fouls.Suffered))
# subset columns to plot only the number of fouls committed

fouls_data <- data.frame("region" = team_sums$Team,"value" = team_sums$`sum(Fouls.Committed)`)
# plot 
fouls_plot <- country_choropleth(fouls_data,
                         title  = "number of Fouls Committed by region",
                         legend = "# fouls",
                         num_colors = 1) +
  xlim(-31.266001, 39.869301) +
  ylim(27.636311, 81.008797) +
  coord_map("lambert", lat0 = 27.636311, lat1 = 81.008797)
fouls_plot

#plot the number of yellow cards against country

ggplot(team_sums,aes(x = Team,y = `sum(Yellow.Cards)`, fill = `sum(Yellow.Cards)` )) +
  geom_bar(stat = 'identity') +
  coord_flip()

# merge team data
total_stats <- merge(team_sums,team_stat)

#plot fouls committed against number of wins by country
ggplot(total_stats,aes(x = total_stats$`sum(Fouls.Committed)`,y = total_stats$Wins, label = Team)) + 
  geom_point() +
  geom_text(nudge_y = 0.2) +
  geom_smooth(method = 'lm', formula = y~x)

# plot fouls committed against number of goals against
ggplot(total_stats,aes(x = total_stats$`sum(Fouls.Committed)`,
                       y = total_stats$Total.goals.against, label = Team)) +
  geom_point() +
  geom_text(nudge_y = 0.2) +
  geom_smooth(method = 'lm', formula = y~x)

#remove romania and plot again
total_stats_no_romania <- total_stats[-16,]
 
ggplot(total_stats_no_romania,aes(x = total_stats_no_romania$`sum(Fouls.Committed)`,
                                  y = total_stats_no_romania$Total.goals.against, label = Team)) +
  geom_point() +
  geom_text(nudge_y = 0.2) +
  geom_smooth(method = 'lm',formula = y~x)


# ball possession analysis

#split dataset and merge back in order to have one record for each team
possession_a <- possession_stat[,-c(4,8)]
possession_a$win <- possession_a$score > possession_a$score_
possession_b <- possession_stat[,-c(3,7)]
possession_b$win <- possession_b$score < possession_b$score_
possession_a <- possession_a[,-5]
possession_b <- possession_b[,-4]
colnames(possession_b) <- colnames(possession_a)
possession_tidy <- rbind(possession_a,possession_b)
dummy_win <- as.data.frame(dummy(possession_tidy$win))
possession_tidy$win_dummy <- dummy_win[,2]

#country possession stats

by_team_pos <- group_by(possession_tidy,team)
team_means <- summarise(by_team_pos, mean(possession))
colnames(team_means) <- c("region","value")

possession_plot <- country_choropleth(team_means,
                                 title = "mean ball possession",
                                 legend="% possession",
                                 num_colors=1)+
  xlim(-31.266001, 39.869301)+
  ylim(27.636311, 81.008797) +
  coord_map("lambert", lat0=27.636311, lat1=81.008797)
possession_plot

#logistic regression

ggplot(data = possession_tidy,aes(x = possession)) +
  geom_point(aes(y = win_dummy)) +
  stat_smooth(aes(y = win_dummy),method = "glm")
