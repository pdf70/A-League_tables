# Filename: "A-League_Womens_tables.R"

# Reads in data from wikipedia of history of all A-league Women's tables
# Note that the format of the input data may change as people change wikipedia entries

# Team colours in HTML format from A-League_kit_colours.R
# Could also source from https://sportsfancovers.com/a-league-color-codes/
# or from https://imagecolorpicker.com/en.

# Retrieve previous work from:
#setwd(output_path) 
#load(file = "a_league_womens_tables_raw.Rdata")     # list - "tables"
#load(file="a_league_womens_tables.Rdata")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries & directories

# Set directory paths
path = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
input_path = paste(path, "/Input", sep="")
output_path = paste(path, "/R_output", sep="")
setwd(path)

# Specify packages (libraries) that are used
library(lubridate)
library(tidyverse)
library(scales)
library(rvest)    # Reading tables from a web page


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parameters 

# From 2008-09 to 2023-24
end_yr = seq(2009, 2024, by = 1)
start_yr = end_yr - 1
seasons_def = paste(start_yr, "-", substr(end_yr,3,4), sep = "")
seasons = case_when(
  seasons_def == "2009-10" ~ "2009",
  seasons_def == "2014-15" ~ "2014",
  TRUE ~ seasons_def)

# Note: need to update this line each year to value of table number in wikipedia for latest season
wiki_table_no = c(rep(2,3), 3, rep(4,5), rep(5,7))
wiki_name = c("_W-League", "_W-League_(Australia)", rep("_W-League", 4), "_W-League_(Australia)",
              rep("_W-League", 6), rep("_A-League_Women",length(seasons)-13))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
make_graph_alw = function(team_abbrev) {
  data_for_graph = a_league_womens_tables %>% 
    filter(abbrev == team_abbrev)
  
  max_teams_in_season = max(data_for_graph$count_teams)
  start_yr = min(data_for_graph$season)
  end_yr = max(data_for_graph$season)
  min_yr = min(data_for_graph$yr_end)
  max_yr = max(data_for_graph$yr_end)
  discont_yr = ifelse(team_abbrev == "CCM", 2010, 2099)
  league_name = "A-League Women"
  
  #Breaks for background rectangles, other formatting
  # Update these values whenever the no. of teams in the league changes
  rects = data.frame(xstart = c(-Inf, 2010.5, 2012.5, 2015.5, 2021.5, 2022.5), 
                     xend = c(2010.5, 2012.5, 2015.5, 2021.5, 2022.5, 2023.5),
                     ystart = c(rep(12,6)), yend = c(8, 7, 8, 9, 10, 11))
  x_intercepts = data_for_graph$yr_end[(data_for_graph$yr_end %% 5) == 0]
  x_intercepts = x_intercepts[!(x_intercepts ==max_yr)]
  
  # Graph of league position
  graph_1 = ggplot(data_for_graph, aes(x = yr_end, y = Pos, group=yr_end<=discont_yr)) +
    geom_line(linewidth=1.15, colour = data_for_graph$team_colours[1]) +
    geom_point(aes(colour=as.factor(champion), size = as.factor(champion))) +
    scale_colour_manual(values = c(data_for_graph$second_colour[1], data_for_graph$champ_colour_alw[1])) +
    scale_size_manual(values = c(2,4)) +
    
    # axes
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = Inf, ymax = yend+0.1),  # 0.1 for margin
              fill = "white", alpha = 1.0, inherit.aes = FALSE) +
    scale_y_continuous(trans = "reverse", expand = c(0,0.1), breaks= pretty_breaks()) +
    scale_x_continuous(breaks= pretty_breaks()) +
    coord_cartesian(xlim = c(min_yr, max_yr), ylim = c(max_teams_in_season, 1)) +
    geom_vline(xintercept=x_intercepts,  linetype="dotted") +
    theme(panel.border = element_rect(fill=NA)) +
    
    # titles
    ggtitle(paste("Position of", data_for_graph$current_name[1], "in", league_name, "from", start_yr, "to", end_yr)) + 
    theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
    labs(x="Year", y="Position") +
    theme(axis.title = element_text(face = "bold")) +
    theme(plot.margin=unit(c(0.5,1,1.5,1.2),"cm")) +
    theme(legend.position = "none") +
    
    # horizontal lines for number of finals teams
    {if(min_yr<2024)geom_segment(aes(x = min(yr_end), xend = min(max_yr,2023.5), y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<2024)&(max_yr>=2024))geom_segment(aes(x = 2023.5, xend = 2023.5, y = 4.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>=2024)geom_segment(aes(x = max(2023.5,min_yr), xend = max(yr_end), y = 6.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)}
  
  graph_1
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in external data
# read all league tables in one loop
# to read a league table manually, see code at bottom, read_html("https://en.wikipedia.org/wiki/2019-20_A-League")
tables = list()
for (j in 1:length(seasons)) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]]  %>% # added to my list
    mutate(season_no = j, season = seasons[j])
  
  if (j%%5==0) print(paste("season = ", seasons[j])) 
}

# Review headers in each of the tables - need consistency of names for combining tables
headers_all = c()
for (j in 1:length(seasons)) {
  header = colnames(tables[[j]])
  headers_all = rbind(header, headers_all)
}

header = colnames(tables[[1]]) %>%
  str_replace("\\.mw-parser.*","")

for (j in 1:length(seasons)) {
  colnames(tables[[j]]) = header
}

# convert from list to data frame
tables_all = do.call(rbind, lapply(tables, as.data.frame))


# read in grand final details, including runners-up
table_premiers_clean = read_html("https://en.wikipedia.org/wiki/A-League_Women_records_and_statistics") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)

table_other = table_premiers_clean[[1]] %>%
  select(c(1,3,5)) %>%
  mutate(Season = sub("-", "-", Season)) %>%
  filter(Season %in% seasons)
colnames(table_other) = c("Season", "Winners", "Runners_up")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
a_leagues_teams = read_csv("a_leagues_teams.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
a_league_womens_tables = tables_all %>% 
  mutate(Team = str_replace(Team, "\\[.*\\]", ""),            # remove text inside square brackets
         champion = ifelse(substr(Team, nchar(Team) - 2, nchar(Team)) == "(C)", 1, 0),
         premiers = ifelse(Pos == 1, 1, 0),
         runners_up = 0,
         finals = ifelse(str_detect(tolower(Qualification), pattern = "finals"), 1, 0),
         Team = str_replace(Team, " \\(C\\)", ""),            # to get consistency in team name
         Pts = as.numeric(str_replace(Pts, "\\[.*\\]", "")),
         pts_per_win = 3,
         pts_per_draw = 1,
         pts_deducted = Pts - (pts_per_win * W + pts_per_draw * D),
         max_avail_pts = Pld * pts_per_win,
         pts_achieved_perc = Pts / max_avail_pts,
         goal_diff = GF - GA,
         GD_prefix = substr(GD,1,1),
         GD_sign = case_when(
           GD_prefix == "+" ~ 1,
           GD_prefix == "-" ~ -1,
           GD_prefix == "0" ~ 1,
           TRUE ~ -1),
         GD_numeric = ifelse(GD_prefix == "0", 0, as.numeric(substr(GD,2,nchar(GD)))) * GD_sign,
         GD_check = GD_numeric - goal_diff, 
         goals_per_game = round(GF / Pld, 2),
         yr_end = as.numeric(substr(season, 1, 4)) + 1,
         wins_for_tiebreak = ifelse(yr_end < 2024, 0, W)) %>%   # competition rules changed 2nd tie-breaker to W from 2023-24.
  group_by(season) %>%
  mutate(count_teams = n(),
         wooden_spoon = ifelse(Pos == max(Pos), 1, 0)) %>%
  ungroup() %>%
  select(Pos:finals, count_teams:wooden_spoon, pts_per_win:wins_for_tiebreak)

# Create a table of team names, including history & past team name changes
teams = as_tibble(unique(a_league_womens_tables$Team))
colnames(teams) = c("previous_name")
teams = teams %>% 
  mutate(current_name = previous_name)
teams$current_name = ifelse(teams$previous_name == "Melbourne Heart", "Melbourne City", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Queensland Roar", "Brisbane Roar", teams$current_name)

teams_all = left_join(teams, a_leagues_teams, by = c("current_name" = "current_name"))

a_league_womens_tables_all = left_join(a_league_womens_tables, teams_all, by = c("Team" = "previous_name"))

# Add additional information of previous season's finishing position
a_league_womens_tables = a_league_womens_tables_all %>%
  arrange(current_name, season_no) %>%
  left_join(table_other, by = c("season" = "Season")) %>%
  mutate(runners_up = ifelse(current_name == Runners_up, 1, 0),
         gf_years = 1,
         grand_finalist = (champion + runners_up) * gf_years,
         prev_pos = ifelse(current_name == lag(current_name), lag(Pos), NA)) %>%
  mutate(next_pos = ifelse(current_name == lead(current_name), lead(Pos), NA)) %>%
  arrange(season_no, Pos) %>%
  mutate(pos_diff = ifelse(is.na(prev_pos), NA, -(Pos - prev_pos)),
         pos_abs_diff = abs(pos_diff)) %>%
  group_by(current_name) %>%
  mutate(cum_champions = cumsum(champion),
         streak_champion = c(ave(c(0, champion), cumsum(c(0, champion) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_champion = c(ave(c(0, champion), cumsum(c(0, champion) > 0), FUN = seq_along) - 1)[-1],
         cum_runners_up = cumsum(runners_up),
         streak_runners_up = c(ave(c(0, runners_up), cumsum(c(0, runners_up) == 0), FUN = seq_along) - 1)[-1],
         cum_premiers = cumsum(premiers),
         streak_premiers = c(ave(c(0, premiers), cumsum(c(0, premiers) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_premiers = c(ave(c(0, premiers), cumsum(c(0, premiers) > 0), FUN = seq_along) - 1)[-1],
         cum_finals = cumsum(finals),
         streak_finals = c(ave(c(0, finals), cumsum(c(0, finals) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_finals = c(ave(c(0, finals), cumsum(c(0, finals) > 0), FUN = seq_along) - 1)[-1],
         cum_grand_finals = cumsum(grand_finalist),
         streak_grand_finals = c(ave(c(0, grand_finalist), cumsum(c(0, grand_finalist) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_grand_finals = c(ave(c(0, grand_finalist), cumsum(c(0, grand_finalist) > 0), FUN = seq_along) - 1)[-1]) %>%
  ungroup() %>%
  mutate(row_number = row_number())


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis of A-League tables data
# Make all-time league table
a_league_womens_all_time_league_table = group_by(a_league_womens_tables, current_name) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            pts_per_game = round(sum(Pts) / sum(Pld), 2),
            count_champions = sum(champion),
            count_runners_up = sum(runners_up),
            count_premiers = sum(premiers),
            count_finals = sum(finals),
            count_1st = sum(Pos == 1),
            count_2nd = sum(Pos == 2),
            count_3rd = sum(Pos == 3),
            count_4th = sum(Pos == 4),
            count_5th = sum(Pos == 5),
            count_6th = sum(Pos == 6),
            best = min(Pos),
            count_spoon = sum(wooden_spoon),
            count_gf = sum(grand_finalist),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(desc(Total_Pts), desc(Total_W), desc(Total_GD), desc(Total_GF))

# champions by final position
champions = filter(a_league_womens_tables, champion == 1)
champions_by_Pos = group_by(champions, Pos) %>%
  summarise(count = n())

# totals by season
season_totals = group_by(a_league_womens_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_Ded = sum(pts_deducted),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            max_ave_goals_scored_team = max(goals_per_game),
            min_ave_goals_scored_team = min(goals_per_game)) %>%
  mutate(ave_goals_scored_game = round(Total_GF / (0.5 * Total_Pld), 1))

title_race_totals = group_by(a_league_womens_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pts_1 = sum(Pts[Pos == 1]),
            Total_Pts_2 = sum(Pts[Pos == 2]),
            Total_GD_1 = sum(goal_diff[Pos == 1]),
            Total_GD_2 = sum(goal_diff[Pos == 2]),
            Total_GF_1 = sum(GF[Pos == 1]),
            Total_GF_2 = sum(GF[Pos == 2])) %>%
  mutate(margin_pts = Total_Pts_1 - Total_Pts_2,
         margin_GD = Total_GD_1 - Total_GD_2,
         margin_GF = Total_GF_1 - Total_GF_2)

# totals by club
club_records = group_by(a_league_mens_tables, current_name) %>%
  summarise(highest_GF = max(GF),
            lowest_GF = min(GF),
            highest_GA = max(GA),
            lowest_GA = min(GA),
            highest_Pts = max(Pts),
            lowest_Pts = min(Pts))

team_streaks = group_by(a_league_womens_tables, current_name) %>%
  summarise(count = n(),
            max_streak_champion = max(streak_champion),
            max_streak_missed_champion = max(streak_missed_champion),
            max_streak_runners_up = max(streak_runners_up),
            streak_premiers = max(streak_premiers),
            max_streak_missed_premiers = max(streak_missed_premiers),
            max_streak_finals = max(streak_finals),
            max_streak_missed_finals = max(streak_missed_finals),
            max_streak_grand_finals = max(streak_grand_finals),
            max_streak_missed_grand_finals = max(streak_missed_grand_finals)) %>%
  arrange(current_name)

# Records for each team in a season
highest_GF_team = club_records %>%
  left_join(a_league_womens_tables, by = c("current_name" = "current_name",
                                           "highest_GF" = "GF")) %>%
  select(current_name, highest_GF, Pld, season)

lowest_GF_team = club_records %>%
  left_join(a_league_womens_tables, by = c("current_name" = "current_name",
                                           "lowest_GF" = "GF")) %>%
  select(current_name, lowest_GF, Pld, season)

highest_GA_team = club_records %>%
  left_join(a_league_womens_tables, by = c("current_name" = "current_name",
                                           "highest_GA" = "GA")) %>%
  select(current_name, highest_GA, Pld, season)

lowest_GA_team = club_records %>%
  left_join(a_league_womens_tables, by = c("current_name" = "current_name",
                                           "lowest_GA" = "GA")) %>%
  select(current_name, lowest_GA, Pld, season)

highest_Pts_team = club_records %>%
  left_join(a_league_womens_tables, by = c("current_name" = "current_name",
                                           "highest_Pts" = "Pts")) %>%
  select(current_name, highest_Pts, Pld, season)

lowest_Pts_team = club_records %>%
  left_join(a_league_womens_tables, by = c("current_name" = "current_name",
                                           "lowest_Pts" = "Pts")) %>%
  select(current_name, lowest_Pts, Pld, season)

# Records for a single season - not adjusted for no. of games
# most & least points
most_pts_season = arrange(a_league_womens_tables, desc(Pts)) %>%
  select(season, Team, Pld, Pts)
head(most_pts_season, 5)

least_pts_season = arrange(a_league_womens_tables, Pts) %>%
  select(season, Team, Pld, Pts)
head(least_pts_season, 5)

# most & least wins
most_wins_season = arrange(a_league_womens_tables, desc(W)) %>%
  select(season, Team, Pld, W)
head(most_wins_season, 5)

least_wins_season = arrange(a_league_womens_tables, W) %>%
  select(season, Team, Pld, W)
head(least_wins_season, 5)

# most & least losses
most_losses_season = arrange(a_league_womens_tables, desc(L)) %>%
  select(season, Team, Pld, L)
head(most_losses_season, 5)

least_losses_season = arrange(a_league_womens_tables, L) %>%
  select(season, Team, Pld, L)
head(least_losses_season, 5)

# most & least draws
most_draws_season = arrange(a_league_womens_tables, desc(D)) %>%
  select(season, Team, Pld, D)
head(most_draws_season, 5)

least_draws_season = arrange(a_league_womens_tables, D) %>%
  select(season, Team, Pld, D)
head(least_draws_season, 5)

# most & least goals scored
most_goals_season = arrange(a_league_womens_tables, desc(GF)) %>%
  select(season, Team, Pld, GF)
head(most_goals_season, 5)

least_goals_season = arrange(a_league_womens_tables, GF) %>%
  select(season, Team, Pld, GF)
head(least_goals_season, 5)

# most & least goals conceded
most_goals_against_season = arrange(a_league_womens_tables, desc(GA)) %>%
  select(season, Team, Pld, GA)
head(most_goals_against_season, 5)

least_goals_against_season = arrange(a_league_womens_tables, GA) %>%
  select(season, Team, Pld, GA)
head(least_goals_against_season, 5)

# best & worst goal difference
best_goals_diff_season = arrange(a_league_womens_tables, desc(goal_diff)) %>%
  select(season, Team, Pld, goal_diff)
head(best_goals_diff_season, 5)

worst_goals_diff_season = arrange(a_league_womens_tables, goal_diff) %>%
  select(season, Team, Pld, goal_diff)
head(worst_goals_diff_season, 5)

# highest & lowest points achieved percentage
highest_pts_perc_season = arrange(a_league_womens_tables, desc(pts_achieved_perc)) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(highest_pts_perc_season, 5)

lowest_pts_perc_season = arrange(a_league_womens_tables, pts_achieved_perc) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(lowest_pts_perc_season, 5)

# most points to not win the league
most_pts_not_premiers_season = arrange(a_league_womens_tables, desc(Pts)) %>%
  filter(premiers == 0) %>%
  select(season, Team, Pld, Pts) 
head(most_pts_not_premiers_season, 5)

# least points to win the league
least_pts_premiers_season = arrange(a_league_womens_tables, Pts) %>%
  filter(premiers == 1) %>%
  select(season, Team, Pld, Pts)
head(least_pts_premiers_season, 5)

# biggest & smallest winning margin in league
most_winning_margin_season = title_race_totals %>%
  arrange(desc(margin_pts), desc(margin_GD), desc(margin_GF)) %>%
  left_join(a_league_womens_tables, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_GD, margin_GF)
head(most_winning_margin_season, 5)

least_winning_margin_season = title_race_totals %>%
  arrange(margin_pts, margin_GD, margin_GF) %>%
  left_join(a_league_womens_tables, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_GD, margin_GF)
head(least_winning_margin_season, 5)

# highest movement in final position
highest_mvmt_up_season = arrange(a_league_womens_tables, desc(pos_diff)) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_up_season, 5)

highest_mvmt_down_season = arrange(a_league_womens_tables, pos_diff) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_down_season, 5)


# lowest position to champion in one season
prev_pos_champion = a_league_womens_tables %>%
  filter(champion == 1) %>%
  select(season, Team, prev_pos) %>%
  arrange(desc(prev_pos), season)
head(prev_pos_champion, 5)

# lowest position after being champion in one season
next_pos_champion = a_league_womens_tables %>%
  filter(champion == 1) %>%
  select(season, Team, next_pos) %>%
  arrange(desc(next_pos), season)
head(next_pos_champion, 5)


# volatility of position from year to year
pos_changes = a_league_womens_tables %>%
  group_by(current_name) %>%
  summarise(count_seasons = n(),
            total_pos_diff = sum(pos_abs_diff, na.rm = TRUE)) %>%
  mutate(ave_mvmt = total_pos_diff / (count_seasons - 1)) %>%
  arrange(desc(ave_mvmt))
pos_changes


# Longest streaks
longest_streaks_champion = arrange(a_league_womens_tables, desc(streak_champion)) %>%
  select(season, Team, streak_champion)
head(longest_streaks_champion, 5)

longest_streaks_missed_champion = arrange(a_league_womens_tables, desc(streak_missed_champion)) %>%
  select(season, Team, streak_missed_champion)
head(longest_streaks_missed_champion, 5)

longest_streaks_runners_up = arrange(a_league_womens_tables, desc(streak_runners_up)) %>%
  select(season, Team, streak_runners_up)
head(longest_streaks_runners_up, 5)

longest_streaks_premiers = arrange(a_league_womens_tables, desc(streak_premiers)) %>%
  select(season, Team, streak_premiers)
head(longest_streaks_premiers, 5)

longest_streaks_missed_premiers = arrange(a_league_womens_tables, desc(streak_missed_premiers)) %>%
  select(season, Team, streak_missed_premiers)
head(longest_streaks_missed_premiers, 5)

longest_streaks_finals = arrange(a_league_womens_tables, desc(streak_finals)) %>%
  select(season, Team, streak_finals)
head(longest_streaks_finals, 5)

longest_streaks_missed_finals = arrange(a_league_womens_tables, desc(streak_missed_finals)) %>%
  select(season, Team, streak_missed_finals)
head(longest_streaks_missed_finals, 5)

longest_streaks_grand_finals = arrange(a_league_womens_tables, desc(streak_grand_finals)) %>%
  select(season, Team, streak_grand_finals)
head(longest_streaks_grand_finals, 5)

longest_streaks_missed_grand_finals = arrange(a_league_womens_tables, desc(streak_missed_grand_finals)) %>%
  select(season, Team, streak_missed_grand_finals)
head(longest_streaks_missed_grand_finals, 5)


# no. of teams in finals
finals_teams = a_league_womens_tables %>% 
  filter(str_detect(tolower(Qualification), pattern = "finals")) %>% 
  group_by(season, yr_end) %>% 
  summarise(finals_teams = max(Pos))

# list of all team abbreviations
teams_unique = unique(a_league_womens_tables$abbrev)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# checks on data for consistency
error_check_pts = a_league_womens_tables %>% 
  filter(!Pts == (pts_per_win * W + pts_per_draw * D))

error_check_pld = a_league_womens_tables %>%
  filter(!Pld == (W + D + L))

error_check_results = season_totals %>%
  filter(!Total_W == Total_L)

error_check_gd_season = season_totals %>%
  filter(!Total_GD == 0)

error_check_gd = a_league_womens_tables %>%
  filter(!(GD_check == 0))

error_check_pos = group_by(a_league_womens_tables, season) %>%
  summarise(count = n(),
            sum_pos = sum(Pos)) %>%
  mutate(exp_sum_pos = count * (count + 1) / 2,
         pos_diff = sum_pos - exp_sum_pos) %>%   # error if calculated difference (pos_diff) is not zero
  filter(!(pos_diff == 0))

error_sorted_pos = a_league_womens_tables %>%
  arrange(season_no, desc(Pts), desc(wins_for_tiebreak), desc(goal_diff), desc(GF)) %>%
  mutate(sorted_row_number = row_number(),
         row_no_diff = row_number - sorted_row_number) %>%
  filter(!(row_no_diff == 0))

check_identical_pos = a_league_womens_tables %>%
  group_by(season_no, Pts, wins_for_tiebreak, goal_diff, GF) %>%
  summarise(count_seasons = n()) %>%
  filter(count_seasons > 1)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# run function to produce graph for a specific team
make_graph_alw("ADE")     # Adelaide United 
make_graph_alw("SYD")     # Sydney FC
make_graph_alw("CCM")     # Central Coast Mariners
make_graph_alw("NEW")     # Newcastle Jets
make_graph_alw("PER")     # Perth Glory
make_graph_alw("BRI")     # Brisbane Roar
make_graph_alw("MVI")     # Melbourne Victory
make_graph_alw("WEL")     # Wellington Phoenix
make_graph_alw("MCI")     # Melbourne City 
make_graph_alw("WSW")     # Western Sydney Wanderers
make_graph_alw("WUN")     # Western United
#make_graph_alw("MAC").    Macarthur FC. Team does not exist for ALW
make_graph_alw("CAN")     # Canberra United


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format
names(a_league_womens_all_time_league_table) <- gsub(x = names(a_league_womens_all_time_league_table), pattern = "_", replacement = " ") 

setwd(output_path)
save(tables, file = "a_league_womens_tables_raw.Rdata")
save(a_league_womens_tables, file = "a_league_womens_tables.Rdata")
write.csv(a_league_womens_tables, file = "a_league_womens_tables_full.csv")
write.csv(a_league_womens_all_time_league_table, file = "a_league_womens_all_time_league_table.csv")
setwd(path) 

# export single graph
#setwd(output_path)
#ggsave("graph_ggsave.pdf")
#setwd(path)

# export multiple graphs
for (i in 1:length(teams_unique)) {
  make_graph_alw(teams_unique[i])
  setwd(output_path)
  #  ggsave(paste("graph_alm_", teams_unique[i], ".pdf", sep=""))
  ggsave(paste("performance_chart_alw_", teams_unique[i], ".png", sep=""))
  ggsave(paste("performance_chart_alw_", teams_unique[i], ".svg", sep=""))
}
setwd(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End


# To do:
# Validate wikipedia data against another source.

# Graph of season_totals data - ave_goals_scored_game, min & max


# Future:
# Auckland FC from 2024-25


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Test
# read one league table manually
table = read_html("https://en.wikipedia.org/wiki/2014_W-League_(Australia)")
tables_all <- table %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)
table_yyyymm = tables_all[[4]]
table_yyyymm