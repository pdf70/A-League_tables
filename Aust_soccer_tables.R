# Filename: "Aust_soccer_tables.R"

# Reads in previously sourced data from wikipedia of history of all National Soccer League and A-League Men tables
# for the purpose of combined graphs for the handful of teams which played in both competitions.

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
# - none needed, data is taken from previous projects


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
make_graph_aust_soccer = function(team_abbrev) {
  data_for_graph = aust_soccer_tables %>% 
    filter(abbrev == team_abbrev) %>%
    filter(!(Pld == 0 & Qualification == "Withdrew"))
  
  max_teams_in_season = max(data_for_graph$count_teams_div)
  start_yr = min(data_for_graph$season)
  end_yr = max(data_for_graph$season)
  min_yr = min(data_for_graph$yr_end)
  max_yr = max(data_for_graph$yr_end)
  league_name = "NSL & A-League Men"
  team_name = data_for_graph$current_name[1]
  colour_main = data_for_graph$team_colours[1]
  colour_orig = data_for_graph$orig_team_colours[1]
  line_colour = colour_main
  
  #Breaks for background rectangles, other formatting
  # Update these values whenever the no. of teams in the league changes
  rects = data.frame(xstart = c(-Inf, 1980.5, 1983.5, 1986.5, 1987.5, 1994.5, 1995.5, 1996.5, 1998.5, 1999.5, 2001.5,
                                2004.5, 2005.5, 2009.5, 2010.5, 2011.5, 2019.5), 
                     xend = c(1980.5, 1983.5, 1986.5, 1987.5, 1994.5, 1995.5, 1996.5, 1998.5, 1999.5, 2001.5, 2004.5,
                              2005.5, 2009.5, 2010.5, 2011.5, 2019.5, 2020.5),
                     ystart = c(rep(16,17)), yend = c(14, 16, 12, 13, 14, 13, 12, 14, 15, 16, 13, 0,
                                                      8, 10, 11, 10, 11))
  x_intercepts = data_for_graph$yr_end[(data_for_graph$yr_end %% 5) == 0]
  x_intercepts = x_intercepts[!(x_intercepts ==max_yr)]
  
  # Graph of league position
  graph_1 = ggplot(data_for_graph, aes(x = yr_end, y = Pos, group=aust_soccer_stint)) +
    geom_line(linewidth=1.15, colour = data_for_graph$team_colours[1]) +
    geom_point(aes(colour=as.factor(champion), size = as.factor(champion))) +
    scale_colour_manual(values = c(data_for_graph$second_colour[1], data_for_graph$champ_colour[1])) +
    scale_size_manual(values = c(2,4)) +
    
    # axes
    scale_y_continuous(trans = "reverse", expand = c(0,0.1), breaks= pretty_breaks()) +
    scale_x_continuous(breaks= pretty_breaks()) +
    coord_cartesian(xlim = c(min_yr, max_yr), ylim = c(max_teams_in_season, 1)) +
    geom_vline(xintercept=x_intercepts,  linetype="dotted") +
    theme(panel.border = element_rect(fill=NA)) +
    
    # titles
    ggtitle(paste("Position of", team_name, "in", league_name, "from", start_yr, "to", end_yr)) + 
    theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
    labs(x="Year", y="Position") +
    theme(axis.title = element_text(face = "bold")) +
    theme(plot.margin=unit(c(0.5,1,1.5,1.2),"cm")) +
    theme(legend.position = "none") +
    
    # horizontal lines for number of finals teams (approximated as 4 in years pre-1983 when no finals were held)
    {if(min_yr<1984)geom_segment(aes(x = min(yr_end), xend = min(max_yr,1983.5), y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1984)&(max_yr>=1984))geom_segment(aes(x = 1983.5, xend = 1983.5, y = 4.5, yend = 5.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((max_yr>=1984)&(min_yr<1993))geom_segment(aes(x = 1983.5, xend = 1992.5, y = 5.5, yend = 5.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1993)&(max_yr>=1993))geom_segment(aes(x = 1992.5, xend = 1992.5, y = 5.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((max_yr>=1993)&(min_yr<2006))geom_segment(aes(x = 1992.5, xend = 2005.5, y = 6.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<2006)&(max_yr>=2010))geom_segment(aes(x = 2005.5, xend = 2009.5, y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<2010)&(max_yr>2010))geom_segment(aes(x = 2009.5, xend = 2009.5, y = 4.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>2010)geom_segment(aes(x = max(2009.5,min_yr), xend = max(yr_end), y = 6.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)} +
    
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = Inf, ymax = yend+0.1),  # 0.1 for margin
              fill = "white", alpha = 1.0, inherit.aes = FALSE)
  
  graph_1
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Retrieve previous NSL work from:
setwd(output_path) 
load(file = "nsl_tables_raw.Rdata")     # list - "tables"
load(file="nsl_tables.Rdata")

# Retrieve previous ALM work from:
setwd(output_path) 
load(file = "a_league_mens_tables_raw.Rdata")     # list - "tables"
load(file="a_league_mens_tables.Rdata")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
nsl_teams = read_csv("nsl_teams.csv")
a_leagues_teams = read_csv("a_leagues_teams.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations

nsl_tables_adj = nsl_tables %>%
  mutate(aust_soccer_stint = nsl_stint,
         current_name = case_when(                            # to get consistency of team names
           current_name == "Brisbane Lions" ~ "Brisbane Roar",
           current_name == "Newcastle United" ~ "Newcastle Jets",
           TRUE ~ current_name),
         abbrev = case_when(                                 # to get consistency
           abbrev == "BRL" ~ "BRI",
           TRUE ~ abbrev))

a_league_mens_tables_adj = a_league_mens_tables %>%
  mutate(count_teams_div = count_teams,
         aust_soccer_stint = ifelse(abbrev == "BRI", 3, 1),
         team_url = team_url_men)

aust_soccer_tables = nsl_tables_adj %>%
  bind_rows(a_league_mens_tables_adj)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis of Australian soccer (NSL & A-League Men) tables data
# Make all-time league table
aust_soccer_all_time_league_table = group_by(aust_soccer_tables, current_name) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_Bon = sum(pts_bonus, na.rm = TRUE),
            Total_WPen = sum(pts_WPen, na.rm = TRUE),
            Total_Ded = sum(pts_deducted),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            pts_per_game = round(sum(Pts) / sum(Pld), 2),
            win_perc = round(Total_W / Total_Pld * 100, 2),
            count_champions = sum(champion),
            count_runners_up = sum(runners_up),
            count_premiers = sum(premiers),
            conference_winners = sum(conference_winners, na.rm = TRUE),
            count_finals = sum(finals),
            count_1st = sum(Pos == 1),
            count_2nd = sum(Pos == 2),
            count_3rd = sum(Pos == 3),
            count_4th = sum(Pos == 4),
            best = min(Pos),
            count_spoon = sum(wooden_spoon),
            count_relegated = sum(relegated, na.rm = TRUE),
            count_gf = sum(grand_finalist),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(desc(Total_Pts), desc(Total_GD), desc(Total_GF))

# totals by club
club_records = group_by(aust_soccer_tables, current_name) %>%
  summarise(highest_GF = max(GF),
            lowest_GF = min(GF),
            highest_GA = max(GA),
            lowest_GA = min(GA),
            highest_Pts = max(Pts),
            lowest_Pts = min(Pts))

# list of all team abbreviations from both competitions
teams_unique = c("BRI", "PER", "NEW", "ADE")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# run function to produce graph for a specific team
#make_graph_aust_soccer("BRI")     Brisbane Lions / Roar - graph is mostly space
make_graph_aust_soccer("PER")    # Perth Glory
make_graph_aust_soccer("NEW")    # Newcastle United / Jets
make_graph_aust_soccer("ADE")    # Adelaide United 




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format
names(aust_soccer_all_time_league_table) <- gsub(x = names(aust_soccer_all_time_league_table), pattern = "_", replacement = " ") 

setwd(output_path)
save(aust_soccer_tables, file = "aust_soccer_tables.Rdata")
write.csv(aust_soccer_tables, file = "aust_soccer_tables_full.csv")
write.csv(aust_soccer_all_time_league_table, file = "aust_soccer_all_time_league_table.csv")
setwd(path) 


# export multiple graphs
for (i in 1:length(teams_unique)) {
  make_graph_aust_soccer(teams_unique[i])
  setwd(output_path)
  ggsave(paste("performance_chart_aust_soccer_", teams_unique[i], ".png", sep=""))
  ggsave(paste("performance_chart_aust_soccer_", teams_unique[i], ".svg", sep=""))
}
setwd(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End


# To do:


