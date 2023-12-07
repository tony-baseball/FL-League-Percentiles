library(tidyverse)
library(ggplot2)
library(plotly)
library(fmsb)
# pitch data
yak <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/clean_yakker23.csv") 
# ---------------- FRONTIER HITTERS  ------------
# Season Stats
h <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/frontier_all_hitters23.csv") %>%
  filter(!NAME %in% c('TOTALS', 'Team Totals', 'Opponents'),
         !grepl('Tie-breaker',NAME))%>%
  rename(`BB%` = 'BB.',
         `K%` = 'K.',
         `wRC+` = 'wRC.') 

lg_obp <- sum((h$H+ h$BB + h$HBP), na.rm = T) / sum(h$PA, na.rm = T)
lg_slg <- sum((h$HR*4) + (h$X3B*3) + (h$X2B*2) + (h$X1B*1), na.rm = T) / sum(h$AB, na.rm = T)
lg_ops <- lg_obp + lg_slg

h <- h %>% 
  select(NAME, TEAM, AVG, OBP, SLG, OPS, `BB%`, `K%`, `wRC+`, wOBA) %>%
  mutate(`OPS+` = round((`OPS`/lg_ops)*100), .after=OPS)

# ---- Yakkertech DATA ----
# create rate stats
h_yak <- yak %>%
  group_by(Batter) %>%
  summarise(`Avg. Exit Velo` = round(mean(ExitSpeed[PitchCall=='InPlay' & HitType != 'Bunt'], na.rm = TRUE),1),
            `Max Exit Velo` = round(max(ExitSpeed, na.rm = TRUE),1),
            `Hard Hit%` = round((sum(hardhit, na.rm = T) / sum(PitchCall=='InPlay' & HitType != 'Bunt'& !is.na(ExitSpeed), na.rm = T) )*100,2),
            `Whiff %` = round((sum(whiff, na.rm = T) / sum(swing,na.rm = T))*100,1),
            `Chase %` = round((sum(swing[in_zone==0], na.rm = T) / sum(in_zone==0,na.rm = T))*100,1)
            
  )
# join normal stats and yakkertech stats
h_full <- h %>% left_join(h_yak, by = c('NAME' = 'Batter'))

stats_columns <- c("AVG", "OBP", "SLG", 'OPS', 'OPS+', "BB%", "K%", "wRC+", "wOBA", "Avg. Exit Velo", "Max Exit Velo", 'Hard Hit%', 'Whiff %', "Chase %")

# pivot 
h_full_pivot <- h_full %>%
  pivot_longer(cols = starts_with(c(stats_columns)),
               names_to = "Stat",
               values_to = "Value")

# create percentiles DF
h_percentiles <- h_full %>%
  mutate(across(all_of(stats_columns), ~ (rank(., na.last = "keep") / sum(!is.na(.))) * 100),
         across(all_of(stats_columns), ~ round(.)))  %>%
  pivot_longer(cols = starts_with(c(stats_columns)),
               names_to = "Stat",
               values_to = "Percentile")  %>%
  
  full_join(h_full_pivot, by = c("NAME","TEAM","Stat"), relationship = "many-to-many") %>%
  group_by(Stat) %>%
  mutate(Rank = ifelse(Stat %in% c("BB%", "ERA", 'FIP', 'vwOBA', 'vSLG',"Avg. Exit Velo",  "Hard Hit%"), 
                       dense_rank(Value), dense_rank(desc(Value))),
         UOM = ifelse(Stat %in% c("BB%", "K%", "Whiff %", "Hard Hit%", 'Chase%'), "%",
                      ifelse(Stat %in% c('Max Exit Velo', 'Avg. Exit Velo'), ' mph',
                             ifelse(Stat %in% c('Fastball Spin'), " rpm",
                                    ''))) ) %>%
  mutate(Rank = ifelse(Stat %in% c("K%", "Whiff %", "Chase %"), 
                       dense_rank(Value), dense_rank(desc(Value))) ) %>%
  ungroup() %>%
  mutate(Stat = factor(Stat, levels = c("AVG", "OBP", "SLG", 'OPS','OPS+', "wOBA",
                                        "wRC+", "Avg. Exit Velo", "Max Exit Velo", 'Hard Hit%', 
                                        "BB%", "K%", 'Whiff %', "Chase %")),
         Percentile = ifelse(Stat %in% c("K%", "Whiff %", "Chase %"), 100 - Percentile, Percentile))

# new %ile

df_percentiles <- h_full %>%
  filter(!is.na(AVG)) %>%
  mutate(across(where(is.numeric), ~rank(.)/length(.)*100),
         across(all_of(stats_columns), ~ round(.))) %>%
  pivot_longer(cols = starts_with(c(stats_columns)),
               names_to = "Stat",
               values_to = "Percentile") %>%
  
  full_join(h_full_pivot, by = c("NAME","TEAM","Stat"), relationship = "many-to-many") %>%
  group_by(Stat) %>%
  mutate(Rank = ifelse(Stat %in% c("BB%", "ERA", 'FIP', 'vwOBA', 'vSLG',"Avg. Exit Velo",  "Hard Hit%"), 
                       dense_rank(Value), dense_rank(desc(Value))),
         UOM = ifelse(Stat %in% c("BB%", "K%", "Whiff %", "Hard Hit%", 'Chase%'), "%",
                      ifelse(Stat %in% c('Max Exit Velo', 'Avg. Exit Velo'), ' mph',
                             ifelse(Stat %in% c('Fastball Spin'), " rpm",
                                    ''))) ) %>%
  mutate(Rank = ifelse(Stat %in% c("K%", "Whiff %", "Chase %"), 
                       dense_rank(Value), dense_rank(desc(Value))) ) %>%
  ungroup() %>%
  mutate(Stat = factor(Stat, levels = c("AVG", "OBP", "SLG", 'OPS', 'OPS+', "wOBA",
                                        "wRC+", "Avg. Exit Velo", "Max Exit Velo", 'Hard Hit%', 
                                        "BB%", "K%", 'Whiff %', "Chase %")),
         Percentile = ifelse(Stat %in% c("K%", "Whiff %", "Chase %"), 100 - Percentile, Percentile))


hitter <- df_percentiles %>% filter(NAME == "Chase Dawson") 

ggplotly(
  ggplot(hitter, aes(text = paste0(Stat,": ", Value,
                                   "\n","Rank: ", Rank,"/",max(df_percentiles$Rank, na.rm = T)))) +
    geom_segment(aes(x = 0, xend = 100, y = Stat, yend = Stat), color = "grey", linewidth = 1.5) +
    geom_point(aes(x = 0, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(aes(x = 50, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(aes(x = 100, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(data = hitter, 
               aes(Percentile, Stat, fill = Percentile), 
               size = 8, alpha = 1, shape = 21, stroke = 1) +
    scale_fill_gradient2(midpoint = 50, low = "#3a64af", mid = "lightgrey", high = "#d82129") +
    xlim(-10,110) +
    ggtitle("Frontier League Hitter Percentile Rankings")+
    geom_text(aes(x = Percentile, y = Stat, label = Percentile), size = 4, fontface = 'bold',  
              color = ifelse(hitter$Percentile > 25 & hitter$Percentile < 75, "black", "white")) +
    geom_text(aes(x = 50, y = 1.4, label = paste0(Value, UOM)), size = 3.5, fontface = 'bold',  
              color = 'black') +
    facet_wrap(~ interaction(y = Stat), scales='free_y', ncol = 4) +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          legend.position = "none",
          strip.background = element_blank() ,
          strip.text = element_text(size = 12, face = 'bold')
    )  
) %>%
  layout(title = 'Frontier League Hitter Percentile Rankings\n', plot_bgcolor = "white") %>%  
  layout( xaxis = list(title = ""), yaxis = list(title = ""),
          images = list(  
            list(  
              source =  "https://i.imgur.com/tQZCji9.png",  
              xref = "paper",  
              yref = "paper",  
              x = 0.5,  
              y = 0.05,  
              sizex = 0.25,  
              sizey = 0.25,  
              xanchor="center",  
              yanchor="center" 
            )  
          ) )




# NEW Savant
ggplotly(
    ggplot(hitter, aes(x = Percentile, y = Stat, fill = Percentile , color = Percentile )) +
    geom_bar(stat = "identity",   width = 0.5) +
    geom_segment(aes(x = Percentile, xend = 100, y = Stat, yend = Stat), color = "grey", linewidth = 1) +
    geom_segment(aes(x = 50, xend = 50, y = 0, yend = 12.5), color = "white", alpha = 0.05, linewidth = 1) +
    geom_segment(aes(x = 5, xend = 5, y = 0, yend = 12.5), color = "white", alpha = 0.05, linewidth = 1) +
    geom_segment(aes(x = 95, xend = 95, y = 0, yend = 12.5), color = "white", alpha = 0.05, linewidth = 1) +
    geom_point(aes(Percentile, Stat), color= 'white', fill = 'white', size = 6, alpha = 1, shape = 21, stroke = 0.5) +
    geom_point(aes(Percentile, Stat),  size = 5, alpha = 1, shape = 21, stroke = 0.5) +
    scale_fill_gradient2(midpoint = 50, low = "blue", mid = "lightgrey", high = "red") +
    scale_colour_gradient2(midpoint = 50, low = "blue", mid = "lightgrey", high = "red") +
    scale_y_discrete(limits = \(Stat) rev(Stat)) +
    xlim(0, 110) +
    ggtitle("Frontier League Hitter Percentile Rankings") +
    geom_text(aes(x = Percentile, y = Stat, label = Percentile), size = 3.5, fontface = 'bold',  
              color = ifelse(hitter$Percentile > 25 & hitter$Percentile < 75, "black", "white")) +
    geom_text(aes(x = 110, y = Stat, label = paste0(Value)), size = 3, fontface = 'bold',  hjust = -1,
              color = 'black', ) +
     theme(
      plot.background = element_rect(fill = '#Fafaf3', color = '#Fafaf3' ),
      panel.background = element_rect(fill = '#Fafaf3', color = '#Fafaf3' ),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(hjust = 1, face='bold'),
     legend.position = "none"
    )
) %>%
  layout(title = 'Frontier League Hitter Percentile Rankings\n', plot_bgcolor = "#Fafaf3") %>% 
  layout(paper_bgcolor='#Fafaf3') %>%  
  layout( xaxis = list(title = ""), yaxis = list(title = ""),
          images = list(  
            list(  
              source =  "https://i.imgur.com/tQZCji9.png",  
              xref = "paper",  
              yref = "paper",  
              x = 0.48,  
              y = 0.05,  
              sizex = 0.25,  
              sizey = 0.25,  
              xanchor="center",  
              yanchor="center" 
            ),
            list(  
              source =  "http://silhouettesfree.com/sports/baseball/batter-silhouette-image-9.png",  
              xref = "paper",  
              yref = "paper",  
              x = -0.05,  
              y = 1.1,  
              sizex = 0.1,  
              sizey = 0.1,  
              xanchor="center",  
              yanchor="center" 
            )  
          ) ) %>% 
  style(hoverinfo = 'none') %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))  


# ---------------- FRONTIER PITCHERS  ------------
p <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/frontier_all_pitchers23.csv") %>%
  filter(!NAME %in% c('TOTALS', 'Team Totals', 'Opponents'),
         !grepl('Tie-breaker',NAME))%>%
  rename(`BB%` = 'BB.',
         `K%` = 'K.',
         `FIP-` = 'FIP.') %>% 
  select(NAME, TEAM, ERA, `FIP`, vwOBA,vSLG,  `BB%`, `K%`) %>%
  mutate(`BB%` = `BB%`*100,
         `K%` = `K%`*100)

# ---- YT DATA ----
p_yak <- yak %>%
  group_by(Pitcher) %>%
  summarise(`Fastball Velo` = round(mean(RelSpeed[TaggedPitchType %in% c('Fastball', 'Sinker')], na.rm = TRUE),1),
            `Fastball Spin` = round(mean(SpinRate[TaggedPitchType %in% c('Fastball', 'Sinker')], na.rm = TRUE),1),
            `Hard Hit%` = round((sum(hardhit, na.rm = T) / sum(PitchCall=='InPlay'& !is.na(ExitSpeed), na.rm = T) )*100,1),
            `Whiff %` = round((sum(whiff, na.rm = T) / sum(swing,na.rm = T))*100,1),
            `Avg. Exit Velo` = round(mean(ExitSpeed[PitchCall=='InPlay'], na.rm = TRUE),1),
            `Chase %` = round((sum(swing[in_zone==0], na.rm = T) / sum(in_zone==0,na.rm = T))*100,1)
            
  )

p_full <- p %>% left_join(p_yak, by = c('NAME' = 'Pitcher')) 

p_stats_columns <- c("ERA", "FIP", "vwOBA", "BB%", "K%", "Fastball Velo","Fastball Spin",  "vSLG",
                     "Avg. Exit Velo",  "Hard Hit%", "Whiff %", "Chase %")

p_full_pivot <- p_full %>%
  pivot_longer(cols = starts_with(c(p_stats_columns )),
               names_to = "Stat",
               values_to = "Value")

p_percentiles <- p_full %>%
  mutate(across(all_of(p_stats_columns), ~ (rank(., na.last = "keep") / sum(!is.na(.))) * 100),
         across(all_of(p_stats_columns), ~ round(.)))  %>%
  pivot_longer(cols = starts_with(c(p_stats_columns)),
               names_to = "Stat",
               values_to = "Percentile")  %>%
  full_join(p_full_pivot, by = c("NAME","TEAM","Stat"), relationship = "many-to-many") %>%
  mutate(Stat = factor(Stat, levels = c("ERA", "FIP","vSLG", "vwOBA", "BB%", "K%", "Whiff %", "Chase %", "Fastball Velo","Fastball Spin",  
                                        "Avg. Exit Velo", "Hard Hit%")),
         Percentile = ifelse(Stat %in% c("BB%", "ERA", 'FIP', 'vwOBA', 'vSLG',"Avg. Exit Velo",  "Hard Hit%"), 100 - Percentile, Percentile)) %>%
  group_by(Stat) %>%
  mutate(Rank = ifelse(Stat %in% c("BB%", "ERA", 'FIP', 'vwOBA', 'vSLG',"Avg. Exit Velo",  "Hard Hit%"), 
                       dense_rank(Value), dense_rank(desc(Value))),
         UOM = ifelse(Stat %in% c("BB%", "K%", "Whiff %", "Hard Hit%", 'Chase%'), "%",
                      ifelse(Stat %in% c('Fastball Velo', 'Avg. Exit Velo'), ' mph',
                             ifelse(Stat %in% c('Fastball Spin'), " rpm",
                                    '')))
  ) %>%
  ungroup()

pitcher <- p_percentiles %>% filter(NAME == "Merfy Andrew") 

ggplotly(
  ggplot(pitcher, aes(text = paste0(Stat,": ", Value,
                                    "\n","Rank: ", Rank))) +
    geom_segment(aes(x = 0, xend = 100, y = Stat, yend = Stat), color = "grey", size = 1.5) +
    geom_point(aes(x = 0, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(aes(x = 50, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(aes(x = 100, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(data = pitcher, 
               aes(Percentile, Stat, fill = Percentile), 
               size = 8, alpha = 1, shape = 21, stroke = 1) +
    scale_fill_gradient2(midpoint = 50, low = "#3a64af", mid = "lightgrey", high = "#d82129") +
    xlim(-10,110) +
    ggtitle("Frontier League Pitcher Percentile Rankings")+
    geom_text(aes(x = Percentile, y = Stat, label = Percentile), size = 4, fontface = 'bold',  
              color = ifelse(pitcher$Percentile > 25 & pitcher$Percentile < 75, "black", "white")) +
    geom_text(aes(x = 50, y = 1.4, label = paste0(Value,UOM) ), size = 3.5, fontface = 'bold',  
              color = 'black')+
    facet_wrap(~ interaction(y = Stat), scales='free_y', ncol = 4) +
    theme(plot.title = element_text(hjust = .5),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          legend.position = "none",
          strip.background = element_blank() ,
          strip.text = element_text(size = 12, face = 'bold')
    )  
) %>%
  layout(title = 'Frontier League Pitcher Percentile Rankings\n', plot_bgcolor = "white")%>%  
  layout( xaxis = list(title = ""), yaxis = list(title = ""),
          images = list(  
            list(  
              source =  "https://i.imgur.com/tQZCji9.png",  
              xref = "paper",  
              yref = "paper",  
              x = 0.5,  
              y = 0.05,  
              sizex = 0.25,  
              sizey = 0.25,  
              xanchor="center",  
              yanchor="center" 
            )  
          ) ) 

# ------- RADAR ----------
# h_full_ranks <- h_full %>%
#   mutate(across(all_of(stats_columns), ~rank(., ties.method = "min") / n() * 100),
#          across(all_of(stats_columns), ~ round(.))) %>%
#   distinct(NAME, .keep_all = T)
# 
# max_min <- data.frame(
#   NAME = c('Max','Min'), TEAM = c('Max','Min'), AVG = c(100, 0), OBP = c(100, 0), SLG = c(100, 0), `BB%` = c(100, 0), `K%` = c(100, 0),
#   `wRC+` = c(100, 0), wOBA = c(100,0), `Avg. Exit Velo` = c(100, 0), `Max Exit Velo` = c(100, 0), `Hard Hit%` = c(100, 0),
#   `Whiff %` = c(100, 0),`Chase %` = c(100, 0)
#   )
# rownames(max_min) <- max_min$NAME
# rownames(h_full_ranks) <- h_full_ranks$NAME
# max_min <- max_min %>%
#   rename(`BB%` = 6,
#          `K%` = 7,
#          `wRC+` = 8,
#          "Avg. Exit Velo" = 10,
#          "Max Exit Velo" = 11,
#          'Hard Hit%' = 12, 
#          'Whiff %' = 13, 
#          "Chase %" = 14
#          )
# radar <- rbind(h_full_ranks,max_min)
# radarr <- radar[c("Max", "Min", "Chase Dawson"), ] %>%
#   select(-c(1:2))
# 
# create_beautiful_radarchart <- function(data, color = "#00AFBB", 
#                                         vlabels = colnames(data), vlcex = 0.7,
#                                         caxislabels = NULL, title = NULL, ...){
#   radarchart(
#     data, axistype = 1,
#     # Customize the polygon
#     pcol = color, pfcol = scales::alpha(color, 0.2), plwd = 2, plty = 1,
#     # Customize the grid
#     cglcol = "grey", cglty = 1, cglwd = 0.8,
#     # Customize the axis
#     axislabcol = "black", 
#     # Variable labels
#     vlcex = vlcex, vlabels = vlabels,
#     caxislabels = caxislabels, title = title, ...
#   )
# }
# 
# radarchart(radarr)
# create_beautiful_radarchart(radarr)
# colnames(h_full_ranks)

fl_p_per <- FL_team_pitch23 %>%
  filter(TEAM == 'League Average')%>%
  rename(`BB%` = 'BB.',
         `K%` = 'K.',
         `FIP-` = 'FIP.') %>%
  mutate(ERA = round(ER/IP*9,2)) %>% 
  select(TEAM, ERA, `FIP`, vwOBA, vSLG,  `BB%`, `K%`) %>%
  mutate(`BB%` = `BB%`*100,
         `K%` = `K%`*100)

fl_p_yak <- yak %>%
  summarise(`Fastball Velo` = round(mean(RelSpeed[TaggedPitchType %in% c('Fastball', 'Sinker')], na.rm = TRUE),1),
            `Fastball Spin` = round(mean(SpinRate[TaggedPitchType %in% c('Fastball', 'Sinker')], na.rm = TRUE),1),
            `Hard Hit%` = round((sum(hardhit, na.rm = T) / sum(PitchCall=='InPlay'& !is.na(ExitSpeed), na.rm = T) )*100,1),
            `Whiff %` = round((sum(whiff, na.rm = T) / sum(swing,na.rm = T))*100,1),
            `Avg. Exit Velo` = round(mean(ExitSpeed[PitchCall=='InPlay'], na.rm = TRUE),1),
            `Chase %` = round((sum(swing[in_zone==0], na.rm = T) / sum(in_zone==0,na.rm = T))*100,1)
  ) %>% 
  mutate(TEAM = 'League Average', .before = `Fastball Velo`)

fl_p <- fl_p_per %>%
  left_join(fl_p_yak, by = 'TEAM') %>% pivot_longer(cols = c(2:ncol(.))) %>%
  rename(Stat = 2,
         `FL Avg` = 3)
fl_per_avg <- left_join(pitcher, fl_p %>% select(Stat, `FL Avg`), by = 'Stat')            
# ----

fl_h_per <- FL_team_hit23 %>%
  filter(TEAM == '-')%>%
  rename(`BB%` = 'BB.',
         `K%` = 'K.',
         `wRC+` = 'wRC.') %>%
  mutate(TEAM = 'League Average')%>% 
  select(TEAM, AVG, OBP, SLG, `BB%`, `K%`, `wRC+`, wOBA) 

fl_h_yak <- yak %>%
  summarise(`Avg. Exit Velo` = round(mean(ExitSpeed[PitchCall=='InPlay'], na.rm = TRUE),1),
            `Max Exit Velo` = round(max(ExitSpeed, na.rm = TRUE),1),
            `Hard Hit%` = round((sum(hardhit, na.rm = T) / sum(PitchCall=='InPlay'& !is.na(ExitSpeed), na.rm = T) )*100,2),
            `Whiff %` = round((sum(whiff, na.rm = T) / sum(swing,na.rm = T))*100,1),
            `Chase %` = round((sum(swing[in_zone==0], na.rm = T) / sum(in_zone==0,na.rm = T))*100,1)
  ) %>% 
  mutate(TEAM = 'League Average', .before = `Avg. Exit Velo`)

fl_h <- fl_h_per %>%
  left_join(fl_h_yak, by = 'TEAM') %>% pivot_longer(cols = c(2:ncol(.))) %>%
  rename(Stat = 2,
         `FL Avg` = 3)
fl_h_per_avg <- left_join(hitter, fl_h %>% select(Stat, `FL Avg`), by = 'Stat')            
