library(tidyverse)
library(ggplot2)
library(plotly)
library(fmsb)
yak <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/clean_yakker23.csv") %>%
  mutate(Batter = gsub("Ti'q", "Ti'Q", Batter))
# ---------------- FRONTIER HITTERS  ------------
h <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/frontier_all_hitters23.csv") %>%
  filter(!NAME %in% c('TOTALS', 'Team Totals', 'Opponents'),
         !grepl('Tie-breaker',NAME))%>%
  rename(`BB%` = 'BB.',
         `K%` = 'K.',
         `wRC+` = 'wRC.')
h_ <- h %>% select(NAME, TEAM, AVG, OBP, SLG, `BB%`, `K%`, `wRC+`, wOBA) 

# ---- YT DATA ----
h_yak <- yak %>%
  group_by(Batter) %>%
  summarise(`Avg. Exit Velo` = round(mean(ExitSpeed[PitchCall=='InPlay'], na.rm = TRUE),1),
            `Max Exit Velo` = round(max(ExitSpeed, na.rm = TRUE),1),
            `Hard Hit%` = round((sum(hardhit, na.rm = T) / sum(PitchCall=='InPlay'& !is.na(ExitSpeed), na.rm = T) )*100,2),
            `Whiff %` = round((sum(whiff, na.rm = T) / sum(swing,na.rm = T))*100,1),
            `Chase %` = round((sum(swing, na.rm = T) / sum(in_zone==0,na.rm = T))*100,1)
            
  )

h_full <- h_ %>% left_join(h_yak, by = c('NAME' = 'Batter'))


h_full_pivot <- h_full %>%
  pivot_longer(cols = starts_with(c("AVG", "OBP", "SLG", "BB%", "K%", "wRC+", "wOBA",
                                    "Avg. Exit Velo", "Max Exit Velo", 'Hard Hit%', 'Whiff %', "Chase %")),
               names_to = "Stat",
               values_to = "Value")

stats_columns <- c("AVG", "OBP", "SLG", "BB%", "K%", "wRC+", "wOBA", "Avg. Exit Velo", "Max Exit Velo", 'Hard Hit%', 'Whiff %', "Chase %")

h_percentiles <- h_full %>%
  mutate(across(all_of(stats_columns), ~rank(., ties.method = "min") / n() * 100),
         across(all_of(stats_columns), ~ round(.)))  %>%
  pivot_longer(cols = starts_with(c("AVG", "OBP", "SLG", "BB%", "K%", "wRC+", 
                                    "wOBA","Avg. Exit Velo", "Max Exit Velo", 'Hard Hit%', 'Whiff %', "Chase %")),
               names_to = "Stat",
               values_to = "Percentile")  %>%
  full_join(h_full_pivot, by = c("NAME","TEAM","Stat"), relationship = "many-to-many") %>%
  mutate(Stat = factor(Stat, levels = c("AVG", "OBP", "SLG", "wOBA",
                                        "wRC+", "Avg. Exit Velo", "Max Exit Velo", 'Hard Hit%', 
                                        "BB%", "K%", 'Whiff %', "Chase %")),
         Percentile = ifelse(Stat %in% c("K%", "Whiff %", "Chase %"), 100 - Percentile, Percentile))%>%
  group_by(Stat) %>%
  mutate(Rank = ifelse(Stat %in% c("K%", "Whiff %", "Chase %"), 
                       dense_rank(Value), dense_rank(desc(Value)) 
  ) 
  ) %>%
  ungroup()

hitter <- h_percentiles %>% filter(NAME == "Chase Dawson") 

ggplotly(
  ggplot(hitter, aes(text = paste0(Stat,": ", Value,
                                   "\n","Rank: ", Rank,"/",max(h_percentiles$Rank, na.rm = T)))) +
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
  layout(title = 'Frontier League Hitter Percentile Rankings\n', plot_bgcolor = "white")


# ---------------- FRONTIER PITCHERS  ------------
p <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/frontier_all_pitchers23.csv") %>%
  filter(!NAME %in% c('TOTALS', 'Team Totals', 'Opponents'),
         !grepl('Tie-breaker',NAME))%>%
  rename(`BB%` = 'BB.',
         `K%` = 'K.',
         `FIP-` = 'FIP.')

p_ <- p %>% select(NAME, TEAM, ERA, `FIP`, vwOBA,vSLG,  `BB%`, `K%`) %>%
  mutate(`BB%` = `BB%`*100,
         `K%` = `K%`*100)

# ---- YT DATA ----
p_yak <- yak %>%
  group_by(Pitcher) %>%
  summarise(`Fastball Velo` = round(mean(RelSpeed[TaggedPitchType %in% c('Fastball', 'Sinker')], na.rm = TRUE),1),
            `Fastball Spin` = round(mean(SpinRate[TaggedPitchType %in% c('Fastball', 'Sinker')], na.rm = TRUE)),
            `Hard Hit%` = round((sum(hardhit, na.rm = T) / sum(PitchCall=='InPlay'& !is.na(ExitSpeed), na.rm = T) )*100),
            `Whiff %` = round((sum(whiff, na.rm = T) / sum(swing,na.rm = T))*100),
            `Avg. Exit Velo` = round(mean(ExitSpeed[PitchCall=='InPlay'], na.rm = TRUE)),
            `Chase %` = round((sum(swing, na.rm = T) / sum(in_zone==0,na.rm = T))*100)
            
  )

p_full <- p_ %>% left_join(p_yak, by = c('NAME' = 'Pitcher')) 

p_full_pivot <- p_full %>%
  pivot_longer(cols = starts_with(c('ERA', 'FIP', 'vwOBA', 'BB%', 'K%', "Fastball Velo",  "Fastball Spin",
                                    "vSLG" ,     "Avg. Exit Velo",   "Hard Hit%",      "Whiff %" ,"Chase %"       )),
               names_to = "Stat",
               values_to = "Value")


p_stats_columns <- c("ERA", "FIP", "vwOBA", "BB%", "K%", "Fastball Velo","Fastball Spin",  "vSLG",
                     "Avg. Exit Velo",  "Hard Hit%", "Whiff %", "Chase %")

p_percentiles <- p_full %>%
  mutate(across(all_of(p_stats_columns), ~rank(., ties.method = "min") / n() * 100),
         across(all_of(p_stats_columns), ~ round(.)))  %>%
  pivot_longer(cols = starts_with(c("ERA", "FIP", "vwOBA", "BB%", "K%", "Fastball Velo","Fastball Spin",  "vSLG",
                                    "Avg. Exit Velo",  "Hard Hit%", "Whiff %", "Chase %")),
               names_to = "Stat",
               values_to = "Percentile")  %>%
  full_join(p_full_pivot, by = c("NAME","TEAM","Stat"), relationship = "many-to-many") %>%
  mutate(Stat = factor(Stat, levels = c("ERA", "FIP","vSLG", "vwOBA", "BB%", "K%", "Whiff %", "Chase %", "Fastball Velo","Fastball Spin",  
                                        "Avg. Exit Velo", "Hard Hit%")),
         Percentile = ifelse(Stat %in% c("BB%", "ERA", 'FIP', 'vwOBA', 'vSLG',"Avg. Exit Velo",  "Hard Hit%"), 100 - Percentile, Percentile)) %>%
  group_by(Stat) %>%
  mutate(Rank = ifelse(Stat %in% c("BB%", "ERA", 'FIP', 'vwOBA', 'vSLG',"Avg. Exit Velo",  "Hard Hit%"), 
                       dense_rank(Value), dense_rank(desc(Value)) 
  ) 
  ) %>%
  ungroup()



pitcher <- p_percentiles %>% filter(NAME == "Kristian Scott") 

ggplotly(
  ggplot(pitcher, aes(text = paste0(Stat,": ", Value,
                                    "\n","Rank: ", Rank))) +
    geom_segment(aes(x = 0, xend = 100, y = Stat, yend = Stat), color = "grey", size = 1.5) +
    geom_point(aes(x = 0, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(aes(x = 50, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(aes(x = 100, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(data = pitcher, 
               aes(Percentile, Stat, fill = Percentile), 
               size = 7, alpha = 1, shape = 21, stroke = 1) +
    scale_fill_gradient2(midpoint = 50, low = "#3a64af", mid = "lightgrey", high = "#d82129") +
    xlim(-10,110) +
    ggtitle("Frontier League Pitcher Percentile Rankings")+
    geom_text(aes(x = Percentile, y = Stat, label = Percentile), size = 3, fontface = 'bold',  
              color = ifelse(pitcher$Percentile > 25 & pitcher$Percentile < 75, "black", "white")) +
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
  layout(title = 'Frontier League Pitcher Percentile Rankings\n', plot_bgcolor = "white")

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
