df <- data.frame(
  Batter = rep("Chase Dawson", 9),
  Variable = c("Avg Exit Velocity", "Max Exit Velocity", "HardHit%", "xwOBA", "xBA", "xSLG", "Barrel %", "K%", "BB%"),
  Percentile = round(seq(10, 100, length.out = 9))
) %>%
  mutate(Variable = factor(Variable))


df2 <- df %>% mutate(facet_col = rep(1:3, 3) )
library(tidyverse)
library(plotly)
# library(ggtext)
# library(extrafont)
ggplotly(
)

ggplot(df %>%
         mutate(Variable = factor(Variable))) +
  geom_segment(aes(x = 0, xend = 100, y = Variable, yend = Variable), color = "grey", size = 1.5) +
  geom_point(aes(x = 0, y = Variable), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(aes(x = 50, y = Variable), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(aes(x = 100, y = Variable), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(data = df, 
             aes(Percentile, Variable, fill = Percentile), 
             size = 10, alpha = 1, shape = 21, stroke = 1) +
  scale_fill_gradient2(midpoint = 50, low = "#3a64af", mid = "lightgrey", high = "#d82129") +
  # scale_fill_gradient(low = "#3a64af",high = "#d82129") +
  xlim(0,102) +
  geom_text(aes(x = Percentile, y = Variable, label = Percentile), size = 4, fontface = 'bold',  
            color = ifelse(df$Percentile > 20 & df$Percentile < 80, "black", "white")) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")  +
  labs(y='')

p <- ggplot(df2) +
  geom_segment(aes(x = 0, xend = 100, y = Variable, yend = Variable), color = "grey", size = 1.5) +
  geom_point(aes(x = 0, y = Variable), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(aes(x = 50, y = Variable), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(aes(x = 100, y = Variable), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(data = df, 
             aes(Percentile, Variable, fill = Percentile), 
             size = 10, alpha = 1, shape = 21, stroke = 1) +
  scale_fill_gradient2(midpoint = 50, low = "#3a64af", mid = "lightgrey", high = "#d82129") +
  # scale_fill_gradient(low = "#3a64af",high = "#d82129") +
  xlim(-5,105) +
  geom_text(aes(x = Percentile, y = Variable, label = Percentile), size = 4, fontface = 'bold',  
            color = ifelse(df2$Percentile > 20 & df2$Percentile < 80, "black", "white")) +
#   ,
# strip.text = element_blank()
  facet_wrap(~ interaction(y = Variable), scales='free_y') +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_blank() ,
        strip.text = element_text(size = 15, face = 'bold')
        )  
ggplotly(p)
# ---------------- FRONTIER HITTERS  ------------
h <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/frontier_all_hitters23.csv") %>%
  filter(!NAME %in% c('TOTALS'),
         !grepl('Tie-breaker',NAME))%>%
  rename(`BB%` = 'BB.',
         `K%` = 'K.',
         `wRC+` = 'wRC.')
h_ <- h %>% select(NAME, TEAM, AVG, OBP, SLG, `BB%`, `K%`, `wRC+`, wOBA) 

# stats_columns <- c("AVG", "OBP", "SLG", "BB%", "K%", "wRC+", "wOBA", "Avg. Exit Velo", "Max Exit Velo")
# 
# # Calculate percentiles for each player in each stat column
# h_percentiles <- h_ %>%
#   mutate(across(all_of(stats_columns), ~rank(., ties.method = "min") / n() * 100),
#          across(all_of(stats_columns), ~ round(.)))  %>%
#   pivot_longer(cols = starts_with(c("AVG", "OBP", "SLG", "BB%", "K%", "wRC+", "wOBA")),
#                names_to = "Stat",
#                values_to = "Percentile")###
# ---- YT DATA ----
yak <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/clean_yakker23.csv") 

h_yak <- yak %>%
  group_by(Batter) %>%
  summarise(`Avg. Exit Velo` = round(mean(ExitSpeed[PitchCall=='InPlay'], na.rm = TRUE)),
            `Max Exit Velo` = round(max(ExitSpeed, na.rm = TRUE)),
            `Hard Hit%` = round((sum(hardhit, na.rm = T) / n())*100),
            `Whiff %` = round((sum(whiff, na.rm = T) / sum(swing,na.rm = T))*100),
            `Chase %` = round((sum(swing, na.rm = T) / sum(in_zone==0,na.rm = T))*100)
                                                
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
         Percentile = ifelse(Stat %in% c("K%", "Whiff %"), 100 - Percentile, Percentile))


hitter <- h_percentiles %>% filter(NAME == "Chase Dawson") 

ggplotly(
ggplot(hitter) +
  geom_segment(aes(x = 0, xend = 100, y = Stat, yend = Stat), color = "grey", size = 1.5) +
  geom_point(aes(x = 0, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(aes(x = 50, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(aes(x = 100, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
  geom_point(data = hitter, 
             aes(Percentile, Stat, fill = Percentile), 
             size = 7, alpha = 1, shape = 21, stroke = 1) +
  scale_fill_gradient2(midpoint = 50, low = "#3a64af", mid = "lightgrey", high = "#d82129") +
  xlim(-5,105) +
  ggtitle("Frontier League Hitter Percentile Rankings")+
  geom_text(aes(x = Percentile, y = Stat, label = Percentile), size = 3, fontface = 'bold',  
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
 

# ---------------- FRONTIER HITTERS  ------------
p <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/frontier_all_pitchers23.csv") %>%
  filter(!NAME %in% c('TOTALS'),
         !grepl('Tie-breaker',NAME))%>%
  rename(`BB%` = 'BB.',
         `K%` = 'K.',
         `FIP-` = 'FIP.')
colnames(p)
p_ <- p %>% select(NAME, TEAM, ERA, `FIP`, vwOBA, `BB%`, `K%`) 

# ---- YT DATA ----
yak <- read.csv("C:/Users/tdmed/OneDrive/_Shiny/_Coop2/clean_yakker23.csv") 

p_yak <- yak %>%
  group_by(Pitcher) %>%
  summarise(`Fastball Velo` = round(mean(RelSpeed[TaggedPitchType %in% c('Fastball', 'Sinker')], na.rm = TRUE),1),
            `Fastball Spin` = round(mean(SpinRate[TaggedPitchType %in% c('Fastball', 'Sinker')], na.rm = TRUE)),
            `Extension` = round(mean(Extension, na.rm = TRUE),1),`Avg. Exit Velo` = round(mean(ExitSpeed[PitchCall=='InPlay'], na.rm = TRUE)),
            `Max Exit Velo` = round(max(ExitSpeed, na.rm = TRUE)),
            `Hard Hit%` = round((sum(hardhit, na.rm = T) / n())*100),
            `Whiff %` = round((sum(whiff, na.rm = T) / sum(swing,na.rm = T))*100),
            `Chase %` = round((sum(swing, na.rm = T) / sum(in_zone==0,na.rm = T))*100)
            
  )
colnames(p_yak)
p_full <- p_ %>% left_join(p_yak, by = c('NAME' = 'Pitcher')) 
colnames(p_full)
p_full_pivot <- p_full %>%
  pivot_longer(cols = starts_with(c('ERA', 'FIP', 'vwOBA', 'BB%', 'K%', "Fastball Velo",  "Fastball Spin",
                                    "Extension" ,     "Avg. Exit Velo", "Max Exit Velo",  "Hard Hit%",      "Whiff %" ,"Chase %"       )),
               names_to = "Stat",
               values_to = "Value")

p_stats_columns <- c("ERA", "FIP", "vwOBA", "BB%", "K%", "Fastball Velo","Fastball Spin",  "Extension",
                   "Avg. Exit Velo", "Max Exit Velo",  "Hard Hit%", "Whiff %", "Chase %")

p_percentiles <- p_full %>%
  mutate(across(all_of(p_stats_columns), ~rank(., ties.method = "min") / n() * 100),
         across(all_of(p_stats_columns), ~ round(.)))  %>%
  pivot_longer(cols = starts_with(c("ERA", "FIP", "vwOBA", "BB%", "K%", "Fastball Velo","Fastball Spin",  "Extension",
                                    "Avg. Exit Velo", "Max Exit Velo",  "Hard Hit%", "Whiff %", "Chase %")),
               names_to = "Stat",
               values_to = "Percentile")  %>%
  full_join(p_full_pivot, by = c("NAME","TEAM","Stat"), relationship = "many-to-many") %>%
  mutate(Stat = factor(Stat, levels = c("ERA", "FIP", "vwOBA", "BB%", "K%", "Fastball Velo","Fastball Spin",  "Extension",
                                        "Avg. Exit Velo", "Max Exit Velo",  "Hard Hit%", "Whiff %", "Chase %")))


pitcher <- p_percentiles %>% filter(NAME == "Merfy Andrew") 

ggplotly(
  ggplot(pitcher) +
    geom_segment(aes(x = 0, xend = 100, y = Stat, yend = Stat), color = "grey", size = 1.5) +
    geom_point(aes(x = 0, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(aes(x = 50, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(aes(x = 100, y = Stat), size = 2.5, alpha = 1, stroke = 0.5, color = 'grey') +
    geom_point(data = pitcher, 
               aes(Percentile, Stat, fill = Percentile), 
               size = 7, alpha = 1, shape = 21, stroke = 1) +
    scale_fill_gradient2(midpoint = 50, low = "#3a64af", mid = "lightgrey", high = "#d82129") +
    xlim(-5,105) +
    ggtitle("Frontier League Pitcher Percentile Rankings")+
    geom_text(aes(x = Percentile, y = Stat, label = Percentile), size = 3, fontface = 'bold',  
              color = ifelse(pitcher$Percentile > 25 & pitcher$Percentile < 75, "black", "white")) +
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
  layout(title = 'Frontier League Pitcher Percentile Rankings\n', plot_bgcolor = "white")

