library(ggpubr)
theme_set(
  theme_pubr(base_size = 10, base_family = 'Roboto') +
    theme( plot.margin=unit(c(5,5,5,10), 'mm') )
)
cumulativeEvents <- data.table::fread( file.path(wd$out, "cumulativeEventsByDate.csv")) %>% 
  replace(is.na(.), 0) %>% 
  pivot_longer(-yr_precise, values_to = "cum_val", names_to = "metric")

lab_x <- unlist(cumulativeEvents[nrow(cumulativeEvents), "yr_precise"])
lab_ids_y <- unlist(cumulativeEvents[cumulativeEvents$yr_precise == lab_x & cumulativeEvents$metric == "cum_IDs", "cum_val"])/1e6
lab_obs_y <- unlist(cumulativeEvents[cumulativeEvents$yr_precise == lab_x & cumulativeEvents$metric == "cum_obs", "cum_val"])/1e6

mycolors <- c("cum_obs" = "#8C3B2F", "cum_IDs" = "#354858")

# Plot vision ------------------------------------------------------------
mynames <- data.frame( name = c("n_novision-0",
                                "n_novision-1",
                                "n_vision-0"  ,
                                "n_vision-1"  ), 
                       lab = c("ID for others",
                               "ID for self",
                               "CV-ID for others",
                               "CV-ID for self"),
                       order = c(1,2,3,4))

df <- readRDS(file.path(wd$bin, "prop_vision.rds")) %>% 
  dplyr::mutate(
    yr = floor(yr_precise),
    name = paste(name, is_observer, sep = "-")
  ) %>% 
  group_by(yr, is_observer, name) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  dplyr::filter(yr < 2022) %>% 
  dplyr::mutate(name = factor(name, levels = rev(mynames$name)))

cols <- c(
  "n_novision-0" = "#4D9078",
  "n_novision-1" = "#5FAD56",
  "n_vision-0"   = "#B68CB8",
  "n_vision-1"   = "#EFBDEB"
)
mylabs <- df %>%
  group_by(name) %>% 
  arrange(desc(yr)) %>% 
  slice(1) %>% 
  ungroup %>%
  arrange(desc(value)) %>% 
  left_join(., mynames) %>% 
  arrange(order) %>% 
  group_by(is_observer) %>% 
  dplyr::mutate(
    name = factor(name, levels = rev(mynames$name)),
    y = cumsum(value)-(value/2)) %>% 
  dplyr::mutate(
    forWho = case_when(
      is_observer == 0 ~ "Identifications for others", 
      TRUE ~ "Identifications for own observations"
    ),
    forWho = factor(forWho, levels = c("Identifications for own observations",  "Identifications for others"))
  )


p_idsPerYear <- ggplot() +
  geom_area(data = df, aes(x = yr, y = value/1e6, fill =name)) +
  scale_fill_manual(values = cols) +
  geom_text(
    data = mylabs, aes(label = lab, x=2021.25, y = y/1e6),
    hjust = 0, vjust = 0.5) +
  scale_x_continuous(
    "Year", limits = c(2012, 2023),
    breaks = seq(2008, 2021, by = 1)) +
  scale_y_continuous(
    "Identifications per year",expand = c(0,0),
    breaks = seq(0,100, by = 10),
    labels = c(0, paste0(seq(10,100, by = 10),"M"))
  ) +
  theme(
    legend.position = "none"
  ) 
ggsave(p_idsPerYear, filename = file.path(wd$figs, "NumIDsPerYear.png"), width = 8, height = 4)


# Break it out into two plots ------
cols2 <- c(
  "n_novision-0" = "#227F9E",
  "n_novision-1" = "#227F9E",
  "n_vision-0"   = "#003D52",
  "n_vision-1"   = "#003D52"
  
)

# Also counnt observations per year.
obsByDate <- readRDS(file.path(wd$bin, "obsByDate.rds")) %>% 
  dplyr::mutate(year_floor = floor(yr_precise)) %>% 
  group_by(year_floor) %>% 
  dplyr::summarise(n_obs_yr = sum(n_obs)) %>% 
  dplyr::filter(year_floor < 2022)

obsCol  <- viridisLite::turbo(10)[1]
df2 <- df %>% 
  dplyr::mutate(
    forWho = case_when(
      is_observer == 0 ~ "Identifications for others", 
      TRUE ~ "Identifications for own observations"
    ),
    forWho = factor(forWho, levels = rev(c("Identifications for own observations",  "Identifications for others")))
  )

p_others <- df2 %>% 
  dplyr::filter(is_observer == 0) %>% 
  ggplot() +
  geom_area(aes(x = yr, y = value/1e6, fill =name), position = "stack", alpha = 0.8) +
  scale_fill_manual(values = cols2) +
  scale_x_continuous(
    "Year", limits = c(2008, 2021.5),
    breaks = seq(2008, 2021, by = 1),
    expand = c(0,0)) +
  scale_y_continuous(
    "IDs for others",
    expand = c(0,0),
    breaks = seq(0,150, by = 10),
    labels = c(0, paste0(seq(10,150, by = 10),"M")),
    limits = c(0,40)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 10, vjust = 0),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.8, size = 7, hjust = 0.5),
    panel.grid.major.y = element_line(color = "grey90")
  ) +
  geom_path(obsByDate, mapping = aes(x = year_floor, y = n_obs_yr/1e6),
            color = "grey50", linetype = 1, size = 1.2) +
  geom_path(obsByDate, mapping = aes(x = year_floor, y = n_obs_yr/1e6),
            color = obsCol, linetype = 1, size = 0.5) 
ggsave(p_others, filename = file.path(wd$figs, "p_others.png"), width = 5, height = 5)


p_self <- df2 %>% 
  dplyr::filter(is_observer == 1) %>% 
  ggplot() +
  geom_area(aes(x = yr, y = value/1e6, fill =name), position = "stack", alpha = 0.8) +
  scale_fill_manual(values = cols2) +
  scale_x_continuous(
    "Year", limits = c(2008, 2021.5),
    breaks = seq(2008, 2021, by = 1),
    expand = c(0,0)) +
  scale_y_continuous(
    "IDs for own observations",
    expand = c(0,0),
    breaks = seq(0,150, by = 10),
    labels = c(0, paste0(seq(10,150, by = 10),"M")),
    limits = c(0,40)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 10, vjust = 0),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.8, size = 7, hjust = 0.5),
    panel.grid.major.y = element_line(color = "grey90")
  ) +
  geom_path(obsByDate, mapping = aes(x = year_floor, y = n_obs_yr/1e6),
            color = "grey50", linetype = 1, size = 1.2) +
  geom_path(obsByDate, mapping = aes(x = year_floor, y = n_obs_yr/1e6),
            color = obsCol, linetype = 1, size = 0.5) 

lablabs <- data.frame(
  label = c("no CV used ","CV used ","observations "),
  y = c(13, 23.5, 34.5),
  x = c(2018, 2018.5, 2018.86),
  color = c(cols2[c(1,3)],obsCol)
) %>% 
  dplyr::mutate(
    lablength = case_when(
      label == "CV used " ~ stringr::str_length(label)/2 + 0.5,
      label == "no CV used " ~ stringr::str_length(label)/2 + 0.5,
      TRUE ~ stringr::str_length(label)/2),
    xend = x - lablength,
    x_segment2end = x + 1.5,
    y_segment2end = y-6
  ) 

p_self2 <- p_self +
  geom_text(data = lablabs, aes(x=x,y=y,label=label,color = color),hjust = 1,vjust=-0.1,size = 3) +
  scale_color_identity() +
  geom_segment(data = lablabs, aes(x = x+0.1, xend = xend, y = y, yend = y), color = "grey50") +
  geom_segment(data = lablabs, aes(x = x+0.1, xend = x_segment2end, y = y, yend = y_segment2end), color = "grey50") +
  geom_point(aes(x = 2020.35, y = 27733268/1e6), color = "grey50", shape = 1, size = 3) +
  coord_cartesian(clip = "off")

ggsave(p_self2, filename = file.path(wd$figs, "p_self.png"), width = 5, height = 5)


# decreasing time to RG ---------------------------------------------------
if(!exists("df_timeToRG")) df_timeToRG <- fread(file = file.path(wd$bin, "df_timeToRG.csv")) %>% 
  as.data.frame()
p_timeToRG <- df_timeToRG %>% 
  as.data.frame() %>% 
  select(which(!duplicated(names(df_timeToRG)))) %>% 
  dplyr::group_by(obs_created_at_year) %>% 
  dplyr::filter(obs_created_at_year != 2022) %>% 
  dplyr::summarise(
    meanTimeToRG = mean(timeToRG, na.rm = T),
    medianTimeToRG = median(timeToRG, na.rm = T)) %>% 
  ggplot() +
  geom_col(
    aes(x=obs_created_at_year, y = medianTimeToRG*24),
    fill = "grey30"
  ) +
  scale_x_continuous(
    "Year",
    breaks = 2008:2021,
    limits = c(2007.5, 2021.5)
  ) +
  scale_y_log10(
    "Median time to research grade",
    breaks = c(4/24, 1/2, 1, 7, 30, 365, 365*2)*24, 
    labels = c("4 hr", "12 hr", "1 day", "1 week", "1 month", "1 yr", "2 yrs"),
    expand = c(0,0)
  ) +
  theme_pubr() +
  theme(
    panel.grid.major.y = element_line(color = "grey90"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.8, size = 7, hjust = 0.5)
  )
(p_timeToRG)

# Combine into large plot --------------------------------
# library(gridExtra)
p_prop2       <- readRDS(file.path(wd$bin, "propIDsByUsers-plot.rds"))
p_idsVobs     <- readRDS(file.path(wd$bin, "propIDsByObs-plot.rds"))
p_uniqueTaxa    <- readRDS(file.path(wd$bin, "uniqueTaxa.rds"))
p_idRankLevel <- readRDS(file.path(wd$bin, "p_idRankLevel.rds"))

library(patchwork)

theme_set(
  theme_pubr(base_size = 10, base_family = 'Roboto') +
    theme( 
      plot.margin=unit(c(2,0,0,2), 'mm'),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 9),
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "transparent", color = "transparent")
    )
)

mylegend <- get_legend(p_idsVobs +  guides( fill = guide_colourbar(title.position = "left")) + theme(legend.position = "right", legend.direction = "vertical", legend.title.align = 0.5, legend.title = element_text(angle = 90), plot.margin=unit(c(0,0,0,0), 'mm')))
pp <- 
  # A B C
  {p_self2} + 
  {p_others + theme(plot.margin = margin(0,5,0,0, unit = "pt"))} +
  {p_timeToRG + theme(
    axis.text.y = element_text(size = 7), 
    axis.title = element_text(size = 9),
    plot.margin = margin(0,0,0,0, unit = "pt")
  )} +
  
  # D D D
  {p_prop2 + theme(plot.margin=unit(c(2,0,0,2), 'mm'))} +
  
  # E F G
  {p_idRankLevel + theme(legend.position = "none")} +
  {p_uniqueTaxa  + theme(legend.position = "none")} +
  {p_idsVobs +  
      coord_cartesian(clip = "off") +
      guides( 
        fill = guide_colourbar(
          title.position = "top",
          barwidth = unit(75, units = "pt"),
          barheight = unit(13, units = "pt"))
      ) + 
      theme(legend.position = c(0.35, 1), 
            legend.direction = "horizontal", 
            legend.background = element_rect(fill = "transparent", color = "transparent"),
            legend.title.align = 0.5,
            legend.title = element_text(size = 9), 
            plot.margin=unit(c(0,0,0,0), 'mm')
      ) }  +
  
  plot_layout(
    design = 
      "ABC
      DDD
      EFG",
    widths = c(1,1,1),
    heights = c(1,1.6,1)
  ) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10, face = "bold")) 

ggsave(pp, filename = file.path(wd$figs, "bigplot-patchwork.png"), height = 8, width = 8, bg = "white")







