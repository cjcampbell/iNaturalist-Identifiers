library(ggpubr)
theme_set(theme_pubr(base_size = 8, base_family = 'Roboto'))
idsByUser <- data.table::fread(file.path(wd$out, "idsByUser.csv"))


# Proportion of IDs from how many users -----------------------------------
library(ggridges)
user_prop_ids <- idsByUser %>% 
  plyr::arrange(desc(n_IDs)) %>% 
  dplyr::mutate( prop_IDs = n_IDs / sum(n_IDs)) %>%
  mutate(cum_IDs = cumsum(prop_IDs)) %>% 
  dplyr::mutate( numUsers = row_number()) %>% 
  dplyr::mutate(user_percentile = 100*numUsers/nrow(idsByUser))

# Percentile details
percentileBreaks <- lapply(c(0.01, 0.05, 0.1, 1, 10, 50, 99), function(x) {
  user_prop_ids %>% dplyr::filter(user_percentile >= x) %>% arrange(user_percentile) %>% slice(1)%>% {data.frame(prop = x, num = .$numUsers)}
}) %>% bind_rows()

# Label details
df <- lapply(c(0.25, 0.50, 0.75, 0.90, 0.99), function(x) {
  user_prop_ids %>% dplyr::filter(cum_IDs > x) %>% slice(1) %>% {data.frame(prop = x, num = .$numUsers, cum_IDs = .$cum_IDs, perc = signif(.$user_percentile, 1))}
}) %>% bind_rows()
df$x         <- 10^(log10(df$num)-0.05)
df$mylab     <- paste0(df$prop*100, "% of IDs made by ",df$num, " users (top ", df$perc,"%)" )
df$linestart <- 10^(log10(df$num) - (stringr::str_length(df$mylab)*.06) )


p_prop2 <- ggplot(user_prop_ids) +
  geom_ridgeline_gradient(aes(x = numUsers, height = cum_IDs, fill = cum_IDs, y = 0)) +
  scale_fill_viridis_c(
    option = "turbo", direction = -1,  limits = c(0,1)
    ) +
  scale_x_log10(
    "Number of Users",
    breaks = c(10, 1e2, 1e3, 1e4, 1e5, 1e6), 
    labels = c(10, 100, "1k", "10k", "100k", "1M") , 
    expand = c(0,0)
    # sec.axis = sec_axis(
    #   ~ .,
    #   name = "User percentiles",
    #   breaks = percentileBreaks$num, 
    #   labels = percentileBreaks$prop
    # )
    ) +
  scale_y_continuous(
    "Proportion Identifications on iNaturalist", 
    expand = expansion(mult = c(0, .1)),
    position = "right",
    limits = c(0,1), breaks = seq(0,1,by=0.25)
    ) +
 # Annnotations
  geom_segment(data = df, aes(x = linestart, xend = num, y = prop, yend = prop, color = cum_IDs )) +
  scale_color_viridis_c(option = "turbo", direction = -1, limits = c(0,1) ) +
  geom_text(
    data = df, 
    aes(x= linestart, y = prop, label = mylab), 
    hjust = -0.1, vjust = -0.5,
    size = 3) +
  geom_point(data = df, aes(x=num, y = prop)) +
  # Theme
  theme(
    legend.position = "none",
  )
(p_prop2)
saveRDS(p_prop2, file = file.path(wd$bin, "propIDsByUsers-plot.rds"))
ggsave(p_prop2, filename = file.path(wd$figs, "propIDsByUsers.png"), width = 6, height = 4)


# IDs v Obs by user -------------------------------------------------------
# Set up color palette.
paletteDeets <- list(
  scale_fill_viridis_c(
    "Users",
    option = "magma",
    trans = scales::pseudo_log_trans(sigma = 0.01),
    breaks = 10^(0:6), labels = c("1", "10", "100", "1k", "10k", "100k", "1M"), limits = c(1,3000)
  )
)
saveRDS(paletteDeets, file = file.path(wd$bin, "paletteDeets.rds"))

# x histogram
ggplot(idsByUser) +
  aes(n_IDs) +
  geom_density() +
  scale_x_log10(limits = c(99,1e6), breaks = c(100,1e3,1e4,1e5,1e6), labels = c(100, "1k", "10k", "100k", "1M"))

# Jitter pts between 1 and 2
set.seed(42)
idsByUser2 <- idsByUser %>% na.omit()
idsByUser2$n_obs <- idsByUser2$n_obs + abs(rnorm(n = nrow(idsByUser), sd = 1))
idsByUser2$n_IDs <- idsByUser2$n_IDs + abs(rnorm(n = nrow(idsByUser), sd = 1))

p_idsVobs <- idsByUser2 %>% 
  ggplot() +
    aes(y=n_obs, x=n_IDs, z = length(unique(user.id))) +
    paletteDeets +
    theme(
      legend.position = "top",
      plot.margin = margin(0,0,0,0, "cm")
    ) +
    stat_summary_2d(
      fun = function(x) length(x),
      binwidth = c(0.05,0.05), aes(color = after_scale(fill))
    ) +
    geom_abline(linetype = 2, size = 0.5, slope = 1, intercept = 0, color = "grey20") +
    scale_y_log10(
      "Observations", 
      limits = c(1,1e6+1), breaks = c(1, 10, 100,1e3,1e4,1e5,1e6)+0.5, labels = c(1, 10, 100, "1k", "10k", "100k", "1M"),
      expand = c(0,0)) +
    scale_x_log10(
      "Identifications", 
      limits = c(1,1e6+1), breaks = c(1, 10, 100,1e3,1e4,1e5,1e6)+0.5, labels = c(1, 10, 100, "1k", "10k", "100k", "1M"),
      expand = c(0,0)) +
    #coord_equal() +
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
    theme(
      legend.position = c(0.3,0.95),
      legend.box.background = element_blank(),
      legend.direction = "horizontal",
      axis.title = element_text(lineheight = .5),
      panel.grid.major.y = element_line(color = "grey90")
    )
(p_idsVobs)
saveRDS(p_idsVobs, file = file.path(wd$bin, "propIDsByObs-plot.rds"))
ggsave(p_idsVobs, filename = file.path(wd$figs, "numberOfIDsVobs.png"), width = 6, height = 4)


# Combine -----------------------------------------------------------------
library(ggpubr)
ggarrange(p_prop2, p_idsVobs, nrow = 1) %>%
  ggsave(plot = ., filename = file.path(wd$figs, "userActivity.png"), width = 8, height = 4)
