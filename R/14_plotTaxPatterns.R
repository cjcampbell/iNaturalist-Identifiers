library(cowplot)
library(ggpubr)

theme_set(theme_pubr(base_size = 8, base_family = 'Roboto'))
idsByUser <- data.table::fread(file.path(wd$out, "idsByUser.csv"))
if(!exists("paletteDeets")) paletteDeets <- readRDS(file.path(wd$bin, "paletteDeets.rds"))


set.seed(42); idsByUser2 <- 
  idsByUser %>% 
  na.omit() %>% 
  mutate(
    n_taxa_IDed  = as.numeric(n_taxa_IDed),
    n_obs_IDed = as.numeric(n_obs_IDed))
set.seed(42); idsByUser2$n_taxa_IDed2  <- idsByUser2$n_taxa_IDed + abs(rnorm(n = nrow(idsByUser2), 0, sd = 0.1))
set.seed(42); idsByUser2$n_obs_IDed2 <- idsByUser2$n_obs_IDed + abs(rnorm(n = nrow(idsByUser2), 0, sd = 0.1))

uniqueTaxa <- idsByUser2 %>% 
  ggplot() + 
  aes(y=n_taxa_IDed, x = n_obs_IDed) +
  geom_bin2d(binwidth = c(0.05,0.05)) +
  #geom_jitter(width = 0.01, height = 0.01, alpha = 0.05) +
  scale_y_log10(
    "Unique taxa identified",
    expand = c(0,0),
    breaks = 10^(0:6),
    labels = c(1, 10, 100, "1k", "10k", "100k", "1M")
  ) +
  scale_x_log10(
    "Identifications",
    expand = c(0,0),
    breaks = 10^(0:6),
    labels = c(1, 10, 100, "1k", "10k", "100k", "1M")
  ) +
  paletteDeets+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(
    panel.grid.major.y = element_line(),
    legend.position = c(0.85,0.1),
    legend.direction = "horizontal"
  ) +
  #geom_smooth(data = idsByUser2 %>% sample_n(10000), aes(y=n_taxa_IDed, x = n_obs_IDed), se=F, method = "loess", color = "grey50") +
   geom_abline(linetype = 2, size = 0.5, slope = 1, intercept = 0, color = "grey20")
(uniqueTaxa)
saveRDS(uniqueTaxa, file = file.path(wd$bin, "uniqueTaxa.rds"))
ggsave(uniqueTaxa, filename = file.path(wd$figs, "obsVtaxa_nohists.png"), width = 6, height = 4)


hist_x <- idsByUser2 %>% 
  ggplot() + 
  aes(n_obs_IDed) +
  geom_histogram(bins = 25) +
  scale_x_log10("") +
  scale_y_log10("") +
  geom_vline(xintercept = median(idsByUser2$n_obs_IDed, na.rm = T), col = "red") +
  theme_void()
hist_y <- idsByUser2 %>% 
  ggplot() + 
  aes(n_taxa_IDed) +
  geom_histogram(bins = 25) +
  scale_x_log10("") +
  scale_y_log10("") +
  geom_vline(xintercept = median(idsByUser2$n_taxa_IDed, na.rm = T), col = "red") +
  theme_void() +
  coord_flip()

align_x_hist <- align_plots(hist_x, uniqueTaxa, align = "v")[[1]]
align_y_hist <- align_plots(hist_y, uniqueTaxa, align = "h")[[1]]

panel1 <- plot_grid(align_x_hist, NULL, uniqueTaxa, align_y_hist,
                    ncol = 2, nrow = 2, rel_heights = c(0.2, 1), rel_widths = c(1, 0.2))
ggsave(panel1, filename = file.path(wd$figs, "obsVtaxa.png"), width = 6, height = 4)

# by id level -------------------------------------------------------------
meanRankLevelByUser <- fread(file.path(wd$out, "meanRankLevelByUser.csv"))
df <- full_join(idsByUser, meanRankLevelByUser, by = c("user.id" = "id_user.id"))
ylabs <- tbl(iNat, "taxonRankLevelInfo") %>% 
  collect %>% 
  group_by(id_taxon.rank_level) %>% 
  slice(1) %>% 
  dplyr::filter(!id_taxon.rank %in% c("stateofmatter","subspecies","subterclass", "parvorder", "tribe","superorder","suborder", "infraorder", "subfamily", "superclass","zoosubsection", "subphylum", "supertribe", "subtribe", "subsection", "complex", "zoosection", "superfamily", "epifamily", "section", "subclass", "infraclass"))

set.seed(42); df$n_IDs2 <- df$n_IDs + abs(rnorm(n = nrow(df), sd = 1))

p_idRankLevel <- df %>% 
  ggplot() +
  geom_bin2d(aes(x=n_IDs2, y = meanRankLevel), binwidth = c(0.02,0.7)) +
  scale_x_log10(
    "Identifications",
    expand = c(0,0),
    breaks = 10^(0:6),
    labels = c(1, 10, 100, "1k", "10k", "100k", "1M")
  ) +
  scale_y_continuous(
    "Mean identification level",
    breaks = ylabs$id_taxon.rank_level,
    labels = ylabs$id_taxon.rank,
    limits = c(NA, 75)
  ) +
  paletteDeets +
  coord_cartesian() +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(
    panel.grid.major.y = element_line(),
    legend.position = c(0.85,0.85),
    legend.direction = "horizontal"
  ) 
(p_idRankLevel)
saveRDS(p_idRankLevel, file = file.path(wd$bin, "p_idRankLevel.rds"))
ggsave(p_idRankLevel, filename = file.path(wd$figs, "idRankLevel.png"), width = 6, height = 4)


# Combine -----------------------------------------------------------------
ggarrange(uniqueTaxa, p_idRankLevel, common.legend = T) %>% 
  ggsave(plot = ., filename = file.path(wd$figs, "idTaxa.png"), width = 8, height = 4)

