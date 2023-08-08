library(sjPlot)
library(ggpubr)
library(scales)
targetDays <- 365*(1:5)
rerun <- FALSE
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "/srv/gspeedq32/mbelitz/iNatIDers/iNat.db")

moreLikelyCol  <- "#CC2936"
likelyMidCol   <- "#EEF0F2"
lessLikelyCol  <- "#08415C"
slowerCol      <- "#C2AC1E"
speedMidCol    <- "#EEF0F2"
fasterCol      <- "#49976A"

# m1 ----------------------------------------------------------------------
# model 1 used unscaled data, so no need to rescale, etc.
if(!file.exists(file.path(wd$bin, "m1_preds.rds")) | isTRUE(rerun)) {
  if(!exists("m1")) m1 <- readRDS(file.path(wd$bin, "m1.rds"))
  p <- sjPlot::plot_model(m1, type = "pred", terms = "timeActive [all]") %>% 
    ggplot_build()
  saveRDS(p, file = file.path(wd$bin, "model1_predictions.rds"))
  m1_preds <- p %>% 
    {.$data[[1]]} %>% 
    dplyr::filter(x %in% targetDays) %>% 
    dplyr::select(x,y)
  saveRDS(m1_preds, file = file.path(wd$bin, "m1_preds.rds"))
  rm(m1, p)
} else if(!exists("m1_preds")) {
  m1_preds <- readRDS(file.path(wd$bin, "m1_preds.rds"))
}

# Load other models -----
## m2 ----------------------------------------------------------------------

### Load m2 details for sanke plot ----
if(!file.exists(file.path(wd$bin, "m2_preds.rds")) | isTRUE(rerun)) {
  if(!exists("m2")) m2 <- readRDS(file.path(wd$bin, "m2.rds"))
  m2_timeActive_s_attributes <- readRDS(file.path(wd$bin, "m2_timeActive_s_attributes.rds"))
  p <- sjPlot::plot_model(m2, type = "pred", terms = "timeActive_sc [all]") %>% 
    ggplot_build()
  m2_preds <- p %>% 
    {.$data[[1]]} %>% 
    dplyr::mutate(x_rescaled = round(x * m2_timeActive_s_attributes$`scaled:scale` + m2_timeActive_s_attributes$`scaled:center`)) %>% 
    dplyr::filter(x_rescaled %in% targetDays) %>% 
    group_by(x_rescaled) %>% 
    dplyr::summarise(x = max(x_rescaled), y = mean(y)) %>% 
    dplyr::select(x,y)
  saveRDS(m2_preds, file = file.path(wd$bin, "m2_preds.rds"))
  rm(m2)
} else if(!exists("m2_preds")) {
  m2_preds <- readRDS(file.path(wd$bin, "m2_preds.rds"))
}


## Plot m2 results-----
if(!exists("m2")) m2 <- readRDS(file.path(wd$bin, "m2.rds"))

m2_data <- plot_model(m2) %>% 
  ggplot_build()

p_m2 <- ggplot() +
  geom_point(data = dplyr::filter(m2_data$data[[2]], x != 5), aes(x=y,y=x,color=colour), alpha = 0.5, size = 2) +
  geom_segment(data = dplyr::filter(m2_data$data[[3]], x != 5), aes(x=ymin, xend = ymax,y=x,yend = x, color=colour)) +
  scale_color_manual(values = c("#E41A1C" = lessLikelyCol, "#377EB8" = moreLikelyCol)) +
  theme_pubr() +
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Reaches RG") +
  scale_y_continuous(
    limits = c(0.5, 4.5),
    breaks = 1:4,
    labels = rev(c(
      "Computer vision used",
      "More experience\nof all identifiers",
      "Higher taxon rank of\nfirst identification",
      "Higher number of obs.\nin associated biome"
    ))
  ) + 
  ylab(NULL) +
  scale_x_continuous(
    "Odds Ratios",
    limits = c(-0.7, NA)
  ) +
  geom_text(aes(label = "More likely to reach RG →", y = 4.5, x = .2), hjust = 0, size = 3, color = moreLikelyCol) +
  geom_text(aes(label = "← Less likely to reach RG", y = 4.5, x = -.2), hjust = 1,size = 3,  color = lessLikelyCol) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    title = element_text(lineheight = 2),
    axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
    panel.grid.major.x = element_line()
  )


## Plot m3 results-----
if(!exists("m3")) m3 <- readRDS(file.path(wd$bin, "m3.rds"))

m3_data <- plot_model(m3) %>% 
  ggplot_build()

p_m3 <- ggplot() +
  geom_point(data = dplyr::filter(m3_data$data[[2]], x != 6), aes(x=y,y=x,color=colour), alpha = 0.5, size = 2) +
  geom_segment(data = dplyr::filter(m3_data$data[[3]], x != 6), aes(x=ymin, xend = ymax,y=x,yend = x, color=colour)) +
  scale_color_manual(values = c("#E41A1C" = fasterCol, "#377EB8" = slowerCol)) +
  theme_pubr() +
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Time to RG") +
  scale_y_continuous(
    limits = c(0.5, 5.5),
    breaks = 1:5,
    labels = c(
      "Computer vision used",
      "More experience of\n identifier prior to\nhitting RG",
      "Higher taxon rank of\nfirst identification",
      "More obs in genera",
      "More of obs in family"
    )
  ) + 
  ylab(NULL) +
  scale_x_continuous(
    "Estimates", 
    limits = c(-10, NA)
  ) +
  geom_text(aes(label = "More time to RG →", y = 5.5, x = 1), hjust = 0, size = 3, color = slowerCol) +
  geom_text(aes(label = "← Less time to RG", y = 5.5, x = -1), hjust = 1,size = 3,  color = fasterCol) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    title = element_text(lineheight = 2),
    axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
    panel.grid.major.x = element_line()
  )


## Plot model param ests together ------------------------------------------

ggarrange(
  p_m2 + theme(text = element_text(size = 10)),
  p_m3 + theme(text = element_text(size = 10)), 
  ncol = 1) %>% 
  ggsave(plot = ., filename = file.path(wd$figs, "modelEsts.png"), height = 6, width = 6)


# Plot random effects on maps ------

library(sf)
library(ggplot2)

# Setup
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "/srv/gspeedq32/mbelitz/iNatIDers/iNat.db")
ccodes <- geodata::country_codes() %>% dplyr::select(ISO3, UNREGION1)

# Load world data
world <- rnaturalearth::countries110 %>%
  st_as_sf(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  #st_make_valid() %>%
  st_transform('+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs')

# Load obs data
obs_info2 <- tbl(iNat, "obs_info2")
df <- obs_info2 %>% collect()

## Model 2 -- which regions are less likely to hit RG ----
m2 <- readRDS(file = file.path(wd$bin, "m2.rds"))
m2_re <- sjPlot::plot_model(m2, type = "re")
m2_re_dat <- ggplot_build(m2_re)

m2_modRes <- m2_re_dat$data[[3]] %>% 
  cbind(., UNREGION1 = m2_re_dat$layout$panel_params[[1]]$y$get_labels()) %>% 
  left_join(., ccodes) %>% 
  right_join(world,  by = c("ISO3" = "iso_a3") )


### model preds -----
p_modPreds2 <- m2_modRes %>% 
  dplyr::filter(!is.na(UNREGION1)) %>% 
  dplyr::mutate(UNREGION1 = factor(UNREGION1, levels = rev(na.omit(unique(m2_modRes$UNREGION1))))) %>% 
  dplyr::select(y,ymin,ymax,UNREGION1,colour) %>% 
  distinct %>% 
  ggplot() +
  geom_point(   aes(x=y,y=UNREGION1,color=colour), alpha = 0.5, size = 2) +
  geom_segment( aes(x=ymin, xend = ymax,y=UNREGION1,yend = UNREGION1, color=colour)) +
  scale_color_manual(values = c("#E41A1C" = lessLikelyCol, "#377EB8" = moreLikelyCol)) +
  theme_pubr() +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_y_discrete(
    "Region",
    expand = expansion(mult = c(0.05, -.01))
  ) +
  scale_x_continuous(
    "Random effects",
    limits = c(-0.5, 0.5)
  ) +
  ggtitle("\n") +
  coord_cartesian(clip = "off") +
  geom_text(aes(label = "More likely to reach RG →", y = 22.5, x =  .2), hjust = 0, size = 3, color = moreLikelyCol) +
  geom_text(aes(label = "← Less likely to reach RG", y = 22.5, x = -.2), hjust = 1,size = 3,  color = lessLikelyCol) +
  theme(
    legend.position = "none",
    title = element_text(lineheight = 1, size = 1),
    axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
    panel.grid.major.x = element_line(),
    text = element_text(size = 10)
  )

outlines <- m2_modRes %>%
  dplyr::filter(!is.na(group)) %>% 
  split(.$group) %>%
  lapply(function(x){
    x$geometry %>%
      st_make_valid %>%
      st_union()
  })

m2_modRes %>%  
  split(.$`group`) %>% 
  lapply(st_union) %>% 
  do.call(c, .) %>% # bind the list element to a single sfc
  st_cast() %>% # mapview doesn't like GEOMETRY -> cast to MULTIPOLYGON
  mapview()

outlines <- st_combine(c( outlines[[1]], outlines[[2]], outlines[[3]], outlines[[4]], outlines[[5]], outlines[[6]], outlines[[7]], outlines[[8]], outlines[[9]], outlines[[10]], outlines[[11]], outlines[[12]], outlines[[13]], outlines[[14]], outlines[[15]], outlines[[16]], outlines[[17]], outlines[[18]], outlines[[19]], outlines[[20]], outlines[[21]]))

### Map -----
map2 <- ggplot() +
  geom_sf(data = m2_modRes, mapping = aes(fill = `y`, geometry = geometry)) +
  geom_sf(outlines, mapping = aes(), fill = NA, linewidth = 0.75, color = "grey50") +
  scale_fill_gradientn(
    "Random effect estimate",
    colors = c(lessLikelyCol, likelyMidCol, moreLikelyCol),
    limits = c(-0.5, 0.5)
  ) +
  coord_sf(crs = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs') +
  theme(
    plot.background = element_rect( fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    axis.text = element_blank()
  )

### Combine ----
ggarrange(p_modPreds2, map2) %>% 
  ggsave(filename = file.path(wd$figs, "mod2_map.png"), width = 10, height = 6)


## Model 3 -----
m3 <- readRDS(file = file.path(wd$bin, "m3.rds"))
m3_re <- sjPlot::plot_model(m3, type = "re")
m3_re_dat <- ggplot_build(m3_re)

m3_modRes <- m3_re_dat$data[[3]] %>% 
  cbind(., UNREGION1 = m3_re_dat$layout$panel_params[[1]]$y$get_labels()) %>% 
  left_join(., ccodes) %>% 
  right_join(world,  by = c("ISO3" = "iso_a3") )


### model preds -----
p_modPreds3 <- m3_modRes %>% 
  dplyr::filter(!is.na(UNREGION1)) %>% 
  dplyr::mutate(UNREGION1 = factor(UNREGION1, levels = rev(na.omit(unique(m2_modRes$UNREGION1))))) %>% 
  dplyr::select(y,ymin,ymax,UNREGION1,colour) %>% 
  distinct %>% 
  ggplot() +
  geom_point(   aes(x=y,y=UNREGION1,color=colour), alpha = 0.5, size = 2) +
  geom_segment( aes(x=ymin, xend = ymax,y=UNREGION1,yend = UNREGION1, color=colour)) +
  scale_color_manual(values = c("#E41A1C" = fasterCol, "#377EB8" = slowerCol)) +
  theme_pubr() +
  scale_y_discrete(
    "Region",
   # position = "right",
    expand = expansion(mult = c(0.05, -.01))
  ) +
  scale_x_continuous(
    "Random effects",
    limits = c(-42, 42),
    breaks = seq(-100,100,by=20)
  ) +
  ggtitle("\n") +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_text(aes(label = "More time to RG →", y = 22.5, x = 8.4), hjust = 0, size = 3, color = slowerCol) +
  geom_text(aes(label = "← Less time to RG", y = 22.5, x = -8.4), hjust = 1,size = 3,  color = fasterCol) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    title = element_text(lineheight = 1, size = 1),
    axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
    panel.grid.major.x = element_line(),
    text = element_text(size = 10)
  )


### Map -----
map3 <- ggplot() +
  geom_sf(data = m3_modRes, mapping = aes(fill = `y`, geometry = geometry)) +
  scale_fill_gradient2(
    "Random effect estimate",
    high = slowerCol, mid = speedMidCol, low = fasterCol
    ) +
  coord_sf(crs = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs') +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect( fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  ) 
(map3)

## Plot everything together ----

cowplot::plot_grid(
  p_m2,
  p_m3,
  p_modPreds2, 
  p_modPreds3, 
  map2,
  map3,
  ncol = 2, nrow = 3) %>% 
  ggsave(filename = file.path(wd$figs, "modelResults.png"), width = 10, height = 10)

top <- cowplot::plot_grid(
  p_m2 ,
  p_m3 + scale_x_continuous("Estimates", limits = c(-25, 25)),
  labels = LETTERS[1:2]
)

mid <- cowplot::plot_grid(
  p_modPreds2, 
  p_modPreds3,
  ncol = 2, nrow = 1, align = "h")

bottom <- cowplot::plot_grid(
  map2 + theme(legend.position = "top", legend.title = element_blank()), 
  map3 + theme(legend.position = "top", legend.title = element_blank()),
  ncol = 2, nrow = 1, align = "hv")

cowplot::plot_grid(top,mid,bottom, ncol = 1, rel_heights = c(1,1,2))


bottom2 <- cowplot::plot_grid(
  p_modPreds2, 
  map2 + 
    coord_sf(
      crs = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs',
      xlim = c(-15.2e6, 16.9e6)
    ) +
    theme(
      legend.position = c(0.2,0.1), 
      legend.direction="horizontal",
      legend.title = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect( fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white")
    ) +
    annotate("text", x= -15e6, y = 9e6, label = "D", fontface = "bold", size = 5),
  p_modPreds3,
  map3 + 
    coord_sf(
      crs = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs',
      xlim = c(-15.2e6, 16.9e6)
    ) +
    theme(
      legend.position = c(0.2,0.1), 
      legend.direction="horizontal",
      axis.title = element_blank(),
      legend.title = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect( fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white")
    )  +
    annotate("text", x= -15e6, y = 9e6, label = "F", fontface = "bold", size = 5),
  rel_widths = c(1,1.5), ncol = 2, nrow = 2,
  labels = c("C", NA, "E", NA)
) 
ggsave(bottom2, filename = file.path(wd$figs, "hitsRGmodel.png"), width = 10, height=8)

cowplot::plot_grid(
    top, 
    bottom2, 
    ncol = 1, rel_heights = c(1,2)
  ) %>% 
  ggsave(.,  filename = file.path(wd$figs, "bigModel.png"), width = 10, height=12, bg = "white")


# Plot m4 results-----
## m4 results ----------------------------------------------------------------------

if(!file.exists(file.path(wd$bin, "m4_preds.rds")) | isTRUE(rerun)) {
  if(!exists("m4")) m4 <- readRDS(file.path(wd$bin, "m4.rds"))
  m4_timeActive_s_attributes <- readRDS(file.path(wd$bin, "m4_timeActive_s_attributes.rds"))
  p <- sjPlot::plot_model(m4, type = "pred", terms = "timeActive_sc [all]") %>% 
    ggplot_build()
  m4_preds <- p %>% 
    {.$data[[1]]} %>% 
    dplyr::mutate(x_rescaled = round(x * m4_timeActive_s_attributes$`scaled:scale` + m4_timeActive_s_attributes$`scaled:center`)) %>% 
    dplyr::filter(x_rescaled %in% targetDays) %>% 
    group_by(x_rescaled) %>% 
    dplyr::summarise(x = max(x_rescaled), y = mean(y)) %>% 
    dplyr::select(x,y)
  saveRDS(m4_preds, file = file.path(wd$bin, "m4_preds.rds"))
  rm(m4)
} else if(!exists("m4_preds")) {
  m4_preds <- readRDS(file.path(wd$bin, "m4_preds.rds"))
}


if(!exists("m4")) m4 <- readRDS(file.path(wd$bin, "m4.rds"))
#ggplot_build(plot_model(m4))$layout$panel_params[[1]]$y.sec$breaks
p_m4 <- plot_model(m4) + 
  theme_pubr() +
  geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("Model 4 - leaves RG y/n") +
  theme(
    panel.grid.major.x = element_line()
  ) +
  scale_y_continuous() +
  scale_x_discrete(
    breaks = c(
      "timeActive_sc", 
      "timeToRG_sc",
      # "as.factor(computerVision)1", 
      "obs1rankLevel_sc", 
      "nObs_biome_sc" 
      #"IDexperience_allIDs_sc", 
      #"IDexperience_allIDs_sc:IDexperience_preRG_sc"
    ),
    labels = c(
      "Time observation\nactive in study",
      "Time to RG",
      "Computer visition used",
      "Taxon rank of\nfirst identification",
      "Number of obs in biome",
      "Total experience\nof all identifiers",
      "Experience\nof all identifiers\ntotal * pre_RG"
    )
  ) +
  theme(
    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)
  )


# Model predictions and sanke plot ----------------------------------------

## After 1 year... ----
yrs <- 1

# Percent inactive / casual:
100-m1_preds[m1_preds$x==365*yrs,"y"]*100
# Percent active:
m1_preds[m1_preds$x==365*yrs,"y"]*100

## Of active observations ----
# Percent hit RG:
m2_preds[m2_preds$x==365*yrs,"y"]*100
# Percent did not hit RG:
100-m2_preds[m2_preds$x==365*yrs,"y"]*100

## Of those that hit RG-----
# Remained RG:
100-m4_preds[m4_preds$x==365*yrs,"y"]*100
# Left RG:
m4_preds[m4_preds$x==365*yrs,"y"]*100

## Of those that left RG ----
whoLeft <- dbGetQuery(iNat, "SELECT * FROM leavesRG;")
whoLeft %>% 
  count(leavesRG, leavesRG_notes) %>% 
  ggplot() +
  geom_col(aes(y = n, x = leavesRG, fill = leavesRG_notes))

whoLeft %>% 
  dplyr::filter(leavesRG == 1) %>% 
  count(leavesRG_notes) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  dplyr::select(perc, leavesRG_notes)

## Of those that remained RG ----
whoLeft %>% 
  dplyr::filter(leavesRG == 0) %>% 
  count(leavesRG_notes) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  dplyr::select(perc, leavesRG_notes)

whoLeft_count <- whoLeft %>% 
  dplyr::filter(leavesRG == 0) %>% count(leavesRG_notes) %>% 
  mutate(frac = n/sum(n))

whoLeft_count2 <- whoLeft %>% 
  dplyr::filter(leavesRG == 1) %>% count(leavesRG_notes) %>% 
  mutate(frac = n/sum(n))


## visualize sanke plot in R ---------------------------------------------------
# Load package
library(networkD3)

qg<-list()
qg$nodes <- data.frame(name=c(
  "Start",          
  "Active",          
  "Inactive",         
  "Hits RG",        
  "Needs ID",       
  "Remain RG",      
  "Leaves RG",      
  "Finer tax level",
  "Same ID",        
  "Taxon swap",
  "Research Grade"
))
qg$links <- bind_rows(
  
  # Start -> inactive
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Start") - 1, 
    target = which(qg$nodes$name=="Inactive") - 1, 
    value = 100-unlist(m1_preds[m1_preds$x==365*yrs,"y"])*100
  )),
  # Start -> active
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Start") -1, 
    target = which(qg$nodes$name=="Active")-1, 
    value = m1_preds[m1_preds$x==365*yrs,"y"]*100
  )),
  # Active -> RG
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Active")-1, 
    target = which(qg$nodes$name=="Hits RG")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*100}
  )),
  # Active -> Needs ID
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Active")-1, 
    target = which(qg$nodes$name=="Needs ID")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(1-m2_preds[m2_preds$x==365*yrs,"y"])*100}
  )),
  # RG -> Finer tax level
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Hits RG")-1, 
    target = which(qg$nodes$name=="Finer tax level")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, taxon has changed from an active RG-taxon but to a finer taxonomic level","frac"])*100}
  )),
  # Finer tax level -> Remains RG
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Finer tax level")-1, 
    target = which(qg$nodes$name=="Remain RG")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, taxon has changed from an active RG-taxon but to a finer taxonomic level","frac"])*100}
  )),
  # RG -> Same ID
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Hits RG")-1, 
    target = which(qg$nodes$name=="Same ID")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, same obs_taxonID persisted","frac"])*100}
  )),
  #Same ID -> Remains RG
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Same ID")-1, 
    target = which(qg$nodes$name=="Remain RG")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, same obs_taxonID persisted","frac"])*100}
  )),
  # RG -> Taxon swap
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Hits RG")-1, 
    target = which(qg$nodes$name=="Taxon swap")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, likely taxon swap (RG-tax is not active)","frac"])*100}
  )),
  #Taxon swap-> Remains RG
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Taxon swap")-1, 
    target = which(qg$nodes$name=="Remain RG")-1,
    value = {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, likely taxon swap (RG-tax is not active)","frac"])*100}
  )),
  
  # RG -> Leaves RG
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Hits RG")-1, 
    target = which(qg$nodes$name=="Leaves RG")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(m4_preds[m4_preds$x==365*yrs,"y"])*100}
  )),
  # Leaves RG -> Needs ID
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Leaves RG")-1, 
    target = which(qg$nodes$name=="Needs ID")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(m4_preds[m4_preds$x==365*yrs,"y"])*sum(whoLeft_count2[whoLeft_count2$leavesRG_notes != "is RG, taxon has changed from an active RG-taxon and is not at a finer taxonomic level", "frac"])*100}
  )),
  # Leaves RG -> Research Grade
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Leaves RG")-1, 
    target = which(qg$nodes$name=="Research Grade")-1,
    value =  {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(m4_preds[m4_preds$x==365*yrs,"y"])*sum(whoLeft_count2[whoLeft_count2$leavesRG_notes == "is RG, taxon has changed from an active RG-taxon and is not at a finer taxonomic level", "frac"])*100}
  )),
  # Remains RG -> Research Grade
  as.data.frame(cbind(
    source = which(qg$nodes$name=="Remain RG")-1, 
    target = which(qg$nodes$name=="Research Grade")-1,
    value = sum(
      # Same ID
      {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, same obs_taxonID persisted","frac"])*100},
      # Taxon swap
      {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, likely taxon swap (RG-tax is not active)","frac"])*100},
      # Finer tax level
      {unlist(m1_preds[m1_preds$x==365*yrs,"y"])*unlist(m2_preds[m2_preds$x==365*yrs,"y"])*unlist(1-m4_preds[m4_preds$x==365*yrs,"y"])*unlist(whoLeft_count[whoLeft_count$leavesRG_notes == "is RG, taxon has changed from an active RG-taxon but to a finer taxonomic level","frac"])*100}
    )
  )),
)

(sn <- sankeyNetwork(Links = qg$links, Nodes = qg$nodes, Source = "source",
                     Target = "target", Value = "value", NodeID = "name",
                     units = "percent", fontSize = 30, nodeWidth = 30,
                     margin = list(0,0,0,0)
))


# you save it as an html
saveNetwork(sn, file.path(wd$bin, "sn.html"))
# library(htmlwidgets)
# saveWidget(sn, file.path(wd$bin, "sn2.html"))

library(webshot)
# you convert it as png
webshot(file.path(wd$bin, "sn.html"), file.path(wd$figs,"sn.png"), vwidth = 2000, vheight = 2000)
#webshot(file.path(wd$bin, "sn2.html"), "sn2.png", vwidth = 1000, vheight = 900)

# Make a quick data.table summarizing these data:
mynodes <- data.frame(qg$nodes) %>% dplyr::mutate(val = row_number()-1) 
(mytable <- left_join(qg$links, mynodes, by = c("source" = "val")) %>% 
    dplyr::rename(from = name) %>% 
    left_join(., mynodes, by = c("target" = "val")) %>% 
    dplyr::rename(to = name) %>% 
    # Scale to a 30" high plot. Value should be height of node in inches.
    dplyr::mutate(
      scaledval = value/100*30, percent = round(value, 1)
    )
)
mytable %>% 
  dplyr::filter(target %in% c(10,4)) %>% 
  group_by(to) %>% 
  dplyr::summarise(
    endscaled = sum(scaledval),
    endVal = round(sum(value),1)
  )


