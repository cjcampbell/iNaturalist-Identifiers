
# Setup -------------------------------------------------------------------

library(DBI)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(geosphere)
library(ggpubr)

idsByUser <- fread(file.path(wd$out, "idsByUser.csv"))
ider_area <- file.path(wd$out, "ider_area.csv") %>% fread
df <- inner_join(idsByUser, ider_area, by = c("user.id", "n_IDs", "n_obs_IDed", "n_taxa_IDed", "idsPerobs_IDed", "n_obs", "n_taxa_observed")) 
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")

## Load user rankings ------

if(!exists("idsByUser")) idsByUser <- fread(file.path(wd$out, "idsByUser.csv"))
if(!exists("ider_area")) ider_area <- fread(file.path(wd$out, "ider_area.csv"))
df_byUser <- idsByUser %>% 
  dplyr::filter(n_obs_IDed >= 100) %>% 
  left_join(ider_area) %>% 
  dplyr::select(user.id, mean_dist, sd_dist, n_IDs, n_taxa_IDed) %>% 
  dplyr::mutate(taxaPerID = n_taxa_IDed/n_IDs)
f_meandist <- ecdf(df_byUser$mean_dist)
f_sd_dist <- ecdf(df_byUser$sd_dist)
f_n_IDs <- ecdf(df_byUser$n_IDs)
f_n_taxa_IDed <- ecdf(df_byUser$n_taxa_IDed)
f_taxaPerID <- ecdf(df_byUser$taxaPerID)
userDeets <- df_byUser %>% 
  dplyr::mutate(
    q_meandist      = f_meandist(mean_dist),
    q_sd_dist       = f_sd_dist(sd_dist),
    q_n_IDs         = f_n_IDs(n_IDs),
    q_n_taxa_IDed   = f_n_taxa_IDed(n_taxa_IDed),
    q_taxaPerID     = f_taxaPerID(taxaPerID)
  )

## Functions ---------------------------------------------------------------

getIDgeoDetails <- function(id) {
  
  dbGetQuery(
    iNat, 
    paste0(
      "SELECT * FROM
        (SELECT * FROM  (SELECT * FROM ider_area WHERE `user.id` = ", id, ") AS area
        INNER JOIN (SELECT * FROM idsByUser) AS idsByUser ON area.`user.id` = idsByUser.`user.id`
        INNER JOIN (SELECT * FROM ids WHERE `id_user.id` = ", id, " ORDER BY RANDOM() LIMIT 5000) AS ids ON area.`user.id` = ids.`id_user.id`
        LEFT JOIN (SELECT obs_ID, latitude, longitude FROM obs) AS obs ON obs.`obs_ID` = ids.`obs_ID`)"
    ))
  
}


makePlot <- function(id, mycolor = NULL) {
  
  df2 <- getIDgeoDetails(id)
  
  pts_from <-  cbind(df2$cent_lon[1], df2$cent_lat[1]) %>% 
    sf_project(
      to = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
      from = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs'
    )
  
  pts_to <- cbind(df2$longitude, df2$latitude) %>% 
    na.omit()
  
  inter <- gcIntermediate(pts_from,  pts_to, n=50, addStartEnd=TRUE, breakAtDateLine=T, sp = T) %>% 
    st_as_sf()
  
  if(is.null(mycolor)) {
    mycolor <- sample(viridisLite::turbo(15, begin = 0.1, end = 0.9), size = 1)
  }
  
  p <- ggplot() +
    geom_sf(wrld, mapping = aes(), color = "black", fill = "#292E3D", linewidth = 0.1) +
    geom_sf(inter, mapping = aes(), color = mycolor, linewidth = 0.1, alpha = 0.5) +
    theme_dark() +
    theme(
      panel.background = element_rect("black", color = NA),
      plot.background = element_rect("black", color = NA),
      panel.grid = element_line(color = "grey20")
    ) +
    coord_sf(crs = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs')
  
  return(p)
  
}


make_quantPlot <- function(id, mycolor = NULL) {
  if(is.null(mycolor)) {
    mycolor <- sample(viridisLite::turbo(15, begin = 0.1, end = 0.9), size = 1)
  }
  
  dd1 <- userDeets %>% 
    dplyr::filter(user.id == id) 
  dd1 %>% 
    dplyr::select(starts_with("q_")) %>% 
    dplyr::select(-q_n_taxa_IDed) %>% 
    pivot_longer(cols = everything(), names_to = "param") %>% 
    mutate(order = case_when(
      param == "q_n_IDs"       ~ 1,
      param == "q_meandist"   ~ 2,
      param == "q_sd_dist"     ~ 3,
      param == "q_taxaPerID" ~ 4
    )) %>% 
    ggplot() +
    geom_vline(xintercept = 0.5, color = "#292E3D") +
    geom_vline(xintercept = 0,   color = "#292E3D") +
    geom_vline(xintercept = 01,  color = "#292E3D") +
    aes(y = order, x = value) +
    geom_segment(aes(yend = order, xend = 0.5), color = mycolor, alpha = 0.8) +
    geom_text(aes(label = round(value*100)), color = "grey80", vjust = -0.5, hjust = 0.5, size = 3) +
    geom_point(color = mycolor) +
    scale_x_continuous(
      name = "Percentile",
      limits = c(0,1),
      breaks = seq(0,1,by=0.25),
      labels = seq(0,100,by=25)
      #expand = c(0,0)
    ) +
    scale_y_reverse(
      name = NULL,
      expand = expansion(mult = c(0, .1)),
      breaks = 1:4,
      labels = c("Number of identifications", "Distance to identifications", "Geographic generalization","Taxonomic generalization")
    ) +
    theme(
      plot.margin = margin(0,0,0,0, unit = "pt"),
      axis.line.x = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.text = element_text(color = "white", size = 12),
      axis.title = element_text(color = "white")
    ) +
    coord_cartesian(clip = "off")
}

makeTogetherPlot <- function(id, mycolor = NULL, returnPlot = FALSE, savePlot = FALSE) {
  if(is.null(mycolor)) {
    mycolor <- sample(viridisLite::turbo(15, begin = 0.1, end = 0.9), size = 1)
  }
  
  mycolor <- "#C8EF34FF"
  p_map <- makePlot(id, mycolor = mycolor)
  p_qPlot <- make_quantPlot(id, mycolor = mycolor)
  p <- ggdraw() +
    draw_plot(p_map, height = 0.75) +
    draw_plot(p_qPlot, x = 0.5, y = 0.95, width = 0.5, height = 0.25, vjust = 1, hjust = 0.5) +
    draw_text(text = paste("User", id), y = 0, x = 1, vjust = -1, hjust = 1, size = 10, color = "grey80")
  if(savePlot) ggsave(plot = p, filename = file.path(wd$figs, paste0("map_rank_", id, ".png")), bg= "#08090C", width = 16, height =12)
  if(returnPlot) return(p)
}


# Arrange colors and codes -------
library(pals) #https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html

mycols <- rev(arc.bluepink(n=16))
names(mycols) <- paste(1:4, rep(1:4,each = 4), sep = "_")
mylabs <- c("tax highly specialized - geo highly specialized",
            "tax highly specialized - geo specialized",
            "tax highly specialized - geo generalized",
            "tax highly specialized - geo highly generalized",
            "tax specialized - geo highly specialized",
            "tax specialized - geo specialized",
            "tax specialized - geo generalized",
            "tax specialized - geo highly generalized",
            "tax generalized - geo highly specialized",
            "tax generalized - geo specialized",
            "tax generalized - geo generalized",
            "tax generalized - geo highly generalized",
            "tax highly generalized - geo highly specialized",
            "tax highly generalized - geo specialized",
            "tax highly generalized - geo generalized",
            "tax highly generalized - geo highly generalized"      
)


# rescale data ---------------------------------------------------

df2 <- df %>% 
  dplyr::filter(!is.na(mean_dist)) %>% 
  dplyr::mutate(taxPerID = n_taxa_IDed / n_IDs, ids_10throot = n_IDs^.1) %>% 
  mutate(across(.cols = c("mean_dist","sd_dist", "n_IDs", "n_taxa_IDed", "taxPerID", "ids_10throot"), .fns = scale, .names = "{.col}_scaled")) %>% 
  dplyr::mutate(
    bin_tax_specialist = cut(taxPerID_scaled, breaks = c(-Inf,-1,0,1,Inf), ordered_result = T, labels = F),
    bin_geo_specialist = cut(sd_dist_scaled,  breaks = c(-1,-.5,0,1,Inf), ordered_result = T, labels = F),
    code = as.character(paste(bin_tax_specialist, bin_geo_specialist, sep = "_"))
  ) %>% 
  left_join(., data.frame(code = names(mycols), lab = mylabs))


df3 <- df2 %>% 
  group_by(code, lab) %>% 
  dplyr::summarise(n=n(), n_IDs = sum(n_IDs)) 

## By users ----

mywidths_users <- 
  df2 %>% 
  dplyr::group_by(bin_tax_specialist) %>% 
  dplyr::summarise(n=n()) %>% 
  dplyr::mutate(
    width = n/sum(n),
    xmin = cumsum(width) - width,
    xmax = cumsum(width)
  ) %>% 
  dplyr::select(-n)

myheights_users <- df2 %>% 
  group_by(bin_tax_specialist, bin_geo_specialist) %>% 
  dplyr::summarise(n=n()) %>% 
  dplyr::mutate(
    height = n/sum(n),
    y = cumsum(height)-height,
    yend = cumsum(height)) %>% 
  dplyr::select(-n, -height)

# How many users are geographic super-specialists?
df2 %>% 
  count(bin_geo_specialist) %>% 
  dplyr::mutate(
    prop = n/sum(n)) %>% 
  dplyr::filter(bin_geo_specialist %in% 1:2) %>% 
  ungroup %>% 
  dplyr::summarise(prop = sum(prop))

df4 <- df2 %>% 
  dplyr::select(bin_tax_specialist, bin_geo_specialist, code, lab) %>% 
  distinct() %>% 
  left_join(mywidths_users) %>% 
  left_join(myheights_users)

margin <- 0.01
p_numberUsers <- 
  df4 %>% 
  ggplot() +
  geom_rect(aes(fill = code, 
                xmin = xmin, xmax = xmax,
                ymin = y, ymax = yend)) +
  scale_fill_manual( values = mycols, breaks = names(mycols)) +
  coord_fixed(clip = "off", ylim = c(-margin*5, 1), xlim = c(-margin*5, 1)) +
  theme_void() +
  theme(legend.position = "none") +
  geom_segment(color = "grey90", x = 0, xend = 0.5, y = -margin*2, yend = -margin*2) +
  geom_segment(color = "grey90", x = 0, xend = 0, y = -margin*2, yend = -margin) +
  geom_segment(color = "grey90", x = 0.5, xend = 0.5, y = -margin*2, yend = -margin) +
  geom_text(x = 0.25, y = margin, hjust = 0.5, vjust = 1.75, 
            label = "50% of identifiers are\ntaxonomic specialists",
            color = "grey90", lineheight = 0.75) +
  geom_segment(color = "grey90", x = -margin*2, xend = -margin*2, y = 0, yend = 0.65) +
  geom_segment(color = "grey90", x = -margin*2, xend = -margin, y = 0, yend = 0) +
  geom_segment(color = "grey90", x = -margin*2, xend = -margin, y = 0.65, yend = 0.65) +
  geom_text(x = -margin, y = 0.65/2, hjust = 0.5, vjust = -0.5, 
            angle = 90, color = "grey90", lineheight = 0.75,
            label = "65% of identifiers are\ngeographic specialists") 


## By number of IDs ----

mywidths <- 
  df2 %>% 
  dplyr::group_by(bin_tax_specialist) %>% 
  dplyr::summarise(n=sum(n_IDs)) %>% 
  dplyr::mutate(
    width = n/sum(n),
    xmin = cumsum(width) - width,
    xmax = cumsum(width)
  ) %>% 
  dplyr::select(-n)

myheights <- df2 %>% 
  group_by(bin_tax_specialist, bin_geo_specialist) %>% 
  dplyr::summarise(n=sum(n_IDs)) %>% 
  dplyr::mutate(
    height = n/sum(n),
    y = cumsum(height)-height,
    yend = cumsum(height)) %>% 
  dplyr::select(-n, -height)

df4 <- df2 %>% 
  dplyr::select(bin_tax_specialist, bin_geo_specialist, code, lab) %>% 
  distinct() %>% 
  left_join(mywidths) %>% 
  left_join(myheights)

p_numberIDs <- df4 %>% 
  ggplot() +
  geom_rect(aes(fill = code, 
                xmin = xmin, xmax = xmax,
                ymin = y, ymax = yend)) +
  scale_fill_manual( values = mycols, breaks = names(mycols)) +
  coord_fixed(clip = "off", ylim = c(-margin*5, 1), xlim = c(-margin*5, 1)) +
  theme_void() +
  theme(legend.position = "none") +
  geom_segment(color = "grey90", x = 0, xend = 0.84, y = -margin*2, yend = -margin*2) +
  geom_segment(color = "grey90", x = 0, xend = 0, y = -margin*2, yend = -margin) +
  geom_segment(color = "grey90", x = 0.84, xend = 0.84, y = -margin*2, yend = -margin) +
  geom_text(x = 0.42, y = margin, hjust = 0.5, vjust = 1.75, 
            label = "84% of identifications are\n produced by taxonomic specialists",
            color = "grey90", lineheight = 0.75)



## Combine ----

mylegend <- data.frame(
  fill = mycols,
  x = rep(1:4,4),
  y = rep(1:4, each = 4)
) %>% 
  ggplot() +
  geom_tile(aes(fill=fill,x=x,y=y)) +
  scale_fill_identity() +
  labs(x = "Taxonomically\n   generalist →",
       y = "Geographically\n   generalist →️") +
  theme_map() +
  theme(
    axis.title.y = element_text(angle = 90),
    axis.title = element_text(size = 10, hjust = 0.5)
  ) +
  # quadratic tiles
  coord_fixed()

## Plot together with plotem -----
makedark <- list(
  theme(
    text = element_text(color = "white", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect("black", color = NA),
    plot.background = element_rect("black", color = NA),
    panel.grid = element_line(color = "grey20")
  ) 
)

p_areas <- ggarrange(
  mylegend + makedark,
  p_numberUsers + makedark + ggtitle("Number of users on iNaturalist") ,
  p_numberIDs + makedark + ggtitle("Number of identifications from users"),
  nrow = 1,
  widths = c(1,2,2)
) 

# ggsave(p_areas, filename = file.path(wd$figs, "usersVIDs-area_dark.png"), bg = "black", width = 12, height = 5)


# Select some example users...
library(sf)
library(geosphere)
wrld <- rnaturalearth::countries110 %>% 
  st_as_sf()

makeDoublePlot <- function(plot1, map1, textString) {
  p <- ggdraw() +
    draw_plot(map1, height = 0.75, width = 1, y = 0.25) +
    draw_plot(plot1, x = 0.5, y = 0.05, width = 0.75, height = 0.25, vjust = 0, hjust = 0.5) +
    draw_text(text = paste(textString), y = 0.37, x = 1, vjust = 1, hjust = 1, size = 12, color = "grey80")
  return(p)
}




# Someone from group 1-1
set.seed(42)
myID_1_1 <- df2 %>% 
  dplyr::filter(code == "1_1", n_IDs > 5000) %>% 
  slice_sample(n=1) %>% 
  dplyr::select(user.id) %>% 
  unlist
(myID_1_1) #https://www.inaturalist.org/users/2442814
mycolor = mycols[names(mycols) == "1_1"]
p_map1_1 <- makePlot(id = myID_1_1, mycolor = mycolor)
p_qPlot1_1 <- make_quantPlot(id = myID_1_1, mycolor = mycolor)
p.1_1 <- makeDoublePlot(p_qPlot1_1, p_map1_1, "@cesarmassi - Cesar Massi  ")
# p.1_1 <- ggdraw() +
#   draw_plot(p_map1_1, height = 0.7) +
#   draw_plot(p_qPlot1_1, x = 0.5, y = 0.95, width = 0.75, height = 0.3, vjust = 1, hjust = 0.5) +
#   draw_text(text = paste("@cesarmassi - Cesar Massi  "), y = 0, x = 1, vjust = -1, hjust = 1, size = 12, color = "grey80")
ggsave(p.1_1, filename = file.path(wd$figs, "user_cesarmassi_mapAndStats.png"), width = 12, height = 12, bg = "black")
ggsave(p_map1_1, filename = file.path(wd$figs, "user_cesarmassi_map.png"), width = 12, height = 12, bg = "black")
ggsave(p_map1_1 + coord_sf(xlim = c(-100, -10), ylim = c(-60, 10)), filename = file.path(wd$figs, "user_cesarmassi_map_zoomed.png"), width = 12, height = 12, bg = "black")

# 1_4
set.seed(42)
#id <- 5643 # Jakob
myID_1_4 <- 429066 # featherenthusiast https://www.inaturalist.org/people/featherenthusiast
mycolor = mycols[names(mycols) == "1_4"]
p_map1_4 <- makePlot(myID_1_4, mycolor = mycolor)
p_qPlot1_4 <- make_quantPlot(myID_1_4, mycolor = mycolor)
p.1_4 <- makeDoublePlot(p_qPlot1_4, p_map1_4, "@featherenthusiast - Amanda Janusz  ")
# p.1_4 <- ggdraw() +
#   draw_plot(p_map1_4, height = 0.7) +
#   draw_plot(p_qPlot1_4, x = 0.5, y = 0.95, width = 0.75, height = 0.3, vjust = 1, hjust = 0.5) +
#   draw_text(text = paste("@featherenthusiast - Amanda Janusz  "), y = 0, x = 1, vjust = -1, hjust = 1, size = 12, color = "grey80")
ggsave(p.1_4, filename = file.path(wd$figs, "user_featherenthusiast_mapAndStats.png"), width = 12, height = 12, bg = "black")
ggsave(p_map1_4, filename = file.path(wd$figs, "user_featherenthusiast_map.png"), width = 12, height = 12, bg = "black")


# # 3-1 or 4-1
# set.seed(37)
# myID_3_1 <- df2 %>% 
#   dplyr::filter(code %in% c("4_1", "3_1")) %>% 
#   arrange(desc(n_IDs)) %>% 
#   slice(4) %>% 
#   dplyr::select(user.id) %>% 
#   unlist
# (myID_3_1)
# # https://www.inaturalist.org/users/896523
# mycolor = mycols[names(mycols) == "3_1"]
# p_map3_1 <- makePlot(id = myID_3_1, mycolor = mycolor)
# p_qPlot3_1 <- make_quantPlot(id = myID_3_1, mycolor = mycolor)
# p.3_1 <- ggdraw() +
#   draw_plot(p_map3_1, height = 0.7) +
#   draw_plot(p_qPlot3_1, x = 0.5, y = 0.95, width = 0.75, height = 0.3, vjust = 1, hjust = 0.5) +
#   draw_text(text = paste("@tutukiwi - Andrew Townsend  "), y = 0, x = 1, vjust = -1, hjust = 1, size = 12, color = "grey80")
# ggsave(p.3_1, filename = file.path(wd$figs, "user_tutukiwi_mapAndStats.png"), width = 12, height = 12, bg = "black")
# ggsave(p_map3_1, filename = file.path(wd$figs, "user_tutukiwi_map.png"), width = 12, height = 12, bg = "black")


# And an alternate
set.seed(37)
myID_3_1_b <- df2 %>% 
  dplyr::filter(code %in% c("4_1", "3_1")) %>% 
  arrange(desc(n_IDs)) %>% 
  slice(1) %>% 
  dplyr::select(user.id) %>% 
  unlist
(myID_3_1_b)
mycolor = mycols[names(mycols) == "3_1"]
p_map3_1b <- makePlot(id = myID_3_1_b, mycolor = mycolor)
p_qPlot3_1b <- make_quantPlot(id = myID_3_1_b, mycolor = mycolor)
p.3_1b <- makeDoublePlot(p_qPlot3_1b, p_map3_1b, "@m_d - Mahomed Desai   ")
# p.3_1b <- ggdraw() +
#   draw_plot(p_map3_1b, height = 0.7) +
#   draw_plot(p_qPlot3_1b, x = 0.5, y = 0.95, width = 0.75, height = 0.3, vjust = 1, hjust = 0.5) +
#   draw_text(text = paste("@m_d - Mahomed Desai   "), y = 0, x = 1, vjust = -1, hjust = 1, size = 12, color = "grey80")
ggsave(p.3_1b, filename = file.path(wd$figs, "user_m_d_mapAndStats.png"), width = 12, height = 12, bg = "black")
ggsave(p_map3_1b, filename = file.path(wd$figs, "user_m_d_map.png"), width = 12, height = 12, bg = "black")



# 3-3
myID_3_3 <- df2 %>% 
  dplyr::filter(code == "3_3") %>% 
  arrange(desc(n_IDs)) %>% 
  slice(11) %>% 
  dplyr::select(user.id) %>% 
  unlist
(myID_3_3)
# https://www.inaturalist.org/users/18780
mycolor = mycols[names(mycols) == "3_3"]
p_map3_3 <- makePlot(id = myID_3_3, mycolor = mycolor)
p_qPlot3_3 <- make_quantPlot(id = myID_3_3, mycolor = mycolor)
p.3_3 <- makeDoublePlot(p_qPlot3_3, p_map3_3, "@hydaticus - Robby Deans  ")
# p.3_3 <- ggdraw() +
#   draw_plot(p_map3_3, height = 0.75, width = 1, y = 0.25) +
#   draw_plot(p_qPlot3_3, x = 0.5, y = 0.05, width = 0.75, height = 0.25, vjust = 0, hjust = 0.5) +
#   draw_text(text = paste("@hydaticus - Robby Deans  "), y = 0.37, x = 1, vjust = 1, hjust = 1, size = 12, color = "grey80")
ggsave(p.3_3, filename = file.path(wd$figs, "user_hydaticus_mapAndStats.png"), width = 12, height = 12, bg = "black")
ggsave(p_map3_3, filename = file.path(wd$figs, "user_hydaticus_map.png"), width = 12, height = 12, bg = "black")


p_maps <- ggarrange(
  p.1_4,
  p.3_3,
  p.1_1,
  p.3_1b,
  ncol = 2, nrow = 2
)

library(gridExtra)
p_big <- ggarrange(
  grid.arrange(
    cowplot::plot_grid(
      {p_numberUsers + makedark + ggtitle("Number of users on iNaturalist") + theme(plot.margin = margin(20,0,0,50, unit = "pt"))},
      {p_numberIDs + makedark + ggtitle("Number of identifications from users") + theme(plot.margin = margin(20,0,0,20, unit = "pt"))},
      align = "hv"
    ),
    ggplot() + makedark,
    mylegend + makedark + labs(title = "Legend") + theme(title = element_text(size = 10, face = "plain", family = "sans")),
    layout_matrix = matrix(c(1,1,1,1,3,4), nrow = 2,ncol = 3),
    widths = c(2,2,1)
  ),
  p_maps,
  ncol = 1, heights = c(1,2)
)

p_big2 <- cowplot::ggdraw() +
  draw_grob(as_grob(p_big)) +
  draw_text(text = "A", x = 0.02, y = 0.98, color = "grey80", size = 18, family = "Roboto", fontface = "bold") +
  draw_text(text = "B", x = 0.42, y = 0.98, color = "grey80", size = 18, family = "Roboto", fontface = "bold") +
  draw_text(text = "C", x = 0.02, y = 0.63, color = "grey80", size = 18, family = "Roboto", fontface = "bold") +
  draw_text(text = "D", x = 0.51, y = 0.63, color = "grey80", size = 18, family = "Roboto", fontface = "bold") +
  draw_text(text = "E", x = 0.02, y = 0.30, color = "grey80", size = 18, family = "Roboto", fontface = "bold") +
  draw_text(text = "F", x = 0.51, y = 0.30, color = "grey80", size = 18, family = "Roboto", fontface = "bold")
ggsave(p_big2, filename = file.path(wd$figs, "usersVIDs-area_dark-Maps.png"), bg = "black", width = 12, height = 14)
