# Setup ------------------------------------------------------------------------
library(lme4)
library(optimx)
library(DBI)
library(sjPlot)
library(glmmLasso)
library(geodata)
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "/srv/gspeedq32/mbelitz/iNatIDers/iNat.db")
ccodes <- geodata::country_codes() %>% dplyr::select(ISO3, UNREGION1)

## Function to rescale and add region...
makeNiceColumns <- function(df) {
  df %>% 
  # Add UNREGION1
  left_join(., ccodes, by = c("geo_ISO3" = "ISO3")) %>% 
  mutate(UNREGION1 = factor(UNREGION1)) %>% 
  # scaled numeric cols to have suffix _sc
  # scaled matrix cols to have suffix _s (useful to retain later for model predictions...)
  mutate(across(c(starts_with('n_obs'),  starts_with("n_ids"), starts_with("IDexperience"), starts_with("nID"), starts_with("nObs"), "timeActive", "obs1rankLevel","timeToRG"), scale, .names = '{col}_s')) %>% 
  mutate(across(where(is.matrix), ~c(.x), .names = '{col}c')) %>% 
  mutate(across(c("computerVision", starts_with("geo_"), starts_with("tax")), as.factor))
}



# Model 1 ----------------------------------------------------------------------
# iNat db table: activeTime
# 
# Model structure:
# active ~ timeActive
#
# Glossary:
# active     -- 0/1 of whether observation ID is associated with active observation (as of study end)
# timeActive -- interval in days between nearest observation posted and end of study. For inactive observations, time was calculated from the *next* observation post date.

mdf <- dbGetQuery(iNat, "SELECT * FROM activeTime")
m1 <- glm(active ~ timeActive, data = mdf, family = binomial(link = "logit"))
summary(m1)

plot_model(m1, type = "pred", terms = "timeActive")

saveRDS(m1, file = file.path(wd$bin, "m1.rds"))

# predict 1-5
#p <- predict(m1, newdata = data.frame(timeActive = 730))
#p



# Model 2 ----------------------------------------------------------------------
## Model target params ----
# Does something reach research grade ##
# iNat db table: obs_info2
# Response = hitsRG
# 
# taxClass
# biome
# computerVisionInitialObs -- was computer vision used for first observation
# activeTime
# obs1Rank -- rank level of first Obs
# cumulative / max / average ID experience in IDers
# numberOfIdentitications at the genus / family
# numberOfObservations at the genus / family
# numberOfSamplesInArea
# numberOfSamplesInArea:numberOfObservationsGenus


## look at correlation among variables ----
# df2 <- dbGetQuery(iNat, "SELECT * FROM obs_info2 WHERE obs_ID IN (SELECT obs_ID FROM obs_info2 ORDER BY RANDOM() LIMIT 10000);") %>% 
#   makeNiceColumns()
# library(corrplot)
# cr <- df2 %>% 
#   select(
#     timeActive_sc, IDexperience_allIDs_sc, IDexperience_preRG_sc,obs1rankLevel_sc, n_obs_gen_sc, n_ids_gen_sc,
#      n_obs_class_sc, n_ids_class_sc, 
#      n_obs_fam_sc, n_ids_fam_sc,
#      nObs_biome) %>% 
#   cor()
# 
# corrplot(cr)
# 
# # removing IDexperience_preRG, n_ids_gen_sc, n_obs_class_sc, n_ids_class_sc,
# #          n_ids_fam_sc
# 
# cr <- df4 %>% select(timeActive_sc, IDexperience_preRG_sc,
#                      obs1rankLevel_sc, n_obs_gen_sc, 
#                      n_obs_fam_sc,
#                      nObs_biome) %>% 
#   cor()
# 
# corrplot(cr)

## model 2 Does something hit RG? -----
paramEsts2 <- list()
for(i in 1:10){
  writeLines(paste(i))
  df2 <- dbGetQuery(iNat, "SELECT * FROM obs_info2 WHERE obs_ID IN (SELECT obs_ID FROM obs_info2 ORDER BY RANDOM() LIMIT 10000);") %>% 
    makeNiceColumns()
  
  mdf2 <- df2 %>% 
    filter(!is.na(hitsRG),
           !is.na(timeActive_sc),
           !is.na(computerVision),
           !is.na(IDexperience_allIDs_sc),
           !is.na(obs1rankLevel_sc),
           !is.na(nObs_biome_sc),
           !is.na(UNREGION1)
           )
  
  l1 <- glmmLasso(hitsRG ~ 
                    timeActive_sc +
                    as.factor(computerVision) + 
                    IDexperience_allIDs_sc + 
                    obs1rankLevel_sc +
                    nObs_biome_sc,
                  rnd = list(UNREGION1 = ~1), 
                  lambda = 50, 
                  data = mdf2,
                  family = binomial(link = "logit"),
                  final.re = T)
  
  s <- summary(l1)
  
  o <- s$coefficients %>% 
    na.omit %>% 
    {data.frame(param = rownames(.), iteration = i,
                Estimate = .[,1], zValue = .[,3], pValue = .[,4],
                sig = {.[,4] < 0.05} , row.names = NULL)}
  
  paramEsts2[[i]] <- o
}

saveRDS(paramEsts2, file = file.path(wd$bin, "paramEsts2.rds"))
paramEsts2 %>% 
  bind_rows() %>% 
  group_by(param) %>% 
  summarise(count = n())

# if parameter is retained in 8 out of 10 runs, keep it for single model with a 
# bunch of datapoints, here we are removing computer vision and nObs_biome_sc

df2 <- dbGetQuery(iNat, "SELECT * FROM obs_info2") %>% 
  makeNiceColumns()
saveRDS(attributes(df2$timeActive_s), file = file.path(wd$bin, "m2_timeActive_s_attributes.rds"))

mdf2 <- df2 %>% 
  filter(!is.na(hitsRG),
         !is.na(timeActive_sc),
         !is.na(computerVision),
         !is.na(IDexperience_allIDs_sc),
         !is.na(obs1rankLevel_sc),
         !is.na(nObs_biome_sc),
         !is.na(UNREGION1))

m2 <- glmer(hitsRG ~ 
                  timeActive_sc +
                  as.factor(computerVision) + 
                  IDexperience_allIDs_sc + 
                  obs1rankLevel_sc +
                  nObs_biome_sc +
                  (1 | UNREGION1),
              data = mdf2,
              family = binomial(link = "logit"),
              control = glmerControl(optimizer ="Nelder_Mead"))


summary(m2)
saveRDS(m2, file = file.path(wd$bin, "m2.rds"))
plot_model(m2)
plot_model(m2, type = "pred", terms = "IDexperience_allIDs_sc")
plot_model(m2, type = "pred", terms = "obs1rankLevel_sc")


# Model 3 -----------------------------------------------------------------


######## model 3 ################
# time to RG #
paramEsts_3 <- list()
for(i in 1:10){
  writeLines(paste(i))
  df3 <- dbGetQuery(iNat, "SELECT * FROM obs_info2 WHERE obs_ID IN (SELECT obs_ID FROM obs_info2 WHERE hitsRG = 1 ORDER BY RANDOM() LIMIT 11000);") %>% 
    makeNiceColumns() %>% 
    # remove NAs
    dplyr::filter(
      !is.na(IDexperience_preRG_sc),
      !is.na(nObs_biome_sc),
      !is.na(n_obs_gen_sc),
      !is.na(obs1rankLevel_sc),
      !is.na(n_obs_fam_sc),
      !is.na(UNREGION1)
      ) %>% 
    dplyr::slice_sample(n = 10000)
  
  l1 <- glmmLasso(timeToRG ~ 
                    timeActive_sc +
                    as.factor(computerVision) + 
                    IDexperience_preRG_sc + 
                    obs1rankLevel_sc +
                    n_obs_gen_sc +
                    n_obs_fam_sc +
                    nObs_biome_sc,
                  rnd = list(UNREGION1 = ~1), 
                  lambda = 50, 
                  data = df3,
                  final.re = T)
  s <- summary(l1)
  
  o <- s$coefficients %>% 
    na.omit %>% 
    {data.frame(param = rownames(.), iteration = i,
                Estimate = .[,1], zValue = .[,3], pValue = .[,4],
                 sig = {.[,4] < 0.05} , row.names = NULL)}
  
  paramEsts_3[[i]] <- o
}

saveRDS(paramEsts_3, file = file.path(wd$bin, "paramEsts_3.rds"))
paramEsts_3 %>% 
  bind_rows() %>% 
  group_by(param) %>% 
  summarise(count = n())

# if parameter is retained in 8 out of 10 runs, keep it for single model with a 
# bunch of datapoints

df3 <- dbGetQuery(iNat, "SELECT * FROM obs_info2 WHERE hitsRG = 1;") %>% 
  makeNiceColumns() %>% 
  # remove NAs
  dplyr::filter(
    !is.na(IDexperience_preRG_sc),
    !is.na(nObs_biome_sc),
    !is.na(n_obs_gen_sc),
    !is.na(obs1rankLevel_sc),
    !is.na(n_obs_fam_sc),
    !is.na(UNREGION1)
  )
saveRDS(attributes(df3$timeActive_s), file = file.path(wd$bin, "m3_timeActive_s_attributes.rds"))
  
library(lmerTest)

m3 <- lmer(timeToRG ~ 
             timeActive_sc +
             as.factor(computerVision) + 
             IDexperience_preRG_sc + 
             obs1rankLevel_sc +
             n_obs_gen_sc +
             n_obs_fam_sc +
             (1 | UNREGION1)
            ,
            data = df3,
            control = lmerControl(optimizer = "optimx", optCtrl=list(method='nlminb'))
)


saveRDS(m3, file = file.path(wd$bin, "m3.rds"))
## write a single model with 
summary(m3)
plot_model(m3)
plot_model(m3, type = "pred", terms = "IDexperience_preRG_sc")
plot_model(m3, type = "pred", terms = "obs1rankLevel_sc")


# Model 4 -----------------------------------------------------------------


## Model target params ----
# timeActive
# obs1rankLevel
# computerVision
# IDexperience_allIDs
# IDexperience_preRG
# n_ids_gen, n_ids_class, n_ids_fam
# n_obs_gen, n_obs_class, n_obs_fam
# nObs_gridID 
# geo_biome
# taxClass
# interaction btwn ID experiences

paramEsts4 <- list()
for(i in 1:10){
  writeLines(paste(i))
  df4 <- dbGetQuery(iNat, "SELECT * FROM obs_info2 WHERE obs_ID IN (SELECT obs_ID FROM obs_info2 WHERE hitsRG = 1 ORDER BY RANDOM() LIMIT 500000);") %>% 
    dplyr::mutate(IDexperience_postRG = IDexperience_allIDs - IDexperience_preRG) %>% 
    makeNiceColumns()

  mdf4 <- df4 %>% 
    filter(!is.na(leavesRG),
           !is.na(timeActive_sc),
           !is.na(computerVision),
           !is.na(IDexperience_allIDs_sc),
           !is.na(IDexperience_postRG_sc),
           !is.na(IDexperience_preRG_sc),
           !is.na(obs1rankLevel_sc),
           !is.na(nObs_biome_sc),
           !is.na(timeToRG_sc),
           !is.na(UNREGION1)) %>% 
    # Retain up to 30k pts from each group (leavesRG y/n). This resamples leavesRG == 1 more likely to be replaced across iterations.
    group_by(leavesRG) %>%
    slice_sample(n=30000) %>%
    ungroup() %>%
    # Select 10k rows
    slice_sample(n=20000)
  
  # Throw error if leavesRG==1 is too small.
  mdf4 %>% count(leavesRG) %>% dplyr::filter(leavesRG==1) %>% select(n) %>% unlist %>% {stopifnot(. >= 90/.02)}
  
  l1 <- glmmLasso(leavesRG ~ 
                    timeActive_sc +
                    timeToRG_sc +
                    as.factor(computerVision) + 
                    #IDexperience_preRG_sc +
                    #IDexperience_postRG_sc +
                    #IDexperience_preRG_sc:IDexperience_postRG_sc +
                    obs1rankLevel_sc +
                    nObs_biome_sc,
                  rnd = list(UNREGION1 = ~1), 
                  lambda = 50, 
                  data = mdf4,
                  family = binomial(link = "logit"),
                  final.re = T)
  
  s <- summary(l1)
  
  o <- s$coefficients %>% 
    na.omit %>% 
    {data.frame(param = rownames(.), iteration = i,
                Estimate = .[,1], zValue = .[,3], pValue = .[,4],
                sig = {.[,4] < 0.05} , row.names = NULL)}
  
  paramEsts4[[i]] <- o
}

saveRDS(paramEsts4, file = file.path(wd$bin, "paramEsts4.rds"))
paramEsts4 %>% 
  bind_rows() %>% 
  group_by(param) %>% 
  summarise(count = n())


# run lasso on equally distributed dataset
df4 <- dbGetQuery(iNat, "SELECT * FROM obs_info2 WHERE hitsRG = 1")

# df4 %>% 
#   {.[,!duplicated(names(.))]} %>% 
#   dplyr::mutate(IDexperience_postRG = IDexperience_allIDs - IDexperience_preRG) %>% 
#   dplyr::filter(leavesRG_notes != "is RG, likely taxon swap (RG-tax is not active)") %>% 
#   group_by(leavesRG) %>% 
#   slice_sample(n=20000) %>% 
#   ggplot(aes(x = IDexperience_postRG, y= IDexperience_preRG, color = factor(leavesRG))) + geom_point(alpha = 0.1)
# 
df4 <- df4 %>%
  {.[,!duplicated(names(.))]} %>% 
  dplyr::mutate(IDexperience_postRG = IDexperience_allIDs - IDexperience_preRG) %>%
  makeNiceColumns()
saveRDS(attributes(df4$timeActive_s), file = file.path(wd$bin, "m4_timeActive_s_attributes.rds"))

mdf4 <- df4 %>% 
  dplyr::filter(!is.na(leavesRG),
         !is.na(timeActive_sc),
         !is.na(computerVision),
         #!is.na(IDexperience_preRG_sc),
         !is.na(obs1rankLevel_sc),
         !is.na(nObs_biome_sc),
         !is.na(timeToRG_sc),
         !is.na(UNREGION1))

##  IDexperience_preRG
m4 <- glmer(leavesRG ~ 
              timeActive_sc +
              poly(timeToRG_sc,2) +
              computerVision +
              #IDexperience_preRG_sc +
              #IDexperience_postRG_sc +
              #n_obs_gen_sc +
              #n_obs_fam_sc +
              obs1rankLevel_sc +
              nObs_biome_sc +
              (1 | UNREGION1),
            data = mdf4,
            family = binomial(link = "logit"),
            control = glmerControl(optimizer ="Nelder_Mead"))

saveRDS(m4, file = file.path(wd$bin, "m4.rds"))
summary(m4)
plot_model(m4)
plot_model(m4, type = "pred")
plot_model(m4, type = "pred", terms = "timeToRG_sc [all]")


# Time to RG followup -----------------------------------------------------

df_timeToRG <- dbGetQuery(iNat, 
           "SELECT * FROM 
             (SELECT * FROM obs_info2 WHERE hitsRG = 1) AS oi
             LEFT JOIN (SELECT * FROM obs WHERE obs_ID IN (SELECT obs_ID FROM obs_info2 WHERE hitsRG = 1)) AS obs ON oi.`obs_ID` = obs.`obs_ID`;")
fwrite(df_timeToRG, file = file.path(wd$bin, "df_timeToRG.csv"), row.names = F)




