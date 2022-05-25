# Models

library(RMySQL)
library(tidyverse)
library(MCMCglmm)

con = dbConnect(MySQL(), group = "krsp-aws")

#TODO: Modularize .>.
suppressWarnings({
  source("scripts/R/lifetime_data.R")
  source("scripts/R/densities.R")
})

sc = 100
verbosity = TRUE

squirrel_densities = squirrelDensities(con)
midden_densities = middenDensities(con)

densities = bind_rows(squirrel_densities, midden_densities) %>% 
  filter(month == 8) %>% 
  select(squirrel_id, year, local_density) %>% 
  group_by(squirrel_id, year) %>% 
  summarize(local_density = mean(local_density))

litters = litters %>% 
  left_join(
    .,
    densities,
    by = c("dam_id" = "squirrel_id", "year" = "year")
  )
  

# Visualizations ----------------------------------------------------------

ggplot(litters, aes(x = local_density, y = part, group = dam_id)) +
  geom_line()

ggplot(litters, aes(x = local_density, y = litter_size, group = dam_id)) +
  geom_line()

ggplot(litters, aes(x = local_density, y = mean_growth, group = dam_id)) +
  geom_line()



# Priors -------------------------------------------------------------------------

int_prior = list(G = list(G1 = list(V = 1, nu = 1,
                                    alpha.mu = 0,
                                    alpha.V = 25^2),
                          G2 = list(V = 1, nu = 1)))

no_cov_prior = list(G = list(G1 = list(V=diag(2), nu=2, alpha.mu=rep(0,2), alpha.V=diag(2)*625),
                            G2 = list(V=1, nu=1, alpha.mu=0, alpha.V=625)))

cov_prior = list(R = list(V = 1, nu = 0.002),
                 G = list(G1 = list(V=diag(2), nu=2, alpha.mu=rep(0,2), alpha.V=diag(2)*625),
                             G2 = list(V=1, nu=1, alpha.mu=0, alpha.V=625)))

# PART models  -------------------------------------------------------------------------
part_dat = litters %>% 
  select(part, dam_id, grid, year, litter_size, dam_age, cone_index, cone_index_tm1,
         local_density) %>% 
  filter(grid %in% c("KL", "SU")) %>% 
  group_by(year, grid) %>% 
  mutate(density_sc = scale(local_density, center = T, scale = T)[,1],
         part_sc = scale(part, center = T, scale = T)[,1]) %>% 
  ungroup() %>%
  group_by(dam_id) %>% 
  mutate(IndAvgDen = mean(density_sc, na.rm = TRUE),
         IndCenDen = density_sc - IndAvgDen) %>% 
  ungroup() %>% 
  mutate(across(c(grid, year, dam_id), as_factor)) %>% 
  filter(!is.na(dam_age),
         !is.na(IndCenDen),
         !is.na(cone_index_tm1))

part_int = MCMCglmm(part_sc ~ poly(dam_age, 2, raw = TRUE) +
                      IndCenDen +
                      IndAvgDen +
                      scale(cone_index_tm1) +
                      grid,
                    random = ~ dam_id + year,
                    family = "gaussian",
                    prior = int_prior,
                    nitt = 13000*sc,
                    burnin = 3000*sc,
                    thin = 10*sc,
                    verbose = verbosity,
                    data = as.data.frame(part_dat),
                    pr = TRUE,
                    saveX = TRUE,
                    saveZ = TRUE)
summary(part_int)
plot(part_int)



part_plast_no_cov = MCMCglmm(part_sc ~ poly(dam_age, 2, raw = TRUE) +
                               IndCenDen +
                               IndAvgDen +
                               scale(cone_index_tm1) +
                               grid,
                             random = ~ idh(1+IndCenDen):dam_id + year,
                             #rcov = ~ units,
                             family = "gaussian",
                             prior = no_cov_prior,
                             nitt = 13000*sc,
                             burnin = 3000*sc,
                             thin = 10*sc,
                             verbose = verbosity,
                             data = as.data.frame(part_dat),
                             pr = TRUE,
                             saveX = TRUE,
                             saveZ = TRUE)

summary(part_plast_no_cov)
plot(part_plast_no_cov)

part_plast_cov = MCMCglmm(part_sc ~ poly(dam_age, 2, raw = TRUE) +
                            IndCenDen +
                            IndAvgDen +
                            scale(cone_index_tm1) +
                            grid,
                          random = ~ us(1+IndCenDen):dam_id + year,
                          rcov = ~ units,
                          family = "gaussian",
                          prior = cov_prior,
                          nitt = 13000*sc,
                          burnin = 3000*sc,
                          thin = 10*sc,
                          verbose = verbosity,
                          data = as.data.frame(part_dat),
                          pr = TRUE,
                          saveX = TRUE,
                          saveZ = TRUE)

summary(part_plast_cov)  
plot(part_plast_cov)

# GROWTH models  -------------------------------------------------------------------------
growth_dat = litters %>% 
  select(mean_growth, dam_id, grid, year, litter_size, dam_age, cone_index, cone_index_tm1,
         local_density) %>% 
  filter(grid %in% c("KL", "SU")) %>% 
  group_by(year, grid) %>% 
  mutate(density_sc = scale(local_density, center = T, scale = T)[,1],
         growth_sc = scale(mean_growth, center = T, scale = T)[,1]) %>% 
  ungroup() %>%
  group_by(dam_id) %>% 
  mutate(IndAvgDen = mean(density_sc, na.rm = TRUE),
         IndCenDen = density_sc - IndAvgDen) %>% 
  ungroup() %>% 
  mutate(across(c(grid, year, dam_id), as_factor)) %>% 
  filter(!is.na(dam_age),
         !is.na(growth_sc),
         !is.na(IndCenDen),
         !is.na(cone_index_tm1))

growth_int = MCMCglmm(growth_sc ~ poly(dam_age, 2, raw = TRUE) +
                      IndCenDen +
                      IndAvgDen +
                      scale(cone_index_tm1) +
                      grid,
                    random = ~ dam_id + year,
                    family = "gaussian",
                    prior = int_prior,
                    nitt = 13000*sc,
                    burnin = 3000*sc,
                    thin = 10*sc,
                    verbose = verbosity,
                    data = as.data.frame(growth_dat),
                    pr = TRUE,
                    saveX = TRUE,
                    saveZ = TRUE)
summary(growth_int)
plot(growth_int)



growth_plast_no_cov = MCMCglmm(growth_sc ~ poly(dam_age, 2, raw = TRUE) +
                               IndCenDen +
                               IndAvgDen +
                               scale(cone_index_tm1) +
                               grid,
                             random = ~ idh(1+IndCenDen):dam_id + year,
                             #rcov = ~ units,
                             family = "gaussian",
                             prior = no_cov_prior,
                             nitt = 13000*sc,
                             burnin = 3000*sc,
                             thin = 10*sc,
                             verbose = verbosity,
                             data = as.data.frame(growth_dat),
                             pr = TRUE,
                             saveX = TRUE,
                             saveZ = TRUE)

summary(growth_plast_no_cov)
plot(growth_plast_no_cov)

growth_plast_cov = MCMCglmm(growth_sc ~ poly(dam_age, 2, raw = TRUE) +
                            IndCenDen +
                            IndAvgDen +
                            scale(cone_index_tm1) +
                            grid,
                          random = ~ us(1+IndCenDen):dam_id + year,
                          rcov = ~ units,
                          family = "gaussian",
                          prior = cov_prior,
                          nitt = 13000*sc,
                          burnin = 3000*sc,
                          thin = 10*sc,
                          verbose = verbosity,
                          data = as.data.frame(growth_dat),
                          pr = TRUE,
                          saveX = TRUE,
                          saveZ = TRUE)

summary(growth_plast_cov)  
plot(growth_plast_cov)





# DIC checks --------------------------------------------------------------
tribble(
  ~model, ~DIC,
  "Intercept Only", part_int$DIC,
  "No Covariance", part_plast_no_cov$DIC,
  "Covariance", part_plast_cov$DIC
)

tribble(
  ~model, ~DIC,
  "Intercept Only", growth_int$DIC,
  "No Covariance", growth_plast_no_cov$DIC,
  "Covariance", growth_plast_cov$DIC
)


saveRDS(part_int, file = "output/models/part_int.rds")
saveRDS(part_plast_no_cov, file = "output/models/part_plast_no_cov.rds")
saveRDS(part_plast_cov, file = "output/models/part_plast_covint.rds")

saveRDS(growth_int, file = "output/models/growth_int.rds")
saveRDS(growth_plast_no_cov, file = "output/models/growth_plast_no_cov.rds")
saveRDS(growth_plast_cov, file = "output/models/growth_plast_covint.rds")


