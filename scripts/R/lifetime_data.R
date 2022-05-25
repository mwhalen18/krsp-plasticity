library(RMySQL)
library(tidyverse)

con = dbConnect(MySQL(), group = "krsp-aws")


# JUVE TABLE
litters <- tbl(con, "juvenile") %>%
  select(litter_id
         , sex
         , squirrel_id
         , n1_weight = weight
         , n2_weight = tagWT) %>%
  # JOIN LITTERS TO JUVENILE to get individual growth info
  left_join(.,
            tbl(con, "litter") %>%
              filter(ln == 1) %>%
              mutate(part = DAYOFYEAR(fieldBDate)) %>%
              select(litter_id = id
                     , fieldBDate
                     , grid
                     , food
                     , part
                     , n1_date = date1
                     , n2_date = tagDt
                     , dam_id = squirrel_id
                     , year = yr),
            by = "litter_id") %>%
  # CALCULATE GROWTH
  mutate(nest_days = DAYOFYEAR(n2_date) - DAYOFYEAR(n1_date),
         growth = (n2_weight - n1_weight) / nest_days) %>%
  # JOIN LITTER info to fla2 to get juvenile survival
  left_join(.,
            tbl(con, "flastall2") %>%
              select(squirrel_id, datee, f2),
            by = c("squirrel_id")) %>%
  mutate(survived_200d = DATEDIFF(datee, fieldBDate) >= 200) %>%
  # Get LITTER level summaries
  group_by(litter_id, dam_id, grid, food, year) %>%
  summarize(litter_size = n(),
            litter_fit = sum(survived_200d),
            part = mean(part),
            fieldBDate = fieldBDate,
            n1_date = mean(DAYOFYEAR(n1_date)),
            n2_date = mean(DAYOFYEAR(n2_date)),
            mean_growth = mean(growth),
            .groups = "drop") %>%
  # Join summaries to FLA to get dam age info
  left_join(.,
            tbl(con, "flastall") %>%
              select(squirrel_id, byear),
            by = c("dam_id" = "squirrel_id")) %>%
  mutate(dam_age = byear - YEAR(fieldBDate))


# bring in cone data
cones_sql <- readr::read_file("scripts/sql/cone_counts.sql")
cones_grids_years <- con %>%
  dbGetQuery(cones_sql) %>%
  tibble()

litters <- collect(litters) %>%
  left_join(cones_grids_years,
            by = c("grid", "year")) %>%
  mutate(prev_year = year - 1) %>%
  left_join(cones_grids_years,
            by = c("grid", "prev_year" = "year"),
            suffix = c("", "_tm1")) %>%
  select(-c(num_trees_tm1, cone_counts_tm1, EXP_tm1, Exp_label_tm1))




