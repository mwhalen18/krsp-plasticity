# Calculate densities from squirrel data
squirrelDensities = function(con, radius = 150, grids = c("KL", "SU"), years = 2012:2022){
  suppressMessages({
    require(lubridate)
    require(furrr)
    require(parallel)
    require(tictoc)
  })
  plan(multisession(workers = (parallel::detectCores() / 2) - 1))
  
  message(glue::glue("Using {(parallel::detectCores() / 2) - 1} cores"))
  
  squirrel_census = tbl(con, "census") %>% 
    select(squirrel_id, sex, grid = gr, date = census_date, locX = locx, locY = locy, census_fate = sq_fate) %>% 
    collect()
  
  census = squirrel_census %>% 
    mutate(date = ymd(date),
           year = year(date),
           month = month(date),
           locX = krsp::loc_to_numeric(locX),
           locY = krsp::loc_to_numeric(locY)) %>% 
    filter(!is.na(locX),
           !is.na(locY),
           grid %in% grids,
           year %in% years,
           !is.na(squirrel_id))
  
  data = census %>% 
    mutate(local_density = 0)
  
  n = length(data$squirrel_id)
  census$ids = seq(n)
  
  message("generating local densities")
  fn = function(x){
    getNeighbors(x, census, data, radius)
  }
  tic()
  neighbors = furrr::future_map(census$ids, fn)
  toc()
  
  data$local_density = unlist(replace(neighbors, !sapply(neighbors, length), 0), recursive = T)
  return(data)
}

middenDensities = function(con, radius = 150, grids = c("KL", "SU"), years = 1988:2022){
  suppressMessages({
    require(dplyr)
    require(furrr)
    require(lubridate)
    require(parallel)
    require(tictoc)
  })
  
  plan(multisession(workers = (parallel::detectCores() / 2) - 1)) #starts a multi-session using n/2-1 cores
  
  message(glue::glue("Using {(parallel::detectCores() / 2) - 1} cores")) #indicate number of cores
  
  neighbors_all = data.frame()
  midden_densities = tbl(con, "dbamidden") %>% 
    select(squirrel_id, sex = Sex, grid, date, locX, locY, census_fate = fate, census_def = def) %>% 
    collect() #generate a working dataframe of individuals you want
  
  census = midden_densities %>% 
    mutate(date = ymd(date),
           year = year(date),
           month = month(date),
           locX = as.numeric(locX),
           locY = as.numeric(locY)) %>% 
    filter(!is.na(locX),
           !is.na(locY),
           !is.na(squirrel_id),
           grid %in% grids,
           year %in% years)
  
  data = census %>% 
    mutate(local_density = 0) #generate new column to store densities
  
  n = length(data$squirrel_id)
  census$ids = seq(n)
  message("generating local densities")
  
  fn = function(x){
    getNeighbors(x, census, data, radius) #get neighbors in local density
  }
  tic()
  neighbors = furrr::future_map(census$ids, fn) #map the neighbors function to our ids
  toc()
  
  data$local_density = unlist(replace(neighbors, !sapply(neighbors, length), 0), recursive = T) #convert list to vector and append to dataframe
  return(data)
}


getNeighbors = function(id, census, data, radius){#This function is parallelized to take in the census and data dfs and generate social neighborhoods
  require(dplyr)
  neighbors = census %>% 
    filter(census$grid == data$grid[id] &
             census$year == data$year[id] &
             census$month == data$month[id] & #All of this comes straigh from esiracusa's github page with minor tweaks
             #ie replacing {plyr} functions with {dplyr} functions
             #For small datasets the speed gain is minimal in human time
             #If running the entire dataset, the time is cut considerably...for the sacrifice of %complexity%
             #In other words I intentionally spent a whole day sitting at a bar optimizing this code no one will ever benefit from
             #FUN!
             (30*data$locX[id]-30 * census$locX)^2 + (30*data$locY[id] - 30*census$locY)^2 <=(radius)^2 &
             !census$squirrel_id[id] == data$squirrel_id) %>% 
    mutate(nind = length(unique(squirrel_id)),
           density = nind / ((pi*radius^2) / 10000)) %>% 
    pull(density) %>% 
    unique() #output will be a list
}

