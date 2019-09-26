source("getEdgeList.R")

# friends <- get_edge_list_of_friends("schwartzie14")
# followers <- get_edge_list_of_followers("schwartzie14")
# core <- bind_rows(followers, friends)
# write_rds(core, "ceo_network/core_network.rds", compress = "gz")

core <- read_rds("ceo_network/core_network.rds")

followers_list <- setdiff(unique(core$from), "15586275")
friends_list <- setdiff(unique(core$to), "15586275")

# ***********************************************
# Friends
# ***********************************************

outfolder <- "ceo_network/data/friends/"
if (!dir.exists(outfolder)) dir.create(outfolder)

friends_done <- str_replace(list.files(outfolder), ".rds", "")
friends_left <- setdiff(friends_list, friends_done)

while (length(friends_left) > 0) { 
  
  new_user <- friends_left[[1]]
  
  friends <- try(get_edge_list_of_friends(new_user))
  followers <- try(get_edge_list_of_followers(new_user))
  
  if (class(friends)[[1]] == "try-error" | class(followers)[[1]] == "try-error") {
    friends_left <- friends_left[-which(friends_left %in% new_user)] ## int subsetting
    next
  }
  
  core <- bind_rows(followers, friends)
  file_name <- paste0(outfolder, new_user, ".rds")
  write_rds(core, file_name, compress = "gz")
  friends_left <- friends_left[-which(friends_left %in% new_user)] 
}

# ***********************************************
# Followers
# ***********************************************

outfolder <- "ceo_network/data/followers/"
if (!dir.exists(outfolder)) dir.create(outfolder)

followers_done <- str_replace(list.files(outfolder), ".rds", "")
followers_left <- setdiff(followers_list, followers_done)

while (length(followers_left) > 0) { 
  
  new_user <- followers_left[[1]]
  
  friends <- try(get_edge_list_of_friends(new_user))
  followers <- try(get_edge_list_of_followers(new_user))
  
  if (class(friends)[[1]] == "try-error" | class(followers)[[1]] == "try-error") {
    followers_left <- followers_left[-which(followers_left %in% new_user)] ## int subsetting
    next
  }
  
  core <- bind_rows(followers, friends)
  file_name <- paste0(outfolder, new_user, ".rds")
  write_rds(core, file_name, compress = "gz")
  followers_left <- followers_left[-which(followers_left %in% new_user)] 
}
