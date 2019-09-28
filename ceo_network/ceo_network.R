
source("getEdgeList.R")

# friends <- get_edge_list_of_friends("schwartzie14")
# followers <- get_edge_list_of_followers("schwartzie14")
# core <- bind_rows(followers, friends)
# write_rds(core, "ceo_network/core_network.rds", compress = "gz")

core <- read_rds("ceo_network/core_network.rds")

followers_list <- setdiff(unique(core$from), "15586275")
friends_list <- setdiff(unique(core$to), "15586275")

# ***********************************************
# Followers
# ***********************************************

outfolder <- "ceo_network/data/followers/"
error_folder <- "ceo_network/data/followers_error/"
if (!dir.exists(outfolder)) dir.create(outfolder)
if (!dir.exists(error_folder)) dir.create(error_folder)

followers_done <- append(str_replace(list.files(outfolder), ".rds", ""), 
                         str_replace(list.files(error_folder), ".rds", ""))
followers_left <- sample(setdiff(followers_list, followers_done))

while (length(followers_left) > 0) { 
  
  new_user <- followers_left[[1]]
  
  friends <- try(get_edge_list_of_friends(new_user))
  followers <- try(get_edge_list_of_followers(new_user))
  
  if (class(friends)[[1]] == "try-error" | class(followers)[[1]] == "try-error") {
    error_output <- list(followers, friends)
    file_name <- paste0(error_folder, new_user, ".rds")
    write_rds(error_output, file_name, compress = "gz")
    followers_left <- followers_left[-which(followers_left %in% new_user)] ## int subsetting
    next
  }
  
  core <- bind_rows(followers, friends)
  file_name <- paste0(outfolder, new_user, ".rds")
  write_rds(core, file_name, compress = "gz")
  followers_left <- followers_left[-which(followers_left %in% new_user)] 
}


# ***********************************************
# Friends
# ***********************************************

outfolder <- "ceo_network/data/friends/"
error_folder <- "ceo_network/data/friends_error/"
if (!dir.exists(outfolder)) dir.create(outfolder)
if (!dir.exists(error_folder)) dir.create(error_folder)

friends_done <- append(str_replace(list.files(outfolder), ".rds", ""), 
                       str_replace(list.files(error_folder), ".rds", ""))
friends_left <- setdiff(friends_list, friends_done)

if (is_empty(list.files(error_folder))) {
  error_list <- c()  
} else {
  error_list <- read_rds(paste0(error_folder, "/list.rds"))
}


while (length(friends_left) > 0) { 
  
  new_user <- friends_left[[1]]
  
  friends <- try(get_edge_list_of_friends(new_user))
  followers <- try(get_edge_list_of_followers(new_user))
  
  if (class(friends)[[1]] == "try-error" | class(followers)[[1]] == "try-error") {
    error_output <- list(followers, friends)
    file_name <- paste0(error_folder, new_user, ".rds")
    write_rds(error_output, file_name, compress = "gz")
    friends_left <- friends_left[-which(friends_left %in% new_user)] ## int subsetting
    
    next
  }
  
  core <- bind_rows(followers, friends)
  file_name <- paste0(outfolder, new_user, ".rds")
  write_rds(core, file_name, compress = "gz")
  friends_left <- friends_left[-which(friends_left %in% new_user)] 
}




