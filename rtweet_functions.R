
## **********************************
## Packages
## **********************************

library(tidyverse)
library(rtweet) ## create token(s) separately


## **********************************
## Many token functions
## **********************************

# To do:
#   
#   make a manyToken variant of lookup_users

manyTokens_get_followers <- function(u, token) {
  
  user_info <- lookup_users(u, token = token)
  fc <- user_info$followers_count
  rl <- rate_limit(token, "get_followers")
  rl$remaining <- rl$remaining * 5e3
  
  message(user_info$screen_name, " has ", scales::comma(fc), " followers ")
  message("number of queries: ",  ceiling(fc / 75000))
  
  if (fc == 0) return(list(NULL))
  
  # ******************************************************
  # Case 1: one token is enough
  # ******************************************************
  
  if (any(rl$remaining >= fc)) {
    i <- which(rl$remaining >= fc)[[1]]
    followers <- get_followers(u, n = fc, token = token[[i]]) %>% 
      pull(user_id)
    
    # ******************************************************
    # Case 2: a combination of the existing tokens is enough
    # ******************************************************
    
  } else if (sum(rl$remaining) >= fc) {
    token_index <- which(rl$remaining > 0) 
    output <- vector("list", length(token_index))
    
    output[[1]] <- get_followers(
      user = u, n = rl$remaining[token_index[1]], 
      token = token[token_index[1]]
    )
    
    for (i in seq_along(token_index)[-1]) {
      output[[i]] <- get_followers(
        user = u, n = rl$remaining[token_index[i]], 
        token = token[token_index[i]],
        page = next_cursor(output[[i - 1]])
      )
    }
    
    followers <- bind_rows(output) %>% 
      drop_na() %>% 
      distinct() %>% 
      pull(user_id)
    
    # ******************************************************
    # Case 3: none of the tokens are enough (make loop)
    # ******************************************************
    
  } else {
    
    ## wait some time if all tokens are exhausted...
    
    if (sum(rl$remaining) == 0) { 
      message("Token reset in ", round(median(rl$reset), digits = 2), " mins...")
      Sys.sleep(as.numeric(median(abs(rl$reset)), "secs") + 2)  ## abs is to mitigate a quirky error
      rl <- rate_limit(token, "get_followers")                  ## in which reset has negative values
      rl$remaining <- rl$remaining * 5e3
      
    }
    
    output <- list()
    i <- 1
    output[[i]] <- get_followers(
      user = u, n = max(rl$remaining), 
      token = token[[which(rl$remaining == max(rl$remaining ))[1]]]
    )
    
    repeat {
      
      i <- i + 1
      rl <- rate_limit(token, "get_followers")
      rl$remaining <- rl$remaining * 5e3
      
      if (sum(rl$remaining) == 0) {   # wait min time possible if all tokens are exhausted
        message("Wait for ", round(median(rl$reset), digits = 2), " mins...")
        Sys.sleep(as.numeric(median(abs(rl$reset)), "secs") + 2)  ## abs is to mitigate a quirky error
        rl <- rate_limit(token, "get_followers")
        rl$remaining <- rl$remaining * 5e3
      }
      
      
      while (sum(rl$remaining) == 0) {            ## Sometimes the tokens don't reset when they're supposed to.
        Sys.sleep(30)                             ## If all works well, this little snippet should never execute.
        rl <- rate_limit(token, "get_followers") 
        rl$remaining <- rl$remaining * 5e3
      }
      
      token_index <- which(rl$remaining == max(rl$remaining))[[1]]
      
      output[[i]] <- get_followers(
        user = u, n = max(rl$remaining),
        token = token[[token_index]],
        page = next_cursor(output[[i - 1]])
      )
      
      if (nrow(output[[i]]) < max(rl$remaining)) {
        break
      }
    }
    
    followers <- bind_rows(output) %>% 
      drop_na() %>% 
      distinct() %>% 
      pull(user_id)
  }
  
  message("Followers: information about ", user_info$name, " (to), from other nodes")
  message("Use tibble::enframe(name = 'to', value = 'from') to get edge list")
  output <- list(user_id = followers) 
  names(output) <- user_info$user_id
  output
}


## -----------------------------------------

manyTokens_get_friends <- function(u, token) {
  
  user_info <- lookup_users(u, token = token)
  fc <- user_info$friends_count
  rl <- rate_limit(token, "get_friends")
  rl$remaining <- rl$remaining * 5e3
  
  message(paste0("\n", user_info$screen_name, " is following ", scales::comma(fc), " users ", "(approx. ",  ceiling(fc / 75000), " queries are required)"))
  message(user_info$screen_name, " is following ", scales::comma(fc), " users ")
  message("number of queries: ",  ceiling(fc / 75000))
  
  if (fc == 0) return(list(NULL))
  
  # ******************************************************
  # Case 1: one token is enough
  # ******************************************************

  if (any(rl$remaining >= fc)) {
    i <- which(rl$remaining >= fc)[[1]]
    friends <- get_friends(u, n = fc, token = token[[i]]) %>%
      pull(user_id)
    
    # ******************************************************
    # Case 2: a combination of the existing tokens is enough
    # ******************************************************
    
    } else if (sum(rl$remaining) >= fc) {
      token_index <- which(rl > 0)
      output <- vector("list", length(token_index))
      
      output[[1]] <- get_friends(
        user = u, n = rl[token_index[1]],
        token = token[token_index[1]]
        )
      
      for (i in seq_along(token_index)[-1]) {
        output[[i]] <- get_friends(
          user = u, n = rl[token_index[i]],
          token = token[token_index[i]],
          page = next_cursor(output[[i - 1]])
        )
      }
      
      friends <- bind_rows(output) %>%
        drop_na() %>%
        distinct() %>%
        pull(user_id)
      
      # ******************************************************
      # Case 3: none of the tokens are enough (make loop)
      # ******************************************************
      
      } else {

        ## wait some time if all tokens are exhausted...

        if (sum(rl$remaining) == 0) {
          message("Token reset in ", round(median(rl$reset), digits = 2), " mins...")
          Sys.sleep(as.numeric(median(abs(rl$reset)), "secs") + 2)  ## abs is to mitigate a quirky error
          rl <- rate_limit(token, "get_followers")                  ## in which reset has negative values
          rl$remaining <- rl$remaining * 5e3
        }
        
        output <- list()
        i <- 1
        output[[i]] <- get_friends(
          user = u, n = max(rl$remaining),
          token = token[[which(rl == max(rl))[1]]]
          )
        
        repeat {
          
          i <- i + 1
          rl <- rate_limit(token, "get_friends")
          rl$remaining <- rl$remaining * 5e3

          if (sum(rl$remaining) == 0) {   # wait min time possible if all tokens are exhausted
            message("Wait for ", round(median(rl$reset), digits = 2), " mins...")
            Sys.sleep(as.numeric(median(abs(rl$reset)), "secs") + 2)  ## abs is to mitigate a quirky error
            rl <- rate_limit(token, "get_friends")
            rl$remaining <- rl$remaining * 5e3
          }
          
          while (sum(rl$remaining) == 0) {            ## Sometimes the tokens don't reset when they're supposed to.
            Sys.sleep(30)                             ## If all works well, this little snippet should never execute.
            rl <- rate_limit(token, "get_friends") 
            rl$remaining <- rl$remaining * 5e3
          }
          
          token_index <- which(rl$remaining == max(rl$remaining))[[1]]

          output[[i]] <- get_friends(
            user = u, n = max(rl$remaining),
            token = token[[token_index]],
            page = next_cursor(output[[i - 1]])
          )
          
          if (nrow(output[[i]]) < max(rl$remaining)) {
            break
          }
          
        }
        
        friends <- bind_rows(output) %>%
          drop_na() %>%
          distinct() %>%
          pull(user_id)
      }
  
  message("Friends: information about ", user_info$name, " (from), to other nodes")
  message("Use tibble::enframe(name = 'from', value = 'to') to get edge list")
  output <- list(user_id = friends) 
  names(output) <- user_info$user_id
  output

}

manyTokens_lookup_users <- function(u_list, token) {
  
  # rtweet::lookup_users() returns data on up to 90,000 Twitter users, then we
  # have to wait 15 minutes
  
  rl <- rate_limit(token, "lookup_users")
  rl$remaining <- rl$remaining * 100
  
  u_length <- length(u_list)
  
  # ******************************************************
  # Case 1: one token is enough
  # ******************************************************
  
  if (any(rl$remaining >= u_length)) {
    i <- which(rl$remaining >= u_length)[[1]]
    user_info <- lookup_users(u_list, token = token[[i]])
  
  # ******************************************************
  # Case 2: a combination of the existing tokens is enough
  # ******************************************************
    
  } else if (sum(rl$remaining) >= u_length) {
    
    usable_tokens <- which(rl$remaining > 0) 
    output_length <- which(cumsum(rl$remaining[usable_tokens]) >= u_length)[[1]]
    
    output <- vector("list", output_length)
    
    u_left <- u_list
    
    for (i in seq_along(output)) {
      
      if (i > 1) {
        u_left <- setdiff(u_left, output[[i - 1]]$user_id)
      }
      output[[i]] <- lookup_users(u_left, token = token[usable_tokens[[i]]])
    }

    user_info <- bind_rows(output)
    
    # ******************************************************
    # Case 3: none of the tokens are enough (make loop)
    # ******************************************************
    
  } else {
    
    if (sum(rl$remaining) == 0) { 
      message("Token reset in ", round(median(rl$reset), digits = 2), " mins...")
      Sys.sleep(as.numeric(median(abs(rl$reset)), "secs") + 2)  ## abs is to mitigate a quirky error
      rl <- rate_limit(token, "lookup_users")                  ## in which reset has negative values
      rl$remaining <- rl$remaining * 100
    }
    
    output <- list()
    u_left <- u_list
    i <- 1
    output[[i]] <- lookup_users(u_left, token = token[[which(rl$remaining == max(rl$remaining ))[1]]])
    
    repeat {
      
      i <- i + 1
      rl <- rate_limit(token, "lookup_users")
      rl$remaining <- rl$remaining * 100
      u_left <- setdiff(u_left, output[[i - 1]]$user_id)
      
      if (sum(rl$remaining) == 0) {   # wait min time possible if all tokens are exhausted
        message("Wait for ", round(median(rl$reset), digits = 2), " mins...")
        Sys.sleep(as.numeric(median(abs(rl$reset)), "secs") + 2)  ## abs is to mitigate a quirky error
        rl <- rate_limit(token, "lookup_users")
        rl$remaining <- rl$remaining * 100
      }
      
      while (sum(rl$remaining) == 0) {            ## Sometimes the tokens don't reset when they're supposed to.
        Sys.sleep(30)                             ## If all works well, this little snippet should never execute.
        rl <- rate_limit(token, "lookup_users") 
        rl$remaining <- rl$remaining * 5e3
      }
      
      token_index <- which(rl$remaining == max(rl$remaining))[[1]]
      
      u_left <- setdiff(u_left, output[[i - 1]]$user_id)
      output[[i]] <- lookup_users(u_left, token = token[[which(rl$remaining == max(rl$remaining))[1]]])
      
      if (nrow(output[[i]]) < max(rl$remaining)) {
        break
      }
    }
    
    user_info <- bind_rows(output) 
  }
  
  return(user_info)
}


## Note, to avoid 

## **********************************
## Quick influence ------- 
## **********************************

influence_data <- function(user, token) {
    
    user_info <- lookup_users(user, token = token[[sample(length(token), size = 1)]])
    
    user_followers <- manyTokens_get_followers(user_info$user_id, token = token)
    followers_details <- manyTokens_lookup_users(unlist(user_followers), token = token)
    
    primary_influence <- sum(c(followers_details$followers_count, user_info$followers_count))
    message(user_info$name, " has a primary influence of ", scales::comma(primary_influence))
  
  return(list(user_info = user_info, follower_details = followers_details, primary_influence = primary_influence))
  
}

# ## Proof of concept
# andres <- influence_data("acastroaraujo", token)
# sergio <- influence_data("SergioChaparro8", token)
# paola <- influence_data("PMolanoA", token)
# 
# df <- tibble(
#   sergio = list(sergio$follower_details$followers_count),
#   andres = list(andres$follower_details$followers_count),
#   paola = list(paola$follower_details$followers_count)
# )
# 
# 
# df %>% 
#   pivot_longer(cols = everything()) %>% 
#   unnest(cols = value) %>% 
#   filter(value > 0) %>% 
#   ggplot(aes(value, fill = fct_rev(name), color = fct_rev(name), y = ..count..)) +
#   geom_density(alpha = 0.2) +
#   scale_x_log10(labels = scales::comma) +
#   theme_minimal(base_family = "IBM Plex Sans") + 
#   scale_fill_viridis_d() + 
#   scale_color_viridis_d() +
#   theme(legend.position = "top") +
#   labs(color = NULL, fill = NULL, x = "\nseguidores de seguidores", y = "seguidores\n",
#        title = "Influencia, por número de seguidores de seguidores")


