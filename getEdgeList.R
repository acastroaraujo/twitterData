# To fix: When users with small amounts of followers are parsed through exhausted tokens,
# they get sent to option 3 and sometimes a sys.sleep error is produced.
# I believe that the whole i <- i + 1 scheme where only one i is needed breaks!!



# ********************************************************
# Setup; only use this function if you have multiple tokens
# ********************************************************

library(tidyverse)
source("createTokens.R")
length(token) ## number of tokens

# ********************************************************
# Main Function
# ********************************************************

get_edge_list_of_followers <- function(u) {
  
  user_info <- lookup_users(u)
  fc <- user_info$followers_count
  rl <- rate_limit(token, "get_followers") %>% 
    pull(remaining) * 5e3
  
  cat(paste0("\n", user_info$screen_name, " has ", scales::comma(fc), " followers "))
  cat(paste0("(approx. ",  ceiling(fc / 75000), " queries are required)\n"))
  
  if (fc == 0) return(list(NULL))
  
  # ******************************************************
  # Case 1: one token is enough
  # ******************************************************
  
  if (any(rl >= fc)) {
    i <- which(rl >= fc)[[1]]
    followers <- get_followers(u, n = fc, token = token[[i]]) %>% 
      pull(user_id)
    
  # ******************************************************
  # Case 2: a combination of the existing tokens is enough
  # ******************************************************
    
  } else if (sum(rl) >= fc) {
    token_index <- which(rl > 0) 
    output <- vector("list", length(token_index))
    
    output[[1]] <- get_followers(
      user = u, n = rl[token_index[1]], 
      token = token[token_index[1]]
      )
    
    for (i in seq_along(token_index)[-1]) {
      output[[i]] <- get_followers(
        user = u, n = rl[token_index[i]], 
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
    
    if (sum(rl) == 0) { 
      cat("Token reset in", median(rate_limit(token, "get_followers") %>% pull(reset)), "mins...\n")
      Sys.sleep(median(rate_limit(token, "get_followers") %>% pull(reset)) * 60 + 10)
      rl <- rate_limit(token, "get_followers") %>% pull(remaining) * 5e3
    }
    
    output <- list()
    i <- 1
    output[[i]] <- get_followers(
      user = u, n = max(rl), 
      token = token[[which(rl == max(rl))[1]]]
      )
    
    repeat {
      
      i <- i + 1
      df_rate_limit <- rate_limit(token, "get_followers")
      rl <- df_rate_limit %>% pull(remaining)
      
      if (sum(rl) == 0) {   # wait min time possible if all tokens are exhausted
        cat("Wait for", median(df_rate_limit %>% pull(reset)), "mins...\n")
        t0 <- df_rate_limit %>% pull(timestamp)
        t1 <- df_rate_limit %>% pull(reset_at)
        Sys.sleep(median(difftime(t1, t0, units = "secs")) + 5)
      }
      
      rl <- rate_limit(token, "get_followers") %>% pull(remaining)
      
      ## Sometimes the tokens don't reset when they're supposed to.
      ## If all works well, this little snippet should never execute.
      while (sum(rl) == 0) { 
        Sys.sleep(30)
        rl <- rate_limit(token, "get_followers") %>% pull(remaining)
      }
      
      token_index <- which(rl == max(rl))[[1]]
      
      output[[i]] <- get_followers(
        user = u, n = (max(rl) * 5e3),
        token = token[[token_index]],
        page = next_cursor(output[[i - 1]])
      )
      
      if (nrow(output[[i]]) < max(rl) * 5e3) {
        break
      }
    }
    
    followers <- bind_rows(output) %>% 
      drop_na() %>% 
      distinct() %>% 
      pull(user_id)
  }
  return(tibble(from = followers, to = user_info$user_id))
}


## -----------------------------------------

get_edge_list_of_friends <- function(u) {
  
  user_info <- lookup_users(u)
  fc <- user_info$friends_count
  rl <- rate_limit(token, "get_friends") %>% 
    pull(remaining) * 5e3
  
  cat(paste0("\n", user_info$screen_name, " is following ", scales::comma(fc), " users "))
  cat(paste0("(approx. ",  ceiling(fc / 75000), " queries are required)\n"))
  
  if (fc == 0) return(list(NULL))
  
  # ******************************************************
  # Case 1: one token is enough
  # ******************************************************
  
  if (any(rl >= fc)) {
    i <- which(rl >= fc)[[1]]
    friends <- get_friends(u, n = fc, token = token[[i]]) %>% 
      pull(user_id)
    
    # ******************************************************
    # Case 2: a combination of the existing tokens is enough
    # ******************************************************
    
  } else if (sum(rl) >= fc) {
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
    
    if (sum(rl) == 0) { 
      cat("Token reset in", median(rate_limit(token, "get_friends") %>% pull(reset)), "mins...\n")
      Sys.sleep(median(rate_limit(token, "get_friends") %>% pull(reset)) * 60 + 10)
      rl <- rate_limit(token, "get_friends") %>% pull(remaining) * 5e3
    }
    
    output <- list()
    i <- 1
    output[[i]] <- get_friends(
      user = u, n = max(rl), 
      token = token[[which(rl == max(rl))[1]]]
    )
    
    repeat {
      
      i <- i + 1
      df_rate_limit <- rate_limit(token, "get_friends")
      rl <- df_rate_limit %>% pull(remaining)
      
      if (sum(rl) == 0) {   # wait min time possible if all tokens are exhausted
        cat("Wait for", median(df_rate_limit %>% pull(reset)), "mins...\n")
        t0 <- df_rate_limit %>% pull(timestamp)
        t1 <- df_rate_limit %>% pull(reset_at)
        Sys.sleep(median(difftime(t1, t0, units = "secs")) + 5)
      }
      
      rl <- rate_limit(token, "get_friends") %>% pull(remaining)
      
      ## Sometimes the tokens don't reset when they're supposed to.
      ## If all works well, this little snippet should never execute.
      while (sum(rl) == 0) { 
        Sys.sleep(30)
        rl <- rate_limit(token, "get_friends") %>% pull(remaining)
      }
      
      token_index <- which(rl == max(rl))[[1]]
      
      output[[i]] <- get_friends(
        user = u, n = (max(rl) * 5e3),
        token = token[[token_index]],
        page = next_cursor(output[[i - 1]])
      )
      
      if (nrow(output[[i]]) < max(rl) * 5e3) {
        break
      }
    }
    
    friends <- bind_rows(output) %>% 
      drop_na() %>% 
      distinct() %>% 
      pull(user_id)
  }
  return(tibble(from = user_info$user_id, to = friends))
}

# ********************************************************
# Demonstration
# rate_limit(token, query = "get_followers")
# rate_limit(token, query = "get_friends")
# ********************************************************

#outfolder <- "followers/"
#if (!dir.exists(outfolder)) dir.create("followers/")

# u <- "schwartzie14"
# out <- get_edge_list_of_followers(u)
# out2 <- get_edge_list_of_friends(u)

# file_name <- paste0(outfolder, u, ".rds")
# write_rds(out, file_name, compress = "gz")

