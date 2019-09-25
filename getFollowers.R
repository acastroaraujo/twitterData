
# ********************************************************
# Setup; only use this function if you have multiple tokens
# ********************************************************

library(tidyverse)
source("createTokens.R")
length(token) ## number of tokens
outfolder <- "followers/"
if (!dir.exists(outfolder)) dir.create("followers/")

# ********************************************************
# Main Function
# ********************************************************

get_edge_list <- function(u) {

  user_info <- lookup_users(u)
  fc <- user_info$followers_count
  rl <- rate_limit(token, "get_followers") %>% 
    pull(remaining) * 5e3
  cat(user_info$screen_name, "has", fc, "followers...\n")
  cat("Approximately",  ceiling(fc / 75000), "queries are required...\n")
  
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
      cat("Token reset in", min(rate_limit(token, "get_followers") %>% pull(reset)), "mins...\n")
      Sys.sleep(min(rate_limit(token, "get_followers") %>% pull(reset)) * 60 + 10)
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
        cat("Wait for", min(df_rate_limit %>% pull(reset)), "mins...\n")
        t0 <- df_rate_limit %>% pull(timestamp)
        t1 <- df_rate_limit %>% pull(reset_at)
        Sys.sleep(min(difftime(t1, t0, units = "secs")) + 5)
      }
      
      rl <- rate_limit(token, "get_followers") %>% pull(remaining)
      
      ## Sometimes the tokens don't reset when they're supposed to.
      ## If all works well, this little snippet should never execute.
      while (sum(rl) == 0) { 
        rl <- rate_limit(token, "get_followers") %>% pull(remaining)
        Sys.sleep(30)
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

# ********************************************************
# Demonstration
# rate_limit(token, query = "get_followers")
# ********************************************************


u <- "ClaudiaLopez"
out <- get_edge_list(u)

file_name <- paste0(outfolder, u, ".rds")
write_rds(out, file_name, compress = "gz")

