library(data.tree)
library(tidyverse)
library(rlang)



encode_outcome <- function(outcome) {
  map_chr(
    outcome,
    function(x) {
      switch (x,
        "green" = "o",
        "yellow" = ".",
        "grey" = "x"
      )
    }
    ) %>% paste(collapse = "")
}
# encode_outcome(c("green", "yellow", "yellow", "grey", "grey"))

decode_outcome <- function(outcome) {
  str_split(outcome, "")[[1]] %>% 
    map_chr(
      function(x) {
        switch (x,
          "o" = "green",
          "." = "yellow",
          "x" = "grey"
        )
      }
    ) 
}

# decode_outcome("o..xx")

get_filters_from_outcome <- function(candidate_word, outcome, encoded=FALSE) {
  if (encoded){
    outcome <- decode_outcome(outcome)
  }
  
  word_letters <- unique(str_split(candidate_word, "")[[1]])
  filters <- character(5 + length(word_letters))
  
  letters_counter <- word_letters %>% 
    map(function(x) c(grey = 0, not_grey = 0)) %>% 
    set_names(nm = word_letters)
  
  # First we find the positional filters
    
  for (n in c(1, 2, 3, 4, 5)){
    current_letter <- substr(candidate_word, n, n)
    current_outcome <- outcome[n]
    if (current_outcome == "green"){
      filters[n] <- paste(current_letter, "position_is", n, sep=".")
    } else{
      filters[n] <- paste(current_letter, "position_not_is", n, sep=".")
    }
    if (current_outcome == "grey"){
      letters_counter[[current_letter]]["grey"] <- 1 + letters_counter[[current_letter]][["grey"]]
    } else {
      letters_counter[[current_letter]]["not_grey"] <- 1 + letters_counter[[current_letter]][["not_grey"]]
    }
  }
  # Now we find the container filters
  filters_index <- 5
  for (letter in names(letters_counter)){
    filters_index <- filters_index + 1
    counts <- letters_counter[[letter]]
    if (counts["grey"] == 0){
      # All counts are green or yellow. 
      filters[filters_index] <- paste(letter, "contains_at_least", counts["not_grey"], sep=".")
    } else{
      # There are grey counts
      filters[filters_index] <- paste(letter, "contains_exact", counts["not_grey"], sep=".")
    }
  }
  filters
}

# get_filters_from_outcome( "abaco", c("green", "yellow", "yellow", "grey", "grey") )
# get_filters_from_outcome( "abaco", encode_outcome(c("green", "yellow", "yellow", "grey", "grey")), encoded=TRUE)

filter_words <- function(words_df, filters) {
  logical_index <- Reduce(f="&", x=select(words_df, filters), accumulate=FALSE)
  words_df[logical_index,]
}

# filter_words(words_df, get_filters_from_outcome("toesa", c("grey", "green", "grey", "grey", "green")))

simulate_outcome <- function(candidate_word, real_word, encoded=FALSE) {
  outcome <- rep("", length.out=5)
  
  splits <- str_split(c(candidate_word, real_word), "")
  candidate_word_letters <- unique(splits[[1]])
  comparison <- splits[[1]] == splits[[2]]
  
  green_counter <- rep_along(candidate_word_letters, 0) %>% 
    set_names(nm = candidate_word_letters)
  
  for (n in c(1, 2, 3, 4, 5)){
    candidate_letter <- splits[[1]][n]
    if (comparison[n]){
      outcome[n] <- "green"
      green_counter[candidate_letter] <- green_counter[candidate_letter] + 1
    } else {
        outcome[n] <- "grey"
    }
  }
  
  yellow_counter <- candidate_word_letters %>% 
    map(function(x) str_count(real_word, x) - green_counter[x]) %>% 
    set_names(nm = candidate_word_letters)
  
  for (n in c(1, 2, 3, 4, 5)){
    candidate_letter <- splits[[1]][n]
    if (outcome[n] == "grey"){
      if (yellow_counter[[candidate_letter]] > 0){
        outcome[n] <- "yellow"
        yellow_counter[[candidate_letter]] <- yellow_counter[[candidate_letter]] -1 
      }
    }
  }
  if (encoded){
    return(encode_outcome(outcome))
  }
  outcome
}

# simulate_outcome("aabbb", "bbaaa", TRUE)
# simulate_outcome("aaaab", "bbaaa")

get_all_encoded_outcomes <- function() {
  n = 1
  all_outcomes <- character(3**5)
  for (a in c("green", "yellow", "grey")){
    for (b in c("green", "yellow", "grey")){
      for (c in c("green", "yellow", "grey")){
        for (d in c("green", "yellow", "grey")){
          for (e in c("green", "yellow", "grey")){
            all_outcomes[n] <- encode_outcome(c(a, b, c, d, e))
            n <- n+1
          }
        }
      }
    }
  }
  all_outcomes
}
  
ALL_OUTCOMES <- get_all_encoded_outcomes()

init_possibilities_tibble <- function() {
  tibble(
    outcome = ALL_OUTCOMES,
    prob = rep_along(ALL_OUTCOMES, 0),
    entropy = rep_along(ALL_OUTCOMES, 0),
    expected_return = rep_along(ALL_OUTCOMES, 0),
    filtered_words = list_along(ALL_OUTCOMES)
  )
}

uniform_entropy <- function(n){
  # Entropy of a uniform distribution of n outcomes
  return(-log(1/n, base=2))
}

get_possibilities <- function(words_df, candidate_word) {
  possibilities <- init_possibilities_tibble()
  num_prev_words <- dim(words_df)[1]
  if (num_prev_words < dim(possibilities)[1]) {
    # It's more efficient to loop over words
    for (word in words_df[["word"]]){
      outcome <- simulate_outcome(candidate_word, word, encoded=TRUE)
      if (possibilities[possibilities[["outcome"]] == outcome, "prob"] == 0){
        # Otherwise, it has been computed already
        filters <- get_filters_from_outcome(candidate_word, outcome, encoded=TRUE)
        filtered_words <- filter_words(words_df, filters)
        prob <- dim(filtered_words)[1]
        entropy <- uniform_entropy(prob)
        possibilities[possibilities[["outcome"]] == outcome, "prob"] <- prob / num_prev_words
        possibilities[possibilities[["outcome"]] == outcome, "entropy"] <- entropy
        possibilities[["filtered_words"]][possibilities[["outcome"]] == outcome][[1]] <- filtered_words
      }
    }
  } else {
    for (outcome in possibilities[["outcome"]]){
      filters <- get_filters_from_outcome(candidate_word, outcome, encoded=TRUE)
      filtered_words <- filter_words(words_df, filters)
      prob <- dim(filtered_words)[1]
      entropy <- uniform_entropy(prob)
      if (prob > 0){
        possibilities[possibilities[["outcome"]] == outcome, "prob"] <- prob / num_prev_words
        possibilities[possibilities[["outcome"]] == outcome, "entropy"] <- entropy
        possibilities[["filtered_words"]][possibilities[["outcome"]] == outcome][[1]] <- filtered_words
      }
    }
  }
  filter(
    possibilities,
    prob > 0
  )
}

find_candidates <- function(all_words, filtered_words, initial_candidate){
  if (!is_null(initial_candidate)){
    return(initial_candidate)
  }
  if(dim(filtered_words)[1] < 4){
    return(filtered_words[["word"]])
  } else {
    # A reasonable heuristic is to pick only words that contain at least two letters present in filtered_words
    letters <- unique(Reduce(f = c, x = str_split(filtered_words[["word"]], ""), accumulate = FALSE))
    filters <- map_chr(
      letters, 
      function(x) paste(x, "contains_at_least", 1, sep=".")
    )
    filter_pairs <- combn(filters, 2)
    candidate_words <- c(filtered_words[["word"]], rep("", dim(all_words)[1]))
    for (pair_index in 1:ncol(filter_pairs)){
      filters <- filter_pairs[,pair_index]
      candidate_words <- unique(c(candidate_words, filter_words(all_words, filters)[["word"]]))
    }
    candidate_words %>% .[.!=""]
  }
}


build_leaf_tree <- function(words, possibility, relative_prob){
  node <- Node$new(possibility)
  node$move <- words[1]
  node$relative_prob <- relative_prob
  node$entropy <- 0
  node$expected_moves <- 1
  node
}

build_current_tree <- function(choice, expected_return, entropy, relative_prob, possibility, all_subtrees){
  node <- Node$new(possibility)
  node$move <- choice
  node$relative_prob <- relative_prob
  node$entropy <- entropy
  node$expected_moves <- expected_return
  for (child in all_subtrees){
    node$AddChildNode(child)
  }
  node
}


compute_subtree <- function(
    all_words, 
    filtered_words, 
    explore_top = 25, 
    level=1,
    current_possibility="start",
    initial_candidate=NULL,
    relative_prob=1.0) {
  # Instead of creating a lot of tibbles inside 
  if (dim(filtered_words)[1] == 1){
    # End of recursion here
    return(list(
      choice = filtered_words[["word"]][1],
      expected_return = 1,
      tree = build_leaf_tree(filtered_words[["word"]], current_possibility, relative_prob)
    ))
  }
  candidate_words <- find_candidates(all_words, filtered_words, initial_candidate)
  candidate_possibilities <- list_along(candidate_words)
  names(candidate_possibilities) <- candidate_words
  
  if (length(candidate_words) > explore_top){
    # In this case we filter by computing entropies first
    expected_entropies <- rep_named(candidate_words, 0) 
    for (word in candidate_words){
      possibilities <- get_possibilities(filtered_words, word)
      candidate_possibilities[[word]] <- possibilities
      expected_entropies[word] <- possibilities %>% 
        mutate(weighted_entropy = prob * entropy) %>% 
        select(weighted_entropy) %>% 
        sum
      if (word %in% filtered_words[["word"]]){
        # This is to avoid ties between words in the filtered list or not
        expected_entropies[word] <- expected_entropies[word] - 0.000001
      }
    }
    top_words <- head(sort(expected_entropies), explore_top)
    # We exclude words that have 0.5 more entropy than the best one 
    top_words <- top_words %>% .[.< 0.51 + top_words[1]]
    candidate_words <- names(top_words)
  }
  expected_returns <- rep_named(candidate_words, 0)
  all_subtrees <- list_along(candidate_words)
  names(all_subtrees) <- candidate_words
  for (word in candidate_words){
    all_subtrees[[word]] <- list()
    if (level == 1){
      print(paste("Exploring word... ", word))
      print(paste("Out of ", length(candidate_words), " words"))
    }
    if (!is_null(candidate_possibilities[[word]])){
      possibilities <- candidate_possibilities[[word]]
    } else {
      possibilities <- get_possibilities(filtered_words, word)
    }
    for (possibility in 1:nrow(possibilities)){
      output_subtree <- compute_subtree(
        all_words, filtered_words = possibilities[possibility, "filtered_words"][[1]][[1]], 
        explore_top = explore_top, 
        level = level + 1,
        current_possibility = possibilities[possibility, "outcome"][[1]],
        initial_candidate = NULL,
        relative_prob = possibilities[possibility, "prob"][[1]])
      expected_returns[word] <- expected_returns[word] + output_subtree$expected_return * possibilities[possibility, "prob"][[1]]
      all_subtrees[[word]][[possibilities[possibility, "outcome"][[1]]]] <- output_subtree$tree
    }
  }
  best <- min(expected_returns)
  best_idx <- which.min(expected_returns)
  choice <- candidate_words[best_idx]
  expected_return <- 1 + best
  entropy <- uniform_entropy(length(filtered_words[["word"]]))
  current_tree <- build_current_tree(choice, expected_return, entropy, relative_prob, current_possibility, all_subtrees[[choice]])
  return(
    list(
      choice = choice,
      expected_return = expected_return,
      tree = current_tree
    )
  )
}

compute_tree <- function(
    all_words, 
    explore_top = 25, 
    initial_candidate = "aireo",
    limit_possibilities = 243) {
  
  node <- Node$new("start")
  node$move <- initial_candidate
  node$relative_prob <- 1.0
  node$entropy <- uniform_entropy(length(all_words[["word"]]))
  expected_return <- 0
  possibilities <- get_possibilities(all_words, initial_candidate)
  for (possibility in 1:min(nrow(possibilities), limit_possibilities)){
    print(paste("Exploring possibility... ", possibility))
    print(paste("Out of ", nrow(possibilities), " possibilities"))
    print(paste("Outcome is", possibilities[possibility, "outcome"][[1]]))
    output_subtree <- compute_subtree(
      all_words, 
      filtered_words = possibilities[possibility, "filtered_words"][[1]][[1]], 
      explore_top = explore_top, 
      level = 1,
      current_possibility = possibilities[possibility, "outcome"][[1]],
      initial_candidate = NULL,
      relative_prob = possibilities[possibility, "prob"][[1]])
    expected_return <- expected_return + output_subtree$expected_return * possibilities[possibility, "prob"][[1]]
    node$AddChildNode(output_subtree$tree)
  }
  node$expected_moves <- expected_return
  node
}

# Make an optimizer with entropy (for levels 1 and 2) and a full optimizer for expected return for next levels

# compute_subtree(words_df, filtered_words)
