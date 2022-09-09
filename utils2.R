library(data.tree)
library(base)
library(tidyverse)
library(rlang)

LEN_WORDS <- 5

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
  if (encoded) {
    outcome <- decode_outcome(outcome)
  }
  
  word_letters <- unique(str_split(candidate_word, "")[[1]])
  filters <- character(LEN_WORDS + length(word_letters))
  
  letters_counter <- word_letters %>% 
    map(function(x) c(grey = 0, not_grey = 0)) %>% 
    set_names(nm = word_letters)
  
  # First we find the positional filters
    
  for (n in 1:LEN_WORDS) {
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
  filters_index <- LEN_WORDS
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

filter_words <- function(words_df, current_indexes=TRUE, filters) {
  logical_index <- Reduce(f="&", x=select(words_df, filters), accumulate=FALSE)
  logical_index & current_indexes
}

# filter_words(words_df, get_filters_from_outcome("toesa", c("grey", "green", "grey", "grey", "green")))

simulate_outcome <- function(candidate_word, real_word, encoded=FALSE) {
  outcome <- rep("", length.out=LEN_WORDS)
  
  splits <- str_split(c(candidate_word, real_word), "")
  candidate_word_letters <- unique(splits[[1]])
  comparison <- splits[[1]] == splits[[2]]
  
  green_counter <- rep_along(candidate_word_letters, 0) %>% 
    set_names(nm = candidate_word_letters)
  
  for (n in 1:LEN_WORDS){
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
  
  for (n in 1:LEN_WORDS){
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
  all_outcomes <- expand.grid(rep(list(c("green", "yellow", "grey")), LEN_WORDS))
  apply(all_outcomes, 1, encode_outcome)
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

entropy <- function(dist) {
  # Entropy of a probability distribution of n outcomes
  dist <- dist / sum(dist)
  sum(-log(dist, base = 2) * dist)
}


get_possibilities <- function(all_words, current_indexes=TRUE, candidate_word) {
  possibilities <- init_possibilities_tibble()
  num_prev_words <- sum(current_indexes)
  prob_factor <- sum(all_words[current_indexes,][["prob"]])
  if (num_prev_words < dim(possibilities)[1]) {
    # It's more efficient to loop over words
    for (word in all_words[current_indexes,][["word"]]){
      outcome <- simulate_outcome(candidate_word, word, encoded=TRUE)
      if (possibilities[possibilities[["outcome"]] == outcome, "prob"] == 0){
        # Otherwise, it has been computed already
        filters <- get_filters_from_outcome(candidate_word, outcome, encoded=TRUE)
        filtered_indexes <- filter_words(all_words, current_indexes, filters)
        prob <- sum(filtered_indexes) / prob_factor
        entropy <- entropy(all_words[filtered_indexes,][["prob"]])
        possibilities[possibilities[["outcome"]] == outcome, "prob"] <- prob
        possibilities[possibilities[["outcome"]] == outcome, "entropy"] <- entropy
        possibilities[["filtered_words"]][possibilities[["outcome"]] == outcome][[1]] <- filtered_indexes
      }
    }
  } else {
    for (outcome in possibilities[["outcome"]]){
      filters <- get_filters_from_outcome(candidate_word, outcome, encoded=TRUE)
      filtered_indexes <- filter_words(all_words, current_indexes, filters)
      prob <- sum(filtered_indexes) / prob_factor
      entropy <- entropy(all_words[filtered_indexes,][["prob"]])
      if (prob > 0){
        possibilities[possibilities[["outcome"]] == outcome, "prob"] <- prob
        possibilities[possibilities[["outcome"]] == outcome, "entropy"] <- entropy
        possibilities[["filtered_words"]][possibilities[["outcome"]] == outcome][[1]] <- filtered_indexes
      }
    }
  }
  filter(
    possibilities,
    prob > 0
  )
}

find_candidates <- function(all_words, current_indexes=TRUE, initial_candidate){
  if (!is_null(initial_candidate)){
    return(initial_candidate)
  }
  if(sum(current_indexes) < 4 && length(current_indexes) > 1){
    # In this case we don't need to look elsewhere the current indexes
    return(all_words[current_indexes, ][["word"]])
  } else {
    # It's mandatory that words contain at least one letter present in the list of words
    letters <- unique(Reduce(f = c, x = str_split(all_words[current_indexes,][["word"]], ""), accumulate = FALSE))
    filters <- map_chr(
      letters, 
      function(x) paste(x, "contains_at_least", 1, sep=".")
    )
    candidate_words <- c(all_words[current_indexes,][["word"]], rep("", dim(all_words)[1]))
    for (filter in filters){
      filtered_indexes <- filter_words(all_words, current_indexes, filter)
      new_words <- all_words[filtered_indexes,][["word"]]
      candidate_words <- unique(c(candidate_words, new_words))
    }
    candidate_words %>% .[.!=""]
  }
}


build_leaf_tree <- function(word, possibility, relative_prob, expected_return){
  node <- Node$new(possibility)
  node$move <- word
  node$relative_prob <- relative_prob
  node$entropy <- 0
  node$expected_moves <- expected_return
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

encode_possibilities <- function(current_indexes, possibilities) {
  lapply(
    possibilities[["filtered_words"]], 
    function(x) strtoi(paste(as.character(as.numeric(x[current_indexes])), collapse = ""), base=2)) %>% 
    unlist %>% sort
}


compute_subtree <- function(
    all_words, 
    current_indexes = TRUE, 
    explore_top = 25, 
    level=1,
    current_possibility="start",
    parent_word="",
    initial_candidate=NULL,
    relative_prob=1.0,
    debug=FALSE) {
  # Instead of creating a lot of tibbles inside 
  if (debug) {
    print(paste(level, parent_word, current_possibility))
  }
  if (sum(current_indexes) == 1 && length(current_indexes) > 1){
    # End of recursion here
    choice <- all_words[current_indexes, 1][["word"]][1]
    expected_return <- 1
    if (choice == parent_word){
      choice <- NULL
      expected_return <- 0
    }
    return(list(
      choice = choice,
      expected_return = expected_return,
      tree = build_leaf_tree(choice, current_possibility, relative_prob, expected_return)
    ))
  }
  candidate_words <- find_candidates(all_words, current_indexes, initial_candidate)
  candidate_possibilities <- list_along(candidate_words)
  names(candidate_possibilities) <- candidate_words
  entropies_computed <- FALSE
  if (length(candidate_words) > explore_top){
    entropies_computed <- TRUE
    # In this case we filter by computing entropies first
    expected_entropies <- rep_named(candidate_words, 0) 
    for (word in candidate_words){
      possibilities <- get_possibilities(all_words, current_indexes, word)
      candidate_possibilities[[word]] <- possibilities
      expected_entropies[word] <- possibilities %>% 
        mutate(weighted_entropy = prob * entropy) %>% 
        select(weighted_entropy) %>% 
        sum
      if (word %in% all_words[current_indexes, ][["word"]]){
        # This is to avoid ties between words in the filtered list or not
        expected_entropies[word] <- expected_entropies[word] - 0.000001
      }
    }
    top_words <- head(sort(expected_entropies), explore_top)
    # We exclude words that have 0.75 more entropy than the best one 
    top_words <- top_words %>% .[.< 0.75 + top_words[1]]
    candidate_words <- names(top_words)
  }
  expected_returns <- rep_named(candidate_words, 100)
  all_subtrees <- list_along(candidate_words)
  names(all_subtrees) <- candidate_words
  if (debug) {
    print("candidate words...")
    print(candidate_words)
    print("filtered words...")
    print(all_words[current_indexes, ][["word"]])
  }
  
  compare_possibilities <- sum(current_indexes) < 30
  if (compare_possibilities) {
    encoded_possibilities <- list_along(candidate_words)
    names(encoded_possibilities) <- candidate_words
  }
  for (word in candidate_words){
    all_subtrees[[word]] <- list()
    # We had init it with a higher number foreseen the possibility of break
    expected_returns[word] <- 0
    if (level == 1){
      print(paste("Exploring word... ", which(candidate_words == word)))
      print(paste("Out of ", length(candidate_words), " words"))
    }
    if (debug) {
      print(paste("exploring word " , word))
    }
    if (!is_null(candidate_possibilities[[word]])){
      possibilities <- candidate_possibilities[[word]]
    } else {
      possibilities <- get_possibilities(all_words, current_indexes, word)
    }
    if (compare_possibilities) {
      encoded_possibility <- encode_possibilities(current_indexes, possibilities)
      if (any(encoded_possibilities %in% list(encoded_possibility))) {
        if (debug) {
          print("Word excluded for possibility checked")
          print(encoded_possibility)
          print(encoded_possibilities)
        }
        next
      } else {
        encoded_possibilities[[word]] <- encoded_possibility
      }
    }
    
    # If the split created by a word (encoded in the possibilities) has been explored, remove it from computation
    # I need a way to encode a possibility using the different splits it generates.
    for (possibility in 1:nrow(possibilities)){
      output_subtree <- compute_subtree(
        all_words, 
        current_indexes = possibilities[possibility, "filtered_words"][[1]][[1]], 
        explore_top = explore_top, 
        level = level + 1,
        current_possibility = possibilities[possibility, "outcome"][[1]],
        parent_word = word,
        initial_candidate = NULL,
        relative_prob = possibilities[possibility, "prob"][[1]],
        debug = debug)
      expected_returns[word] <- expected_returns[word] + output_subtree$expected_return * possibilities[possibility, "prob"][[1]]
      all_subtrees[[word]][[possibilities[possibility, "outcome"][[1]]]] <- output_subtree$tree
    }
    if (expected_returns[word] < 1) {
      # For example, if there are n remaining words and there is one word that
      # both splits perfectly the data and is a valid guess, there is no better outcome
      break
    }
  }
  best <- min(expected_returns)
  best_idx <- which.min(expected_returns)
  if (entropies_computed) {
    print(paste("Top word selected is ", best_idx, "Out of", length(candidate_words)))
    print(paste("Entropies ", top_words))
    print(paste("Returns ", expected_returns))
  }
  choice <- candidate_words[best_idx]
  expected_return <- 1 + best
  entropy <- entropy(all_words[current_indexes,][["prob"]])
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
    explore_top = 20, 
    initial_candidate = "aireo",
    explore_only = NULL, 
    verbose=TRUE,
    debug=FALSE, 
    filename=NULL) {
  # TODO: Try with more words https://github.com/xavier-hernandez/react-wordle-spanish/blob/main/src/constants/wordlist.ts
  
  node <- Node$new("start")
  node$move <- initial_candidate
  node$relative_prob <- 1.0
  node$entropy <- entropy(all_words[["prob"]])
  expected_return <- 0
  possibilities <- get_possibilities(all_words, TRUE, initial_candidate)
  if (!is_null(explore_only)){
    range_possibilities <- explore_only
  } else {
    range_possibilities <- 1:nrow(possibilities)
  }
  for (possibility in range_possibilities) {
    print(paste("Exploring possibility... ", possibility))
    print(paste("Out of ", nrow(possibilities), " possibilities"))
    output_subtree <- compute_subtree(
      all_words, 
      current_indexes = possibilities[possibility, "filtered_words"][[1]][[1]], 
      explore_top = explore_top, 
      level = 2 - verbose,
      current_possibility = possibilities[possibility, "outcome"][[1]],
      initial_candidate = NULL,
      parent_word = initial_candidate,
      relative_prob = possibilities[possibility, "prob"][[1]],
      debug=debug)
    expected_return <- expected_return + output_subtree$expected_return * possibilities[possibility, "prob"][[1]]
    node$AddChildNode(output_subtree$tree)
    if (debug) {
      print(output_subtree$tree, "relative_prob", "expected_moves", "move", "entropy")
    }
  }
  node$expected_moves <- expected_return + 1
  if (!is_null(filename)) {
    nodelst <- ToListExplicit(node)
    write_yaml(as.yaml(nodelst), filename)
  }
  node
}

# Make an optimizer with entropy (for levels 1 and 2) and a full optimizer for expected return for next levels

# compute_subtree(words_df, filtered_words)
simulate_game <- function(tree, real_word) {
  
}

play_game <- function(tree) {
  while (!tree$isLeaf) {
    print("Expected win on ")
    print(tree$expected_moves)
    print("Move ")
    print(tree$move)
    result <- readline(prompt="Enter result: ")
    tree <- tree[[result]]
  }
}

compute_entropies <- function(all_words, candidate_words){
  entropies <- list_along(candidate_words)
  names(entropies) <- candidate_words
  for (word in candidate_words) {
      possibilities <- get_possibilities(all_words, TRUE, word)
      entropies[word] <- possibilities %>% 
        mutate(weighted_entropy = prob * entropy) %>% 
        select(weighted_entropy) %>% 
        sum
  }
  entropies
}
