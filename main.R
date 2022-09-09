library(yaml)

osList <- yaml.load(yaml)
osNode <- as.Node(osList)
print(osNode, "users", "prob")

# Convert back to list of list
osListBack <- ToListExplicit(osNode)
as.yaml(osListBack)
write_yaml(as.yaml(osListBack), "./example2.yml")
read_yaml("./example.yml")
newTree <- FromListExplicit(yaml.load(read_yaml("./example.yml")))
yaml_str <- yaml.load_file("./example.yml")
yaml.load(yaml_str)
print(newTree, "relative_prob", "entropy", "move")

# Replicate example2 programmatically
root_tree <- Node$new("aireo_wordle_tree")
root_tree$A
starting_node <- root_tree$AddChild("start")
starting_node$relative_prob <- 1.0
starting_node$entropy <- 4.5
starting_node$move <- "aireo"
child1 <- starting_node$AddChild("ooxo")
child1$relative_prob <- 0.5
child1$entropy <- 2.5
child1$move <- "fatal"
child2 <- starting_node$AddChild(".oxo")
child2$relative_prob <- 1.0
child2$entropy <- 4.5
child2$move <- "fiesta"
child3 <- child1$AddChild("oxxo")
child3$relative_prob <- 1.0
child3$entropy <- 4.5
child3$move <- "stroke"
child4 <- child2$AddChild("oxxo")
child4$relative_prob <- 0.3
child4$entropy <- 2.5
child4$move <- "fatal"

osListBack <- ToListExplicit(root_tree)
write_yaml(as.yaml(osListBack), "./example3.yml")


first.choice <- find.compatible.words(
  wordle.data,
  list(
    "a.contains_exact.1",
    "e.contains_exact.1",
    "b.contains_exact.0",
    "f.contains_exact.0",
    "g.contains_exact.0",
    "h.contains_exact.0",
    "j.contains_exact.0",
    "k.contains_exact.0",
    "ñ.contains_exact.0",
    "q.contains_exact.0",
    "v.contains_exact.0",
    "w.contains_exact.0",
    "x.contains_exact.0",
    "y.contains_exact.0",
    "z.contains_exact.0"
  )
)
first.choice <- first.choice %>% 
  add_row(word = "aireo") %>% 
  add_row(word = "pulir") %>% 
  add_row(word = "toesa") %>% 
  add_row(word = "salen") %>% 
  add_row(word = "secan") %>% 
  add_row(word = "nacer") %>% 
  add_row(word = "cesar") 



compute.entropy <- function(current.words, candidates){
  # Need to pass prev filters
  entropies <- rep_along(candidates[["word"]], 0)
  num.words <- dim(current.words)[1]
  n <- 0
  for (word in candidates[["word"]]){
    n <- n + 1
    print(paste("Computing", word))
    for (outcome in all.outcomes){
      filters <- find.filters(word, outcome)
      compatible.words <- dim(find.compatible.words(current.words, filters))[1]
      if (compatible.words > 0){
        prob <- compatible.words / num.words
        weighted.entropy <- prob * uniform.entropy(compatible.words)
        entropies[n] <- entropies[n] + weighted.entropy
      }
    }
  }
  candidates["entropy"] <- entropies
  return(candidates)
}

first.choice <- compute.entropy(wordle.data, first.choice)
top.choices <- top_n(first.choice, -10, entropy)

# Next steps: Pick top 10 and, for each one, simulate all possible outcomes. 
# In second step we also optimize for entropy. In the rest, we start optimizing num_rounds
compute.subtree <- function(current.words, word, outcome=NULL){
  # Simulate outcome if words is less than 243
  if (is_null(outcome)){
    print("Yes")
  } else{
    print(outcome)
  }
  possible.outcomes <- list()
  n <- 1
  for (outcome in all.outcomes){
    filters <- find.filters(word, outcome)
    compatible.words <- dim(find.compatible.words(current.words, filters))[1]
    if (compatible.words > 0){
      possible.outcomes[[n]] <- outcome
      n <- n + 1
    }
  }
  return(possible.outcomes)
}
compute.subtree(wordle.data, "aireo")
compute.subtree("a", "b")
