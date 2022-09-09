library(yaml)
library(data.tree)
library(data.table)
source("words.R")
words_df <- get_words_df()
words_df[["word"]]
fwrite(list(words_df[["word"]]), file = "./wordle/data/words.csv")
source("utils2.R")
# aireo_tree <- compute_subtree(words_df, words_df, level=0, initial_candidate="aireo")
aireo_tree <- compute_tree(words_df, initial_candidate="aireo", verbose=TRUE, debug=FALSE)
aireo_tree_lst <- ToListExplicit(aireo_tree)
write_yaml(as.yaml(aireo_tree_lst), "./trees/aireo.yml")

toesa_tree <- compute_tree(words_df, initial_candidate="toesa", verbose=TRUE, debug=FALSE, filename="./trees/toesa.yml")
cesar_tree <- compute_tree(words_df, initial_candidate="cesar", verbose=TRUE, debug=FALSE, filename="./trees/cesar.yml")

aireo_tree <- FromListExplicit(yaml.load(read_yaml("./trees/aireo.yml")))
play_game(aireo_tree)
result <- readline(prompt="Enter result: ")
aireo_tree$xo.xo
aireo_tree[[result]]

print(aireo_tree, "relative_prob", "expected_moves", "move", "entropy")
filtered_words <- filter_words(words_df, get_filters_from_outcome("aireo", "xxx.x", encoded=TRUE))

outcome <- compute_subtree(words_df, filtered_words)
print(outcome$tree, "relative_prob", "expected_moves", "move", "entropy")
fetal <- get_possibilities(filtered_words, "fetal")

filtered_words <- filter_words(filtered_words, get_filters_from_outcome("falsa", "x.xxx", encoded=TRUE))

outcome <- compute_subtree(words_df, filtered_words)
print(outcome$tree, "relative_prob", "expected_moves", "move", "entropy")


filtered_words <- filter_words(words_df, get_filters_from_outcome("aireo", "o.xxx", encoded=TRUE))
words_df[["word"]] == "acida"

newO <- get_possibilities(words_df, TRUE, "aireo")

current_words <- words_df[filter_words(words_df,TRUE, get_filters_from_outcome("aireo", "xx..o", encoded=TRUE)),]["word"]
current_indexes <- filter_words(words_df,TRUE, get_filters_from_outcome("aireo", "xx..o", encoded=TRUE))
sum(current_indexes)




first_choice <- filter_words(
  words_df,
  TRUE,
  c(
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
first_choice <- words_df[first_choice, ][["word"]]
first_choice <- c(first_choice, "aireo",   "pulir" , "toesa" , "salen" , "secan" , "nacer" , "cesar")
entropies <- compute_entropies(words_df, first_choice)
entropies_df <- data.frame(
  word = names(entropies),
  entropy = unlist(entropies)
)
write_csv(entropies_df, "./data/entropies.csv")
