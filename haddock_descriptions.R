# Import libraries
require(RJSONIO)
require("rwebppl")

# Working Directory
setwd("~/Desktop/is")

## Visuals Master ##
refs_json <- '[
  {"Animal": "rabbit", "Container": "bag", "Size": 1}
  , {"Animal": "rabbit", "Container": "bag", "Size": 2}
  , {"Animal": "frog", "Container": "bag", "Size": 3}
  , {"Animal": "frog", "Container": "box", "Size": 1}
  , {"Animal": "rabbit", "Container": "box", "Size": 2}
  , {"Animal": "frog", "Container": "basket", "Size": 3}
  , {"Animal": "rabbit", "Container": "box", "Size": 1}
  ]'

refs <- fromJSON(refs_json)
refs <- do.call("rbind", refs)
refs <- data.frame(refs)

## Visual Logical Constructor ##
conds_idx <- list(
  c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  , c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  , c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)
  )

get_visuals <- function(refs, conds_idx, i){
  
  cond_idx <- unlist(conds_idx[i])
  visual <- refs[cond_idx, ]
  
  return(visual)
}

## Descriptions ##
sizes <- c('smaller', 'small', 'big', 'bigger', 'empty')

get_descriptions <- function(visual, sizes) {
  
  visual_unique <- unique(visual[, c("Animal", "Container")])
  
  descs <- list()
  for (ref in 1:nrow(visual_unique)) {
    for (size in sizes) {
      animal <- visual_unique[ref, "Animal"]
      container <- visual_unique[ref, "Container"]
      desc = list(animal, size, container)
      descs <- list.append(descs, unlist(desc))
    }
  }
  desc <- list("none", "none", "none")
  descs <- list.append(descs, unlist(desc))
  
  return(descs)
}

get_costs <- function(descs) {

  costs <- list()
  columns <- list()
  
  for (i in 1:length(descs)) {
  
    desc <- unlist(descs[i])
    desc_str <- paste(desc[1], desc[2], desc[3])
    adjective <- desc[2]
    tail <- substr(adjective, nchar(adjective) - 1, nchar(adjective))
    
    if (tail == "er") {
    
      cost <- 1.5
    
    } else if (adjective %in% c("small", "big")) {
    
      cost <- 1
    
    } else if (adjective == "empty") {
    
      cost <- 0.5
    
    } else {
    
      cost <- 0
    
    }
    costs <- list.append(costs, cost)
    columns <- list.append(columns, desc_str)
  }
  costs <- data.frame(costs)
  colnames(costs) <- unlist(columns)
  
  return(costs)
}

execute_model <- function(truncatedDescription, cond, model) {
  
  # Model
  visuals <- get_visuals(refs, conds_idx, cond)
  descs <- get_descriptions(visuals, sizes)
  costs <- get_costs(descs)
  model_data <- list(truncatedDescription, visuals, descs, costs)
  
  model <- webppl(program_file=model, data = model_data, data_var = "model_data")

  return(model)
}

## Main ##

# Calling Parameters 
conds <- c(1, 2, 3)
truncatedDescriptions <- list(
  c("rabbit", "big", "silence")
  , c("rabbit", "bigger", "silence")
)
models <- c("helena.wppl")

# Execute Model
results <- data.frame()
for (truncatedDescription in truncatedDescriptions) {
  for (cond in conds) {
    for (model in models) {
      result <- execute_model(truncatedDescription, cond, model)
      result$Adjective <- truncatedDescription[2]
      result$Condition <- cond
      result$Model <- model
      results <- rbind(results, result)
    }
  }
}

colnames(results) <- c(
  "Animal"
  , "Container"
  , "Size"
  , "Probability"
  , "Adjective"
  , "Condition"
  , "Model")
print()