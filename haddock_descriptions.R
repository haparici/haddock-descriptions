# Import libraries
require(ggm)
require(ggplot2)
require(rlist)
require(RJSONIO)
require("rwebppl")

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


##Random Variables##

pos<-list(c("rabbit", "big", "bag"),c("rabbit", "big", "box"))
cmp<-list(c("rabbit", "bigger", "bag"),c("rabbit", "bigger", "box"))

randomVariables<-list(pos,cmp)




execute_model <- function(randomVariable, cond, model,context) {
  
  # Model
  visuals <- get_visuals(refs, conds_idx, cond)
  descs <- get_descriptions(visuals, sizes)
  costs <- get_costs(descs)

  model_data <- list(randomVariable, visuals, descs, costs, context)
  
  model <- webppl(program_file=model, data = model_data, data_var = "model_data")
  
  return(model)
  
}

## Main ##

# Calling Parameters 
conds <- c(1, 2, 3)
contexts <- c("no-cc","cc")
models <- c("bumford_cc.wppl")

# Execute Model
results <- data.frame()


for (context in contexts) {
for (randomVariable in randomVariables) {
  for (cond in conds) {
    for (model in models) {
      result <- execute_model(randomVariable, cond, model,context)
      result$Adjective <- randomVariable[[1]][2]
      result$Context <- context
      result$Condition <- cond
      result$Model <- model
      results <- rbind(results, result)
      print(paste("Processing adjective"
                  , randomVariable[[1]][2]
                  , "for context"
                  , context
                  , "for condition"
                  , cond
                  , "with model"
                  , model))
    }
  }
}}

colnames(results) <- c(
  "Animal"
  , "Container"
  , "Size"
  , "Probability"
  , "Adjective"
  , "Context"
  , "Condition"
  , "Model")

results

## Plots ##

bags<-subset(results, Container=="bag")

cbPalette <- c("#009E73", "#CC79A7","#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00",  "#999999")

ggplot(bags, aes(x=Condition, y=Probability, fill=Adjective)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  theme(axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=15),  
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=14)
  )+
  xlab("Display Type") +
  ylab("Bag Resolution") +
  facet_grid(Context ~ Adjective) +
  labs(fill="Adj. Type")


