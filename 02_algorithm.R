
library(dplyr)
# source("00_data_preparation.R")
# source("01_load_data.R")


# RANKING ALGORITHM ----------

ranking_algo = function(answers, data_wide) {
  
  # if no categories are selected, nothing is shown
  if (all(answers == 0)) {
    return(rep(-1, nrow(data_wide)))
  } else {
    
    # initialize
    data_mat = as.matrix(data_wide)
    total = data_mat[,"total"]
    cats = names(answers)[answers == 1]
    
    # consistency measures
    if ("refugee" %in% cats) cats = union(cats, "migrant")
    if ("asylum" %in% cats) cats = union(cats, "refugee")
    if ("resettlement and humanitarian programs" %in% cats) cats = union(cats, c("refugee", "asylum"))
    if ("eldercare" %in% cats) cats = union(cats, "senior")
    if (any(c("FGM/C", "forced marriage", "domestic violence", "sexual violence") %in% cats))
      cats = union(cats, "violence and discrimination")
    if (any(c("trans", "intersex") %in% cats)) cats = union(cats, "LGBTQ+")
    
    # count all matching categories
    score = if (length(cats) > 1) 2*rowSums(data_wide[,cats]) else 2*data_wide[,cats]
    diff = total - score
    score = score - diff + max(diff)
    # prefer places with less specification
    score = score + 1/total
    # make the numbers prettier
    score = rank(score, ties="random")
    
    # SPECIAL CASES
    # hide women if not ticked
    if (!any(c("women", "LGBTQ+") %in% cats)) {
      score[data_mat[,"women"] == 1] = -1
    }
    # hide LGBTQ+ if not ticked
    if (!("LGBTQ+" %in% cats)) {
      score[data_mat[,"LGBTQ+"] == 1] = -1
    }
    # hide violence and discrimination if not ticked
    if (!("violence and discrimination" %in% cats)) {
      score[data_mat[,"violence and discrimination"] == 1] = -1
    }
    # hide sex work if not ticked
    if (!("sex work" %in% cats)) {
      score[data_mat[,"sex work"] == 1] = -1
    }
    # hide resettle program if not ticked
    if (!("resettlement and humanitarian programs" %in% cats)) {
      score[data_mat[,"resettlement and humanitarian programs"] == 1] = -1
    }
    # hide other age groups
    age_groups = c("children", "teenagers (12-17)", "young adults (18-26)",
                   "adult (27+)", "senior")
    if (any(age_groups %in% cats)) {
      # other age groups apply
      s = setdiff(age_groups, cats)
      s = if (length(s) > 1) (rowSums(data_mat[,s]) > 0) else as.logical(data_mat[,s])
      # the selected age group applies
      i = intersect(age_groups, cats)
      i = if (length(i) > 1) (rowSums(data_mat[,i]) > 0) else as.logical(data_mat[,i])
      score[s & !i] = -1
    }
    # set without ID documents on top
    if ("without ID documents" %in% cats) {
      max_val = max(score)
      score[data_mat[,"without ID documents"] == 1] = score+max_val
    }
    # set pregnant or infant on top
    if ("pregnancy and infant care" %in% cats) {
      max_val = max(score)
      score[data_mat[,"pregnancy and infant care"] == 1] = max_val + score[data_mat[,"pregnancy and infant care"] == 1]
    }
    # set resettlement programs on top
    if ("resettlement and humanitarian programs" %in% cats) {
      max_val = max(score)
      score[data_mat[,"resettlement and humanitarian programs"] == 1] = score+max_val
    }
    
    # remove all that have none of the cats
    a = if (length(cats) > 1) rowSums(data_mat[, cats]) == 0 else data_mat[, cats] == 0
    # in case there are only 10 or less places selected,
    # add the places that offer solely "social aid" or "legal advice"
    if (sum(!a) <= 10) {
      b = (total <= 1 & (data_mat[,"social aid"] == 1 | data_mat[,"legal advice"] == 1)) 
    } else b = FALSE
    score[a & !b] = -1
    
    score = (1:nrow(data_wide))[score > 0][order(score[score > 0], decreasing=T)]
    
    return(score)
  }
}

