Outcome = read.csv("outcome-of-care-measures.csv")
Outcome[,11] = as.numeric(Outcome[, 11])
Outcome[,17] = as.numeric(Outcome[, 17])
Outcome[,23] = as.numeric(Outcome[, 23])

best = function(state, outcome) {
  if(! state %in% Outcome$State) stop('invalid state')
  
  if (outcome == 'heart failure') {
    Outcome1 <<- Outcome[Outcome$State == state, c(2, 17)]
  } else if (outcome == 'heart attack') {
      Outcome1 <<- Outcome[Outcome$State == state, c(2, 11)]
  } else if (outcome == 'pneumonia') {
      Outcome1 <<- Outcome[Outcome$State == state, c(2, 23)]
    } else stop('invalid outcome')
  
  Outcome1[which(min(Outcome1[, 2], na.rm = T) == Outcome1[, 2]), 1]
}


