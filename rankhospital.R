Outcome = read.csv("outcome-of-care-measures.csv")
Outcome[,11] = as.numeric(Outcome[, 11])
Outcome[,17] = as.numeric(Outcome[, 17])
Outcome[,23] = as.numeric(Outcome[, 23])

rankhospital = function(state, outcome, num = 'best') {
  if(! state %in% Outcome$State) stop('invalid state')
  
  if (outcome == 'heart failure') {
    Outcome1 <<- Outcome[Outcome$State == state, c(2, 17)]
  } else if (outcome == 'heart attack') {
    Outcome1 <<- Outcome[Outcome$State == state, c(2, 11)]
  } else if (outcome == 'pneumonia') {
    Outcome1 <<- Outcome[Outcome$State == state, c(2, 23)]
  } else stop('invalid outcome')
  
  Outcome1 <<- Outcome1[is.na(Outcome1[,2]) == 0,]
  if (num == 'best') {
    Outcome1[order(Outcome1[,2], Outcome1[,1]),][1,1]
  } else if (num == 'worst'){
    Outcome1[order(-Outcome1[,2], Outcome1[,1]),][1,1]
  } else {
    Outcome1[order(Outcome1[,2], Outcome1[,1]),][num,1]
  }
}


