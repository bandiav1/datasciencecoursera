library(data.table)

Outcome = read.csv("outcome-of-care-measures.csv")
Outcome[,11] = as.numeric(Outcome[, 11])
Outcome[,17] = as.numeric(Outcome[, 17])
Outcome[,23] = as.numeric(Outcome[, 23])

rankall = function(outcome, num = 'best') {
  if (outcome == 'heart failure') {
    Outcome1 <<- Outcome[, c(7, 2, 17)]
  } else if (outcome == 'heart attack') {
    Outcome1 <<- Outcome[, c(7, 2, 11)]
  } else if (outcome == 'pneumonia') {
    Outcome1 <<- Outcome[, c(7, 2, 23)]
  } else stop('invalid outcome')
  
  Outcome1 <<- Outcome1[is.na(Outcome1[,3]) == 0,]
  Outcome1[, c('state', 'hospital', 'rate')] = Outcome1[, c(1,2,3)]
  Outcome2 <<- data.table(Outcome1[,-c(1,2,3)])
  if (num == 'best') {
    Outcome2 <<- Outcome2[order(state, rate, hospital)][, ':='(num1 = seq_along(hospital)), keyby = .(state)][, head(.SD,1), keyby = .(state)][, c(1,2)]
  } else if (num == 'worst') {
    Outcome2 <<- Outcome2[order(state, rate, hospital)][, ':='(num1 = seq_along(hospital)), keyby = .(state)][, tail(.SD,1), keyby = .(state)][, c(1,2)]
  } else {
    Outcome2 <<- Outcome2[order(state, rate, hospital)][, ':='(num1 = seq_along(hospital)), keyby = .(state)][num != num1, ':='(hospital = NA)][order(state, hospital), head(.SD,1), keyby = .(state)][, c(1,2)]
  }
}


