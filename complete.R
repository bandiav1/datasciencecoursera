complete = function(directory, id = 1:332) {
    files = list.files(paste('C:/Users/bandiav1/Desktop/datasciencecoursera/', directory, sep = ''), pattern = '.csv', full.names = T)
    temp1 <<- data.frame(id = numeric(), nobs = numeric())
    for (i in id) {
      temp = read.csv(files[i])[, c('nitrate', 'sulfate')]
      temp1 <<- rbind(temp1, data.frame(id = i, nobs = sum(complete.cases(temp))))
    }
    temp1
}
