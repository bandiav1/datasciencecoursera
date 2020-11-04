corr = function(directory, threshold = 0) {
    files = list.files(paste('C:/Users/bandiav1/Desktop/datasciencecoursera/', directory, sep = ''), pattern = '.csv', full.names = T)
    cr1 = c()
    for (i in 1:length(files)) {
      temp = read.csv(files[i])[, c('nitrate', 'sulfate')]
      if (sum(complete.cases(temp)) > threshold) {
        temp1 = temp[complete.cases(temp),]
        cr1 = c(cr1, cor(temp1)[1, 2])
      }
    }
    return(cr1)
}
