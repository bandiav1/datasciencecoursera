pollutantmean = function(directory, pollutant, id = 1:332) {
    files = list.files(paste('C:/Users/bandiav1/Desktop/datasciencecoursera/', directory, sep = ''), pattern = '.csv', full.names = T)
    temp1 = c()
    for (i in id) {
      temp = read.csv(files[i])
      temp1 = c(temp1, as.vector(temp[, c(pollutant)]))
    }
    mean(temp1, na.rm = T)
  
}
