library(ggplot2)

# random semple
s1 = sample(1:6, 40, replace = TRUE)
s2 = sample(1:6, 70, replace = TRUE)
s3 = sample(1:6, 100, replace = TRUE)

probFreq = table(s1)

probFreqDF  = as.data.frame(probFreq)
colnames(probFreqDF) = c("Faces", "Frequency")

rel = rank(table(s1))/length(table(s1))

### Plotting
plot(s1, main = "SCATTER PLOT OF SAMPLE", xlab = "Index", ylab = "Faces of dice")
barplot(probFreq, main = "BAR PLOT OF SAMPLE", xlab = "Faces of dice", ylab = "Frequency", border = "dark blue", col = gray(1-rel))

points = ggplot(probFreqDF, aes(x=Faces, y = Frequency)) +
            geom_point(size = 2)

############################################
Faces = factor(NULL, levels = probFreqDF$Faces)
Freq = as.numeric(NULL)

for(i in 1:nrow(probFreqDF)) {
  faces = as.numeric(probFreqDF$Faces[i])
  
  val = rep(faces, as.numeric(probFreqDF$Frequency[i]))
  Faces = c(Faces, val)
  Freq = c(Freq, rep(1, as.numeric(probFreqDF$Frequency[i])))
}
temp = data.frame(Faces = Faces, Freq = Freq)

#  histogram
hist = ggplot(temp, aes(x=Faces)) +
          geom_histogram(binwidth = 0.5)

#################

points
hist


