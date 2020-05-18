s1 = sample(1:6, 40, replace = TRUE)
s2 = sample(1:6, 70, replace = TRUE)
s3 = sample(1:6, 100, replace = TRUE)

probFreq = table(s1)
probFreq

### Misselenious
quantile(probFreq)  # looking onto the quartiles