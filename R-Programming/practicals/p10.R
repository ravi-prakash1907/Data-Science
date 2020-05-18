age <- c(rep(20,5),rep(21,3),rep(22,4),rep(23,3))

mdn = median(age[age < 22])    # 1
mdnAll = median(age)           # 2
mean = mean(age)                      # 3
mode = names(table(age))[table(age)==max(table(age))]   # 4

## 5
age = c(age, rep(23, 2))

mean.new = mean(age)
mdnAll.new = median(age)
mode.new = names(table(age))[table(age)==max(table(age))]
