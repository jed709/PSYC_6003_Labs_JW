test

a = rnorm(100)

d=c()

for (i in a) {
  if (i<=0){
    d = c(d,i)
  }
}

d = c(1:5)
d = c(d,6)
append(d,7)

for (i in 0:-100) {
  print(i)
}

stroop_dat = NULL

for(i in 1:36) {
  c_dat = read.csv(file=sprintf('data/stroop/individual/stroop_s%.3d.csv',i))
  stroop_dat = rbind(stroop_dat, c_dat)
}