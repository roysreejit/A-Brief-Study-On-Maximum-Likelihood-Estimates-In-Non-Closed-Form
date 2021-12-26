rm(list=ls())


# Step One

n = seq(1000, 100000, 1000)
th = c(0.123, 1.234, 2.345, 3.456, 4.567, 5.678, 6.789, 7.89, 8.901, 9.012)

g = function(x, p) {
  s = 0
  for (i in 1:length(x)) {
    s = s + {
      (x[i] - p) / (1 + (x[i] - p) ^ 2)
    }
  }
  s
}

g1 = function(x, p) {
  s = 0
  for (i in 1:length(x)) {
    s = s + {
      ((x[i] - p) ^ 2 - 1) / (1 + (x[i] - p) ^ 2) ^ 2
    }
  }
  s
}

itf = function(th) {
  thhat = array(0)
  for (i in n) {
    x = rcauchy(i, th, 1)
    t = array(0)
    t[1] = floor(th)
    t[2] = t[1] - (g(x, t[1]) / g1(x, t[1]))
    while (abs(t[2] - t[1]) > 0.0000000000001) {
      t[1] = t[2]
      t[2] = t[2] - (g(x, t[2]) / g1(x, t[2]))
    }
    thhat[i / 1000] = t[2]
  }
  thhat
}

for (i in 1:length(th)) {
  plot(n, itf(th[i]), ylab = "??^")
  abline(h = th[i])
}


# Step Two

n = 100000
for (m in 1:length(th)) {
  y = rcauchy(n, th[m], 1)
  k = seq(1000, 10000, 1000)
  vbs = array(0)
  for (i in 1:length(k)) {
    thhat = array(0)
    for (j in 1:k[i]) {
      x = sample(y, length(y), T)
      t = array(0)
      t[1] = floor(th)
      t[2] = t[1] - (g(x, t[1]) / g1(x, t[1]))
      while (abs(t[2] - t[1]) > 0.0000000001) {
        t[1] = t[2]
        t[2] = t[2] - (g(x, t[2]) / g1(x, t[2]))
      }
      thhat[j] = t[2]
    }
    vbs[i] = var(thhat)
  }
  plot(k, vbs)
  abline(h = 2 / n)
}
