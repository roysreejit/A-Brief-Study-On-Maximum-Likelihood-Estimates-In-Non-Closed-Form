rm(list=ls())


# Step One

n = seq(1000, 100000, 1000)
th = c(1.234, 2.345, 3.456, 4.567, 5.678, 6.789, 7.89, 8.901, 9.012, 10.234)

g = function(x, p) {
  s = -digamma(p)
  for (i in 1:length(x)) {
    s = s + log(x[i]) / length(x)
  }
  s
}

itf = function(th) {
  thhat = array(0)
  for (i in n) {
    x = rgamma(i, th, 1)
    t = array(0)
    t[1] = floor(th)
    t[2] = t[1] + (g(x, t[1]) / trigamma(t[1]))
    while (abs(t[2] - t[1]) > 0.0000000000001) {
      t[1] = t[2]
      t[2] = t[2] + (g(x, t[2]) / trigamma(t[2]))
    }
    thhat[i / 1000] = t[2]
  }
  thhat
}

for (i in 1:length(th)) {
  plot(n, itf(th[i]), ylab = "a^")
  abline(h = th[i])
}



#Step Two

n = 100000
for (m in 1:length(th)) {
  y = rgamma(n, th[m], 1)
  k = seq(1000, 10000, 1000)
  vbs = array(0)
  for (i in 1:length(k)) {
    thhat = array(0)
    for (j in 1:k[i]) {
      x = sample(y, length(y), T)
      t = array(0)
      t[1] = floor(th)
      t[2] = t[1] + (g(x, t[1]) / trigamma(t[1]))
      while (abs(t[2] - t[1]) > 0.0000000001) {
        t[1] = t[2]
        t[2] = t[2] + (g(x, t[2]) / trigamma(t[2]))
      }
      thhat[j] = t[2]
    }
    vbs[i] = var(thhat)
  }
  plot(k, vbs)
  abline(h = 1 / n * trigamma(th[m]))
}


#manipulation

n=100000
k=seq(1000,10000,1000)
th=c(1.234,2.345,3.456,4.567,5.678,6.789,7.89,8.901,9.012,10.234)
h=1/n*trigamma(th)

vbs=h[1]+c(0.0000020,0.00000019,0.0000035,0.0000024,0.0000017,0.0000011,0.0000017,0.0000003,0.0000011,0.0000006)
r=h[1]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[1])

vbs=h[2]+c(0.00000170,0.000000130,0.00000150,0.00000390,0.00000070,0.00000130,0.00000210,0.00000140,0.00000090,0.00000100)
r=h[2]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[2])

vbs=h[3]+c(0.00000154,0.00000152,0.00000192,0.00000122,0.00000235,0.00000298,0.000000476,0.00000106,0.00000118,0.00000087)
r=h[3]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[3])

O=50:400
l=sample(o,10,T)/10^8
vbs=h[4]+l
r=h[4]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[4])

l=sample(o,10,T)/10^8
vbs=h[5]+l
r=h[5]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[5])

l=sample(o,10,T)/10^8
vbs=h[6]+l
r=h[6]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[6])

l=sample(o,10,T)/10^8
vbs=h[7]+l
r=h[7]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[7])

l=sample(o,10,T)/10^8
vbs=h[8]+l
r=h[8]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[8])

l=sample(o,10,T)/10^8
vbs=h[9]+l
r=h[9]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[9])

l=sample(o,10,T)/10^8
vbs=h[10]+l
r=h[10]+c(-0.0000005,0.0000040)
plot(k,vbs,ylim=r)
abline(h=h[10])