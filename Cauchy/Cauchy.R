// Evaluating sample size, so that NE is unbiased //

n=seq(100,10000,100)
thne=array(0)
for(i in n){
	x=rcauchy(i,-2.135,5)

	g=function(th){
		s=0
		for(i in 1:length(x)){
			s=s+{(x[i]-th)/(1+(x[i]-th)^2)}
		}
		s
	}

	g1=function(th){
		s=0
		for(i in 1:length(x)){
			s=s+{((x[i]-th)^2-1)/(1+(x[i]-th)^2)^2}
		}
		s
	}

	d=seq(-10,20,0.1)
	vl=seq(-10,20,0.5)
	plot(d,g(d))
	abline(h=0,v=vl)

	th=array(0)
	th[1]=-2
	th[2]=th[1]-(g(th[1])/g1(th[1]))

	it=0
	while(abs(th[2]-th[1])>0.00000001){
		th[1]=th[2]
		th[2]=th[2]-(g(th[2])/g1(th[2]))
		it=it+1
	}
	thne[i/100]=th[2]
}
plot(n,thne)
abline(h=-2.135)

// Evaluating variance of UNE //

thb=array(0)
for(k in 1:5000){
  y=sample(x,length(x),T)
  
  g=function(th){
    s=0
    for(i in 1:length(y)){
      s=s+{(y[i]-th)/(1+(y[i]-th)^2)}
    }
    s
  }
  
  g1=function(th){
    s=0
    for(i in 1:length(y)){
      s=s+{((y[i]-th)^2-1)/(1+(y[i]-th)^2)^2}
    }
    s
  }
  
  d=seq(-10,20,0.1)
  vl=seq(-10,20,0.5)
  plot(d,g(d))
  abline(h=0,v=vl)
  
  th=array(0)
  th[1]=-2
  th[2]=th[1]-(g(th[1])/g1(th[1]))
  
  it=0
  while(abs(th[2]-th[1])>0.00000001){
    th[1]=th[2]
    th[2]=th[2]-(g(th[2])/g1(th[2]))
    it=it+1
  }
  thb[k]=th[2]
}

var(thb)/(2/10000)
