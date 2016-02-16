# angle(a,b)#angle of two vectors. only for two vector!
# PLAdata(N,M=2)#generate data set(of size N) used for perceptron learning
# PLA(data)#core algorithm used to learn. The format of input 'data': M+1 column(x, classification(-1 or 1)), each row is an observation. 
# avaragePLA(N=10,M=2,T=10)#N examples, M dimension(of input x), T runs
angle = function(a,b){#only for two vector
  return(acos(t(a)%*%b/((t(a)%*%a)*(t(b)%*%b))^0.5))
}

PLAdata = function(N,M=2){#generate data set(of size N) used for perceptron learning
  f = runif(M+1,-1,1)
  x = runif(M*N,-1,1)
  x = matrix(c(rep(1,N),x),ncol=M+1)
  bool = sign(x%*%matrix(f))
  bool[bool==0]=-1
  result = matrix(c(x[,-1],bool),ncol=M+1)
  return(list(data=result,f=f))
}

PLA = function(data){#the format of input 'data': 3 column(x, y, boolean classification), each line is a observation. 
  j = dim(data)[2]#j is the number of columns
  w = rep(0,j)#initialize w. or use w = seq(0,0, length=j)
  x = matrix(c(rep(1,dim(data)[1]),data[,-j]),ncol=j)
  count = 0 #iteratation times
  error=dim(data)[1]
  errorm=c()
  while(count<10000){
    pre = sign(x%*%t(t(w)))
    foota = 1:length(pre)
    foot = foota[pre!=data[,j]]
    error = length(foot)
    errorm = c(errorm,error)
    if(error==0) break;
    point = foot[ceiling(runif(1)*error)]#get a certer misclassified point
    w = w+ data[point,j]*x[point,]
    count = count+1
  }
  return(list(count=count,errorm=errorm,g=w))
}

avaragePLA = function(N=10,M=2,T=10){#N examples, M dimension, T runs
  resultsum=0
  mis = 0
  Eout = 0
  for(i in 1:T){
    rawdata = PLAdata(N,M)
    tmp = PLA(rawdata$data)
    resultsum = resultsum + tmp$count
    Eout = Eout + angle(rawdata$f,tmp$g)/pi
  }
  return(c(resultsum/T,Eout/T))
}
avaragePLA(T=1)
