LeastSquareApproximation<-function(x,y,d){
  matrixA<-matrix(0,d,d)
  m<-length(x)
  for (i in 1:d){
    for(j in 1:d){
      matrixA[i,j]=sum(x^(i+j-2))

    }

  }
  matrixA[1,1]=m
  print(matrixA)

  matrixB<-matrix(0,d,1)
  for (i in 1:d){

    matrixB[i]<-sum(y*x^(i-1))


  }
  print(matrixB)

  print("coefficient")
  ans<-matrix(0,d,1)
  ans<-solve(matrixA,matrixB)
  print(ans)

  sum=0
  f<-matrix(0,m,1)
  for (i in 1:m){
    for (j in 1:d){

      f[i]=f[i]+ans[j]*(x[i]^(j-1))

    }
  }
  print("error")
  error<-sum((y-f)^2)
  print(error)

  plot( x, y, type="l", col="red" ,main = 'Least Squares Approximation')
  par(new=TRUE)
  plot( x, f, type="l", col="blue" )
}
