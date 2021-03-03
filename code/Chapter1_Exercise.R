#Question 1
calculate = function(x, y, operator = '+') {
  if (operator == "+") {
    print(x + y)
  } else if (operator == "-") {
    print(x - y)
  } else if (operator == "*") {
    print(x * y)
  } else{
    print(x / y)
  }
}
calculate(50, 10, '-')

#Question2---Method1
Vector2Matrix = function(x) {
  A = matrix(NA, length(x), length(x))
  for (i in 1:length(x)) {
    for (j in 1:length(x)) {
      A[i, j] = x[i] - x[j]
    }
  }
  return(A)
}
Vector2Matrix(x = 1:8)

#Question2---Method2
Vector2Matrix_Fast = function(x){
  l = length(x)
  A0 = matrix(rep(x,l),l,l)
  V0 = A0 - t(A0)
  return(V0)
}
Vector2Matrix_Fast(x = 1:8)
?t

#Question3---Method1
oddSum = function(x) {
  summ = 0
  i = 1
  while (i <= x) {
    summ = summ + i
    i = i + 2
  }
  print(summ)
}
oddSum(5)

#Question3---Method2
oddSum2 = function(x) {
  summ = 0
  for (i in 1:x) {
    if (i%%2 != 0) {
      summ = summ + i
    }
  }
  print(summ)
}
oddSum2(5)

#Question3---Mehtod3
oddSum3 = function(x){
  oddn = seq(1, x, 2)
  return(sum(oddn))
}
oddSum3(100)

#Question 4
simul1 = function(x){
  sum(x)/
  
}