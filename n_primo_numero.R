
numero_primo_numero <- function(n)
{
  #como sabemos, los números primos son mayores que 1 (por ende positivos), enteros y solo divisibles por 1 y por si mismos. 
  #entonces, si el número es menor o igual a 0 configuramos para que el algoritmo nos muestre un mensaje de error
  if (n<=0){
    return("Error, solo se aceptan números enteros y mayores a 1")
  }
  #el primer número primo, por definición arbitraria es el 2 (dos). entonces:
  else if (n==1){
    return("El número primo número 1 es: 2")
    #el número primo ubicado en la posición 1 siempre va a ser el 2 (dos)
  } 
  else{
    #si el número primo que buscamos no se ubica en una posición negativa, ni neutral y es mayor a 1 (uno), el algoritmo nos va a responder que el número que buscamos, en la posición "n" es: (número primo ubicado en la posición "n")
    print(paste("El número primo número",n, "es:"))
    #vamos a crear un vector para guardar los números entre los que vamos a buscar los números primos ubicados en la posición n
    disc = c()
    vec = c()
    #debemos establecer el limite superior, es decir el número hasta el que el algoritmo va a buscar números primos. 
    # como el número primo 1000 es 7919, vamos a poner como límite 10000 para tener margen de buscar números primos en posiciones mayores pero sin tener un costo computacional demasiado alto sin sentido
    for (num in seq(3,10000))
    {
      #definimos qué números son primos y qué otros no lo son
      #hay que recordar que nuestro algoritmo va a buscar número mayores a 1, entonces:
      disc[num-2]= FALSE
      #paras i en la secuencia desde 2 a num-1:
      for(i in seq(2,num-1))
      {
        if((num%%i)==0){
          disc[num-2]=TRUE
          break
        }
      }
      #los números primos van a ser almacenados en el vector "vec", cuando no son primos el valor que almacenamos es 0
      if (disc[num-2]==FALSE){
        vec[num-2]=num
      }
      else{
        vec[num-2]=0
      }
    }
  }
  #creamos un vector nuevo en el que almacenamos los números primos (sin los ceros mencionados arriba que correspondían a números no primos) de manera ordenada de acuerdo a su posición
  vec_primos <- c()
  cont=1
  for (k in 1:length(vec)){
    if ((vec[k])!=0){
      vec[k] -> vec_primos[cont]
      cont = cont+1
    }
  }
  return(vec_primos[n-1])
}