## creamos una funcion que guarde la matrix en cache


makeCacheMatrix <- function(Matrix = matrix()){
    #creamos una variable para hacer la inversa
    M_inversa <- NULL
    
    M_set <- function(y){
        Matrix <<- y
        M_inversa <<- NULL
    }
    
    #obtener la matrix original
    M_Get <- function() Matrix
    
    #fijar la inversa
    Set_Inv <- function(inversa) M_inversa <<- inversa
    
    #obtener la inversa
    Get_Inv <- function() M_inversa
    
    #lista que retorna las 4 funciones
    
    list(M_set = M_set, 
         M_Get = M_Get,
         Set_Inv = Set_Inv,
         Get_Inv = Get_Inv)
}

#funcion para hallar la inversa

cacheSolve <- function(M_inv, ...){
    M_inversa <- M_inv$Get_Inv()
    
    if(!is.null(M_inversa)){
        message("obteniendo datos en cache")
        return(M_inversa)
    }
    
    #Hallando la inversa
    datos <- M_inv$M_Get()
    M_inversa <- solve(datos, ...)
    M_inv$Set_Inv(M_inversa)
    M_inversa
}