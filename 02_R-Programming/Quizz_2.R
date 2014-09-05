# Question 1
cube <- function(x, n){
	x^3
}
cube(3) # returns 27

# Question 2
x <- 1:10
if(x > 5){
	x <- 0
} # warning because x > 5 is a logical vector of length 10

# Question 3
f <- function(x){
	g <- function(y){
		y + z
	}
	z <- 4
	x + g(x)
}
z <- 10
f(3) # returns 10, looks for 'z' in the environment in which it was defined (body of function 'f')


