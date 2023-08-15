#1
# start out with a number to test
x <- 3

# you'll want your function to return this number
x^2

square <- function(x) {
	x^2
}

# test it out
square(x)
square(53)

53^2 # does this match? yes

#2
raise<-function(x,power){
		x^power
}

# test with
raise(x = 2, power = 4)
# should give you
2^4

#3
raise<-function(x,power=2){
	x^power
	return(new_val)
}

# test
raise(x = 5)
# should give you
5^2
