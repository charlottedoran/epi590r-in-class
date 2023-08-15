x <- 4

x^2
#Create a function to square a number
square <- function(x) {
	x^2
}

#Check that it worked
square(x)
square(53)
53^2

#Create a function to raise a number to any power
raise <- function(x, power) {
	x^power
}

#Check that it worked
raise(x = 4, power = 2)
raise(x = 2, power = 4)
2^4
raise(x = 5, power = 2)

#Change the raise function to set it to a default of squaring
raise <- function(x, power = 2) {
	x^power
}

#Check that it worked
raise(2)
raise(x = 2)
raise(5, power = 3)
raise(5)
