
# Set your desired work directory
getwd()  
setwd("C:/Users/najee/Documents/R/MSDS6306-W5-LS")
library(ggplot2)
library(stats)
library(devtools)

# Prelim. / Code prep.
rm(list = ls()) 
objects()


# Question 1: Write a function in R called P7 which takes one argument n, and will return (or print) the first n numbers in base 7
P7 <- function(n, base = 7) 
{
  if (n == 0) 
    return(c())
  if (n < 0) 
    stop(simpleError("Negative number can???t be used with this function"))
  nDigits <- ceiling(log(n + 1, 7))
  powers <- 7 ^ (0:nDigits)
  out <- diff(n %% powers)/powers[-length(powers)]
  out1 <- rev(out)
  out2 <- paste(out1, collapse = '')
  out2 = as.numeric(out2)
  out3 <- 1:out2
  return(out3)
}
# To use the function, just simple enter the integer desired (n) and it will return the series of the first (n) integer up to the (n) correspondent number in base 7. The functions also checks for null and negative values. For example:  
P7(5) # will give you [1] 1 2 3 4 5 (as 5 in base 10 equals to 5 in base 7)
P7(25) # will give you [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 [26] 26 27 28 29 30 31 32 33 34  (as 25 in base 10 equals to 34 in base 7
P7(30) # will give you [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 (as 30 in base 10 equals to 42 in base 7

# Question 2. Write a function called ???base10to7??? which takes one argument x, and will convert a decimal number to base 7. At input check  if x is scalar or not and control the input.
### is.scalar <- function(n) {is.atomic(n) && length(n) == 1L}
Base10to7 <- function(n, base = 7) 
{ 
  if (n == 0) 
    return(c())
  if (n < 0) 
    stop(simpleError("Negative number can???t be used with this function"))
  
##  if ( is.scalar(n) == FALSE) 
##    stop(simpleError("Please use scalar numbers"))
  nDigits <- ceiling(log(n + 1, 7))
  powers <- 7 ^ (0:nDigits)
  out <- diff(n %% powers)/powers[-length(powers)]
  return(rev(out))
}
# To use the function, just simple enter the desired integer in base 10 and it will return the correspondent number in base 7. The functions also checks for the use of scalar numbers and null and negative values. For example:  
Base10to7(5) # will give you 5 
Base10to7(25) # will give you [1] 34
Base10to7(77) # will give you [1] 1 4 0

# Question 3. Write a function called ???base7to10??? which takes an argument y, and converts a base number 7 to decimal. 
Base7to10 = function(base_number, base = 7) 
  {
  split_base = strsplit(as.character(base_number), split = "")
  return(sapply(split_base, function(x) sum(as.numeric(x) * base ^ (rev(seq_along(x) - 1)))))
  }

# To use the function, just simple enter the desired integer (to be considered in base 7) and it will return the correspondent number in base 10. For example: 
Base7to10(5) # will give you 5 
Base7to10(21) # will give you [1] 15
Base7to10(51) # will give you [1] 36

# Question 4: generalize the function to take any form & especially steps 1 2 , 3 above
numberInBase <- function(number,base)
  {
  numberInBaseRecur <- function(number,base)
    {
    lastDigit <- function(number,base) number %% base
    if (number == 0) result <- c(0)
    else result <- c(numberInBaseRecur(number %/% base,base),
                     lastDigit(number,base))
    result
    }
  result <- numberInBaseRecur(number,base)
  while (result[1] == 0 && length(result) > 1) result <- result[-1]
  result
  }
use <- function(digiseq)
  {
  digits <- c(as.character(0:9),LETTERS)
  paste(sapply(digiseq,function(x)digits[x + 1]),collapse = "")
  }
use(numberInBase(21,2))

# To use the function use(numberInBase(m,n), just simple enter a desired integer as first parameter and the desired base value as second parameter and the function will return the input m number in the selected base n. For example:  
use(numberInBase(21,2)) # will give you [1] "10101"
use(numberInBase(30,8)) # will give you [1] "36"
use(numberInBase(556,10)) # will give you [1] "556"
use(numberInBase(556,16)) # will give you [1] "22C"


# Question 5: see if we can plot a bar or histo chart for each
# As an example, we are using here the functions use(numberInBase(m,n)) to plot a chart with 2 numbers, the first one being the inputed number m (assumed in base 10) and the second one its equivalent in base n
m <- 167
n <- 8
c <- use(numberInBase(m , n)) # This returns 247 as the equivalent of 167 (b10) in base 8
v <- c(m , c)
v1 <- as.numeric(v)

# using additional arguments to customize a graphic
v1LabelMain <- "Correspondent numbers in different scale"
v1LabelX <- "Numbers"
v1LabelY <- "Values"
v1RainbowColors <- rainbow(length(v1))
# Y axis range could be adjusted like this: ylim<-max(v1) + v1LimY <- c(0, ylim) as well as other customizations are possible ie showing legend, values, adding text, dividing the chart in panels, etc. ???
barplot(height = v1, main = v1LabelMain, xlab = v1LabelX, ylab = v1LabelY, ylim = c(0,300), col = v1RainbowColors)


# Question 6: plot a histogram for density function Pr(5<X<15) and the uniform distribution is (0, 30)
# Can we generalize this function plot?
# Let's draw a sample of size 1000 from a normal distribution with mean 0 and standard deviation 30 

x <- rnorm(1000, 0, 30)
m = mean(x)
s = sd(x)
hist(x, col = "green", probability = TRUE, main = "Histograms and density function for a rand (0,30) sample")

# Adding vertical lines to capture 95% of the sample
lines(c(-s*1.96,-s*1.96), c(0, dnorm(-s*1.96)), col = "green")
lines(c(s*1.96,s*1.96), c(0, dnorm(s*1.96)), col = "green")

# Adding the distribution curve with parameters m and s 
curve(dnorm(x, mean = m, sd = s), col = "red", add = TRUE)
# Adding the probability density curve  
lines(density(x), col = "black")



# It???d be possible to generalize the function plot eg for further references see, amongst others, 
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_dist.html 
# https://tomizonor.wordpress.com/2013/05/11/distribution-density-histogram/
# http://nicercode.github.io/guides/plotting/
