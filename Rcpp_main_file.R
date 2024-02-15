## =========================================================
## libraries
## =========================================================

library(data.table)
library(magrittr)
library(Rcpp)
library(RcppArmadillo)
library(bench)

library(tvm)


## =========================================================
## excercises come from hadley's book, the chapter on Rcpp
## =========================================================

# https://adv-r.hadley.nz/rcpp.html#sourceCpp


## =========================================================
## establish working directory here
## =========================================================

# home computer
my_path <- 'C:/Users/micha/OneDrive/Documents/02_learning/rcpp/Rcpp_practice/'

# work computer

# my_path <- ''


## =========================================================
## hadley's excercises
## =========================================================

# cppFunction() allows you to write C++ functions in R:

cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')
# add works like a regular R function
add
#> function (x, y, z) 
#> .Call(<pointer: 0x107536a00>, x, y, z)

add(1,2,3)



# When you run this code, Rcpp will compile the C++ code and construct an R function that connects to the compiled C++ function. 
# There’s a lot going on underneath the hood but Rcpp takes care of all the details so you don’t need to worry about them.
# 
# The following sections will teach you the basics by translating simple R functions to their C++ equivalents. We’ll start simple 
# with a function that has no inputs and a scalar output, and then make it progressively more complicated:



# 25.2.1 No inputs, scalar output
# Let’s start with a very simple function. It has no arguments and always returns the integer 1:


one <- function() 1L
one
one()


# The equivalent C++ function is:

# int x_one() {
#   return 1;
# }

# We can compile and use this from R with cppFunction()

cppFunction('int x_one() {
  return 1;
}')

x_one

# This small function illustrates a number of important differences between R and C++:
#   
# - The syntax to create a function looks like the syntax to call a function; you don’t use assignment to create functions as you do in R.
# 
# - You must declare the type of output the function returns. This function returns an int (a scalar integer). The classes for the most 
#   common types of R vectors are: NumericVector, IntegerVector, CharacterVector, and LogicalVector.
# 
# - Scalars and vectors are different. The scalar equivalents of numeric, integer, character, and logical vectors are: double, int, String, and bool.
# 
# - You must use an explicit return statement to return a value from a function.
# - Every statement is terminated by a ;.



# 25.2.2 Scalar input, scalar output
# The next example function implements a scalar version of the sign() function which returns 1 if the input is positive, and -1 if it’s negative:
#   
signR <- function(x) {
  if (x > 0) {
    1
  } else if (x == 0) {
    0
  } else {
    -1
  }
}

signR
signR(-100)


cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

signC
signC(100)


# In the C++ version:
#   
# - We declare the type of each input in the same way we declare the type of the output. While this makes the code a little more verbose, 
#   it also makes clear the type of input the function needs.
# 
# - The if syntax is identical — while there are some big differences between R and C++, there are also lots of similarities! C++ also has 
#   a while statement that works the same way as R’s. As in R you can use break to exit the loop, but to skip one iteration you need to use 
#   continue instead of next.



# 25.2.3 Vector input, scalar output
# One big difference between R and C++ is that the cost of loops is much lower in C++. For example, we could implement the sum function in R 
# using a loop. If you’ve been programming in R a while, you’ll probably have a visceral reaction to this function!

sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}

sumR
sumR(1:30)

# In C++, loops have very little overhead, so it’s fine to use them. In Section 25.5, you’ll see alternatives to for loops that more clearly 
# express your intent; they’re not faster, but they can make your code easier to understand.

cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

sumC

sumC(1:30)


# The C++ version is similar, but:
#   
# To find the length of the vector, we use the .size() method, which returns an integer. C++ methods are called with . (i.e., a full stop).
# 
# The for statement has a different syntax: for(init; check; increment). This loop is initialised by creating a new variable called i with 
# value 0. Before each iteration we check that i < n, and terminate the loop if it’s not. After each iteration, we increment the value of i 
# by one, using the special prefix operator ++ which increases the value of i by 1.
# 
# In C++, vector indices start at 0, which means that the last element is at position n - 1. I’ll say this again because it’s so important: 
#   IN C++, VECTOR INDICES START AT 0! This is a very common source of bugs when converting R functions to C++.
# 
# Use = for assignment, not <-.
# 
# C++ provides operators that modify in-place: total += x[i] is equivalent to total = total + x[i]. Similar in-place operators are -=, *=, and /=.
# 
# This is a good example of where C++ is much more efficient than R. As shown by the following microbenchmark, sumC() is competitive 
# with the built-in (and highly optimised) sum(), while sumR() is several orders of magnitude slower.

x <- runif(1e3)
bench::mark(
  sum(x),
  sumC(x),
  sumR(x)
)[1:6]

# A tibble: 3 × 6
# expression      min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# sum(x)        3.2µs    3.4µs   282021.        0B     28.2
# sumC(x)       6.9µs    7.7µs   105333.    6.62KB     10.5
# sumR(x)      93.6µs   94.4µs     9848.        0B      0 



# 25.2.4 Vector input, vector output
# Next we’ll create a function that computes the Euclidean distance between a value and a vector of values:

pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}

pdistR
pdistR(x = 1, ys = 1:10)

# In R, it’s not obvious that we want x to be a scalar from the function definition, and we’d need to make that clear in the documentation. 
# That’s not a problem in the C++ version because we have to be explicit about types:

cppFunction('NumericVector pdistC(double x, NumericVector ys) {
  int n = ys.size();
  NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out;
}')

pdistC
pdistC(x = 1, ys = 1:10)


# This function introduces only a few new concepts:
#   
# We create a new numeric vector of length n with a constructor: NumericVector out(n). Another useful way of making a vector is to copy an existing one: 
# NumericVector zs = clone(ys).
# 
# C++ uses pow(), not ^, for exponentiation.
# 
# Note that because the R version is fully vectorised, it’s already going to be fast.

y <- runif(1e6)
bench::mark(
  pdistR(0.5, y),
  pdistC(0.5, y)
)[1:6]

# A tibble: 2 × 6
# expression          min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>     <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# pdistR(0.5, y)   8.35ms   8.42ms      118.    7.65MB     36.7
# pdistC(0.5, y)   6.99ms   7.06ms      139.    7.64MB     25.0


# 25.2.5 Using sourceCpp
# 
# So far, we’ve used inline C++ with cppFunction(). This makes presentation simpler, but for real problems, it’s usually easier to use stand-alone C++ 
# files and then source them into R using sourceCpp(). This lets you take advantage of text editor support for C++ files (e.g., syntax highlighting) as 
# well as making it easier to identify the line numbers in compilation errors.
# 
# Your stand-alone C++ file should have extension .cpp, and needs to start with:
#   
#   #include <Rcpp.h>
#   using namespace Rcpp;
# 
# And for each function that you want available within R, you need to prefix it with:
#   
#   // [[Rcpp::export]]

# You can embed R code in special C++ comment blocks. This is really convenient if you want to run some test code:
#   
#   /*** R
#   # This is R code
#   */

# The R code is run with source(echo = TRUE) so you don’t need to explicitly print output.
# 
# To compile the C++ code, use sourceCpp("path/to/file.cpp"). This will create the matching R functions and add them 
# to your current session. Note that these functions can not be saved in a .Rdata file and reloaded in a later session; 
# they must be recreated each time you restart R.
# 
# For example, running sourceCpp() on the following file implements mean in C++ and then compares it to the built-in mean():

# go see the file for this text.  it's called "my_other_cpp_file.cpp"


sourceCpp(paste0(my_path, "my_other_cpp_file.cpp"))

# A tibble: 2 × 13
# expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result    memory             time               gc                  
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   
# mean(x)       325µs    326µs     3035.   23.28KB        0  1518     0   
# meanC(x)      107µs    108µs     8952.    2.49KB        0  4472     0   


# NB: If you run this code, you’ll notice that meanC() is much faster than the built-in mean(). This is because it trades 
# numerical accuracy for speed.
# 
# For the remainder of this chapter C++ code will be presented stand-alone rather than wrapped in a call to cppFunction. 
# If you want to try compiling and/or modifying the examples you should paste them into a C++ source file that includes 
# the elements described above. This is easy to do in RMarkdown: all you need to do is specify engine = "Rcpp".


## =========================================================
## now to create a matrix of cash flows, and use c++ to irr them
## =========================================================

sourceCpp(paste0(my_path, "my_irr_cpp_file.cpp"))

calculateIRR

cashflows <- c(-100, 50, 50, 50, 50)  # Example cash flows
irr <- calculateIRR(cashflows)
print(irr)





my_cash_flows <- matrix(
  rep(c(-100, (runif(20) * 10) + 10), 1e6),
  nrow = 21,
  ncol = 1e6
)

my_cash_flows[,1]







cppFunction(depends = "RcppArmadillo", ' 
arma::mat fun_cpp_arma(arma::mat Mat) {
 
   // You can get the number of rows and columns of matrix Mat as follows
   Rcout << "Mat.n_rows: " << Mat.n_rows << std::endl;
   Rcout << "Mat.n_cols: " << Mat.n_cols << std::endl;
   int nrow_Mat = Mat.n_rows;
 
   // Conduct loop over the rows of matrix Mat
   // For each row: add the number of the row to the matrix values
   for ( int i=0; i<nrow_Mat; i++ ) {
     Mat.row(i) = Mat.row(i) + i;
   }
   return Mat ;
}
')

fun_cpp_arma(my_cash_flows)

my_cash_flows


