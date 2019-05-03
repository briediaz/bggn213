Class 7: R functions and packages
================
Brie Diaz
4/24/2019

More on function writing
------------------------

First we will revisit our function from last day

``` r
source("http://tinyurl.com/rescale-R")
```

Rest the **rescale()** function

``` r
rescale(1:10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

rescale(c(1:10, "string"))
--------------------------

-&gt; error

cmd + option + i to open up an R coding bar
===========================================

``` r
x <- c(1:10, "string")
!is.numeric(x)
```

    ## [1] TRUE

``` r
##rescale2(x)
```

Function practice
-----------------

Write a function to identify NA elements in two vectors

Start with a simple example input where I now what the answer should be.

``` r
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)
```

``` r
is.na(x)
```

    ## [1] FALSE FALSE  TRUE FALSE  TRUE

``` r
is.na(y)
```

    ## [1]  TRUE FALSE  TRUE FALSE FALSE

``` r
which(is.na(x))
```

    ## [1] 3 5

``` r
which(is.na(y))
```

    ## [1] 1 3

I am looking for the positions where it is TRUE in both vectors....

``` r
is.na(x) & is.na(y)
```

    ## [1] FALSE FALSE  TRUE FALSE FALSE

Take the sum to find how many TRUEs there are (TRUE = 1; FALSE = 0)

``` r
sum(is.na(x) & is.na(y))
```

    ## [1] 1

This is my working snippet of code that I can use as the body of my first function!!

``` r
both_na <- function(x,y) {
  sum(is.na(x) & is.na(y))
}
```

``` r
both_na(x,y)
```

    ## [1] 1

``` r
both_na(c(NA, NA, NA), c(NA, NA, 1))
```

    ## [1] 2

``` r
both_na(c(NA, NA, NA), c(1, NA, NA, NA))
```

    ## Warning in is.na(x) & is.na(y): longer object length is not a multiple of
    ## shorter object length

    ## [1] 3

``` r
both_na(c(NA, NA, NA), c(1, NA, NA, NA, NA, NA))
```

    ## [1] 5

Check that the length of our inputs are equal

``` r
x <- c(NA, NA, NA)
y <- c(1, NA, NA, NA, NA, NA)
length(x) != length(y)
```

    ## [1] TRUE

``` r
both_na2 <- function(x, y) {
 if(length(x) != length(y)) {
 stop("Input x and y should be the same length")
 }
 sum( is.na(x) & is.na(y) )
}
```

Try the both\_na3() function with extra features

``` r
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)
both_na3(x, y)
```

    ## Found 1 NA's at position(s):3

    ## $number
    ## [1] 1
    ## 
    ## $which
    ## [1] 3

The which() function finds which positions are TRUE

``` r
which(c(F, F, T, F, T))
```

    ## [1] 3 5

Write a function grade() to determine an overall grade from a vector of student homework assignment scores, dropping the lowest singe assigment score

``` r
s1 <- c(100, 100, 100, 100, 100, 100, 100, 90)
sum(s1)
```

    ## [1] 790

``` r
sum(s1) - min(s1)
```

    ## [1] 700

``` r
length(s1) -1
```

    ## [1] 7

``` r
(sum(s1) - min(s1))/(length(s1) -1)
```

    ## [1] 100

Find the minimum score

``` r
min(s1)
```

    ## [1] 90

Subtract the minimum score from the sum of the vector

``` r
sum(s1) - min(s1)
```

    ## [1] 700

Divide by the length of the vector -1 (n-1)

``` r
(sum(s1) - min(s1))/(length(s1)-1)
```

    ## [1] 100

I got my snippet, now to turn it into my first version of the grade() function

``` r
grade <- function(x) {
  (sum(x, na.rm =TRUE) - min(x, na.rm =TRUE))/(length(x)-1)
}
```

OR, if we want the user to be able to determine what to do with NA: - because we need to pass the na.rm through the function/s that is/are actually doing the work!

``` r
grade1 <- function(x, na.rm = TRUE) {
  if (na.rm) {
    return((sum(x, na.rm =TRUE) - min(x, na.rm =TRUE))/(length(x)-1))
  } else {
    (sum(x) - min(x))/(length(x)-1)
  }
}
```

``` r
s1 <- c(100, 100, 100, 100, 100, 100, 100, 90)
s2 <- c(100, NA, 90, 90, 90, 90, 97, 80)

grade(s1)
```

    ## [1] 100

``` r
grade(s2)
```

    ## [1] 79.57143

``` r
class <- read.csv("student_homework.csv", row.names = 1)
```

``` r
grade(class[1,])
```

    ## [1] 91.75

APPLY grade() to the whole class

``` r
ans <- apply(class, 1, grade)
```

``` r
sort(ans, decreasing = TRUE)
```

    ##  student-7  student-8 student-13  student-1 student-12 student-16 
    ##      94.00      93.75      92.25      91.75      91.75      89.50 
    ##  student-6  student-5 student-17  student-9 student-14 student-11 
    ##      89.00      88.25      88.00      87.75      87.75      86.00 
    ##  student-3 student-19 student-20  student-2 student-18  student-4 
    ##      84.25      82.75      82.75      82.50      72.75      66.00 
    ## student-15 student-10 
    ##      62.50      61.00

``` r
df1
```

    ##     IDs exp
    ## 1 gene1   2
    ## 2 gene2   1
    ## 3 gene3   1

``` r
df2
```

    ##     IDs exp
    ## 1 gene2  -2
    ## 2 gene4  NA
    ## 3 gene3   1
    ## 4 gene5   2

One last function example
-------------------------

Find the intersection of two sets (two vectors) \# Start with a simple version of the problem

``` r
x <- df1$IDs
y <- df2$IDs

intersect(x,y)
```

    ## [1] "gene2" "gene3"

``` r
x
```

    ## [1] "gene1" "gene2" "gene3"

``` r
y
```

    ## [1] "gene2" "gene4" "gene3" "gene5"

``` r
x %in% y
```

    ## [1] FALSE  TRUE  TRUE

``` r
which(x %in% y)
```

    ## [1] 2 3

``` r
x[x %in% y]
```

    ## [1] "gene2" "gene3"

``` r
y %in% x
```

    ## [1]  TRUE FALSE  TRUE FALSE

``` r
gene_intersect <- function(x, y) {
  cbind(x[x %in% y], y[y %in% x])
}
```

Improve by simplifying for human consumption
============================================

``` r
gene_intersect4 <- function(df1, df2, gene.colname="IDs") {

 df1.name <- df1[,gene.colname]
 df2.name <- df2[,gene.colname]

 df1.inds <- df1.name %in% df2.name
 df2.inds <- df2.name %in% df1.name

 cbind( df1[ df1.inds, ],
 exp2=df2[ df2.inds, "exp"] )
}
```

Improve by simplifying for human consumption
============================================

gene\_intersect4 &lt;- function(df1, df2, gene.colname="IDs") {

df1.name &lt;- df1\[,gene.colname\] df2.name &lt;- df2\[,gene.colname\]

df1.inds &lt;- df1.name %in% df2.name df2.inds &lt;- df2.name %in% df1.name

cbind( df1\[ df1.inds, \], exp2=df2\[ df2.inds, "exp"\] ) } \# Getting closer! gene\_intersect4(df1, df2) \#&gt; IDs exp exp2 \#&gt; 2 gene2 1 -2 \#&gt; 3 gene3 1 1

Test, break, fix, text again
============================

df1 &lt;- data.frame(IDs=c("gene1", "gene2", "gene3"), exp=c(2,1,1), stringsAsFactors=FALSE) df3 &lt;- data.frame(IDs=c("gene2", "gene2", "gene5", "gene5"), exp=c(-2, NA, 1, 2), stringsAsFactors=FALSE) \# Works but could do with more spit and polish! gene\_intersect4(df1, df3) \#&gt; IDs exp exp2 \#&gt; 1 gene2 1 -2 \#&gt; 2 gene2 1 NA \#&gt; Warning in data.frame(..., check.names = FALSE): row names were found from a short variable and have been discarded

``` r
merge(df1, df2, by="IDs")
```

    ##     IDs exp.x exp.y
    ## 1 gene2     1    -2
    ## 2 gene3     1     1

``` r
#> IDs exp.x exp.y
#> 1 gene2 1 -2
#> 2 gene3 1 1
```
