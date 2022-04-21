
#question 1

log(x=1:5)
log(x=1:5,base=50:100)
#vectorized in both 

log(x=3,base=1:10)
# it make sens for 1 argument to be length 1 , and 1 argument larger


# question 2 

a=1
b="hi"
c=TRUE
d=2L
e=1+4i;e


class(c(a,b,c,d,e))
class(c(a,c,d,e))
class(c(a,c,d))
class(c(c,d))
#1: charater
#2: complex
#3: numeric
#4: interger
#5: logical

# question 3

list(x=c(1L,2L,3L,4L,5L),y=c("hee","won"),
     z= function(x) (y=x+1) , w=NA)

# question 4

a=1:10000

#1.
c=(as.integer(a/13)-a/13);c
length(c[c==0.00])
#2.
length(a[a%%13==0])
#3
d=a/13-a%/%13;d
length(d[d==0])

#question 5

iris
is.list(iris)
#yes data frame is lists,
#therefore it can contain various type of data like character, interger 



#6

n=Inf
n+10
n-100
1/n
2*n
-5*n
log(n)