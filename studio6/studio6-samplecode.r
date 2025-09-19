#---------------------------------
# Sample code for 18.05 Studio 6
#--------------------------------
# 1. Discretizing a joint pdf
# 2. Drawing a simple heat map
# 3. Drawing a heat map corresponding to the joint pdf in part 1.
# 4. Playing with the colors in a heat map
# 5. Linear regression



# 1. Discretizing a joint probability density
# We will build table approximating a continuous joint pdf.
  
# We assume the following
#  A. a and b are independent parameters. The pair (a,b) represents a single hypothesis
#  B. a ~ exp(0.1)
#  C. b ~ Norm(3,4)

# We need to pick ranges on a and b. In general they should make sense in the context of the problem. Here we choose them somwhat arbitrarily.
  
# We also need to pick discretization stepsizes da and db
# range of a = [0,25], da = 0.1
# range of b = [-13,19] db = 0.1
# Discretize a and b
a = seq(0,25, 0.1)
b = seq(-13,19, 0.1)
  
# Discretize the densities of a and b
tmp_prob_a = dexp(a, 0.1)
tmp_prob_b = dnorm(b, 3, 4)
# Normalize the discrete probabilities
prob_A = tmp_prob_a/sum(tmp_prob_a)
prob_B = tmp_prob_b/sum(tmp_prob_b)

# Inititalize and build the joint probability table
# We use a double loop. A simpler method is to use the R function outer which does exactly what we want.
joint_prob_table = matrix(NA, nrow=length(prob_A), ncol=length(prob_B))
for (i in 1:length(prob_A)) {
  for (j in 1:length(prob_B)) {
    joint_prob_table[i,j] = prob_A[i]*prob_B[j]
  }
}
# Name the rows and columns with the values
rownames(joint_prob_table) = a
colnames(joint_prob_table) = b
  

cat('Size of table:', dim(joint_prob_table), '\n')
cat('Here is a small part of the table:\n')
# Note the values of a and b are the row and column labels
print(joint_prob_table[1:6,1:4])
  


# We draw simple heat map with large blocks, so you can see
# that a heat map is really a set of rectangles of different
# colors. When the rectangles are made small enough, e.g.
# a few pixels, the heat map appears smooth
x = (1:4)/4
y = (1:8)/4
tab = matrix(NA, nrow=length(x), ncol=length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    tab[i,j] = x[i]*y[j]
  }
}
  
# We set the 4, 1 entry to NA. This will be drawn in white, so you can see how the table entries are translated to the image plot.
# To summarize: The 4,1 entry is drawn at the point given by the 4th x entry and the 1st y entry. 
tab[4,1]=NA
print(tab)
image(x, y, tab, xlab='x', ylab='y') #heat map



# 3. Draw a heat map
#    To run this you need to make joint_prob_table using part 1.

# R has some sophisticated heat map capabilities. Here is a very simple method.
image(a,b,joint_prob_table,xlab='a',ylab='b',
      col=terrain.colors(100)) 
  
# We found the best precision to use by trial and error.
# The reason for printing minp and maxp is to give a sense of the scale in the heat map. It is not as easy as it should be to show a color in basic R.
minp = round(min(joint_prob_table),11)
maxp = round(max(joint_prob_table),6)
s = paste('min =', minp, ', max = ', maxp )
title(s)
  
# Find the values of a and b that give the maximum probability
maxind = which(joint_prob_table == max(joint_prob_table), arr.ind=TRUE)
amax = a[maxind[1]]
bmax = b[maxind[2]]
print(paste("Maximum probability = ", maxp))
print(paste("Maximum probabilty is at a = ", amax, ",  b=", bmax))



# 4. Playing with the colors in a heat map
x = 0:100
y = 0:100
tab = matrix(0, nrow=100, ncol=100)
for (i in 1:100) {
  for (j in 1:100) {
    tab[i,j] = i + 2*j
  }
}
opar = par(mfrow=c(2,2), mar=c(1.8,2.0,1,1),mgp=c(.5,1,0))
image(x, y, tab, col=rainbow(100), xlab='', ylab='')
text(0,40, 'rainbow', pos=4, cex=1.3)
image(x, y, tab, col=heat.colors(10), xlab='', ylab='')
text(0,40, 'heat.colors', pos=4, cex=1.3)
image(x, y, tab, col=topo.colors(100), xlab='', ylab='')
text(0,40, 'topo.colors', pos=4, cex=1.3)
image(x, y, tab, col=terrain.colors(100), xlab='', ylab='')
text(0,40, 'terrain.colors', pos=4, cex=1.3)
par(opar)


# 5. Linear regression example
# We'll now see how to find the best linear function for a given set of (x,y) values.

# We start by creating syntethic data of 10 (x,y) values pairs 
x = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7)
y = c(39, 46, 37, 43, 39, 56, 60, 54, 64, 57)

# We draw a scatter plot of the different (x,y) value pairs. From the plot it seems like there is a linear 
# relationship between the x and y values. When the value of x increases, the value of y tend to increase as well.
plot(x, y, xlab = "x", ylab = "y", col = "red", pch=19, main = "Scatter Plot of (x,y) values.")

# We will now find the 'best' linear function to approximate our data.
# Best means here that it minimizes the sum of squared distances from the (x,y) pairs to the line.
# The lm() function finds that function, before calling the lm() function we first need to arrange the data in a dataframe object.
data = data.frame(x, y)
lm = lm(y ~ x, data)

# The output of the lm function contains all the data about the function.
# For example coef() will return the coefficients of the linear function.
# If the function is x -> ax + b, then the 'intercept' corresponds to the value of b, and the value under 'x' is the slope a.
print(coef(lm))

# We can add linear function to the scatter plot using the abline() function.
abline(lm)

# Finally, suppose we had some hypothesis, say that y = 10*x + 28, we can add that hypothesis to the plot and compare with the output of lm()
curve(10*x + 28, from = 0, to = 200, col = "blue", add = TRUE)
