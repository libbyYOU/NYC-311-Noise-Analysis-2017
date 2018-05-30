### Homework 1 -- He YOU 8548512389

## Problem 1: General R Questions
# 1. Create a vector that contains numbers (10, 15, 20, 25, . . . , 130). Save it in a varaible called vec.
# By using function sequence to generate a series of numbers from 10 to 130 by 5.
vec=seq(10,130,by=5)

# 2. Use the function length() to find out how many elements there are in vec?
length(vec)
# There are 25 elements in vec.

# 3. What is the value of the 16th element?
vec[16]
# The 16th element is 85.

# 4. Look online for a function that computes the average, median, and standard deviation in R. Use these
#functions to compute the average, median, and standard deviation for vec.
mean(vec)   # The average is 70
median(vec) # The median is 70
sd(vec)     # The Standard deviation is 36.799


# 5. List all the values that are multiples of 10. Save them all in a vector called vec10.
vec10=vec[seq(1,length(vec),by=2)]
# In this vector, the first number is a multiple of 10 and numbers increase by 5. Thus every other number
# from the first one is a multiple of 10.

## Problem 2: Tips in Resturants
# 6. Read the dataset from ???tips.csv???, and save it in a variable called tips.
tips=read.csv("tips.csv")

# 7. Use the function sum() to compute the total tips in the dataset.
sum(tips$tip)
# Sum of total tips in the dataset is 731.58.

# 8. How many females are there in the dataset?
nrow(tips[tips$sex=="F",])
# There are 87 females.

# 9. What is the total tip on a Thursday day?
sum(tips[tips$day=="Thu",]$tip)
# Total tip on a Thursday is 171.83

# 10. How many female smokers are in the tips dataset who dine on Friday?
nrow(tips[tips$sex=="F" & tips$day=="Fri" & tips$smoker=="Yes",])
# The number of female smokers who dine on Friday is 7.
