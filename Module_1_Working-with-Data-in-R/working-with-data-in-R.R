# This is a comment
iris <- iris
View(iris)

View(x = iris)

#Extract a single column
iris$Sepal.Length

#Save column as a separate variable
sep_len <- iris$Sepal.Length

#Run a function on a single column in df
mean(x = iris$Sepal.Length)

#Run a function on the 'sep_len' object
mean(x = sep_len) #Output should be identical to output from line 14

install.packages("tidyverse")
library(tidyverse)

#Use filter function in dplyr
# Base R command: iris[iris$Sepal.Length <5, ]
filter(iris, Sepal.Length < 5)

#Save the filtered output as a new variable
filtered_iris <- filter(iris, Sepal.Length < 5)

#Filter the iris dataset
# Base R command: iris[iris$Species == "setosa", ]
filter(iris, Species == "setosa")

#Filter iris dataset for the 'setosa' species and a 'Sepal.Length' less than 5
# Base R command: iris[iris$Species == "setosa", ]
filter(iris, Species == "setosa" & Sepal.Length < 5)


#Use the mutate() function
# Base R command: iris$new_data1 <- iris$Sepal.Length*2
mutate(iris, new_data1 = Sepal.Length *2)

#Add new columns 
mutate(iris, new_data1 = Sepal.Length *2,
       new_data2 = Sepal.Length/Sepal.Width) 


#Use the select() function
# Base R command: iris$Species
select(iris, Species) #selects the Species column and maintains the data structure


#Calculate the mean of Sepal.Length.
# Base R command: mean(iris$Sepal.Length)
summarise(iris, mean = mean(Sepal.Length))

#Calculate the mean, standard deviation and variance of Sepal.Length simultaneously
# Base R command: mean(iris$Sepal.Length); sd(iris$Sepal.Length); var(iris$Sepal.Length)
summarise(iris, mean = mean(Sepal.Length),
          sd = sd(Sepal.Length),
          variance = var(Sepal.Length)) #calculates the mean, standard deviation and variance of Sepal.Length and assigns them to a specified name for easy identification


#Use the arrange() function
# arrange(iris, Sepal.Length). This does the same thing as below
iris %>% arrange(Sepal.Length) # NOTE: ascending is default

#Arrange data in descending order
iris %>% arrange(desc(Sepal.Length)) # does the same thing as above but in descending order of Sepal.Length.

