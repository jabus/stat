# This is my first R script
# and you can add comments to your script using #
# R console ignores comments
################################

#displaying text to the screen
print("This is my first script")

#assignment statements
a <- 2+3
print(a)

#a simple plot
x <- c(-10:10)
y <- x^2
df <- as.data.frame(cbind(x,y)) #column bind two (or more vectors)

#Uncomment this line to "manually" edit my dataframe
#df$x[3] <- 3 #make changes to your data explicit (do not use the "Data Editor")

plot(x=df$x, y=df$y
	, pch=17, col="red", main="My first plot")

#for more details on contolling base R plots, look up "par"

#Functions in R
#...

write.table(x=df
	, file="C:\\Users\\jabus\\OneDrive\\Documents\\2459\\Delv Bio\\Clients\\Novo Nordisk UCSD\\stats course\\dev\\my_first_script_data.csv"
	, sep = ","
	, row.names=FALSE
	, col.names=TRUE)


### Extending R's base functionality: packages (a.k.a libraries)
install.packages("ggplot2") 
#Packages are maintained by individual contributors, in a central repository (CRAN)
#Every time you update your R version, you need to update (install) your R packages
#Collect your favorite packages in a script, and run

library("ggplot2") #this makes the functions defined in this library available to the current script.

### Getting help
help(str)
?str

#If you ask for help on forums, e.g. Stack Overflow, you should provide information about your R session:
sessionInfo()
