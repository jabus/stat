# CfB Stats & R training
# 1. Reading and preparing data for analysis
# 2. Descriptive statistics, univariate statistical analyses (t-test, AOV)

# Jabus Tyerman, jabus@delvbio.com
# last updated: 12 Nov 2018
###################################

#Uncomment the following two lines and run if you need to install tidyverse and car packages.
#install.packages(tidyverse)
#install.packages(car)

library(tidyverse) #Load libraries 
library(car) #for LeveneTest






###Loading data: From .csv to dataframe
# Using file chooser
mydata <- read.csv( file.choose() )

#Hardcoded path + filename
datafile <- "Tier2_plate_data.csv"

mydata <- read.csv(datafile)

head(mydata)
tail(mydata)

str(mydata)

# -->Discussion: Problems with loading data

# ( ASIDE: ---when you're comfortable with read.csv,
#   look up the package 'readr' --it's a 
#   tidyverse approach to loading data in R)




# Two ways to use functions in R
# --reminder: library(tidyverse)

# R 1.0 traditional functions,
#"inside out" or nesting functions

# f( g( h( i(  x ))))
mydata <- head(read.csv(datafile), n=20)

# R 2.0 'tidyverse', left-to-right parsing of functions
# ---dplyr, tidyr, ggplot2 packages

# functions x  %>% i %>% h %>% g %>% f
# x  %>% i(., ...) %>% h(., ...) %>% g(., ...) %>% f(., ...)

mydata <- datafile %>% read.csv %>% head(n=20) 

#mydata <- datafile %>% read.csv(na.strings=c("NA", "-"), header=TRUE) %>% head 
#mydata <- TRUE %>% read.csv(file=datafile, na.strings=c("NA", "-"), header=.) %>% head 

mydata <- datafile %>%
  read.csv(na.strings=c("NA", "-"), header=TRUE)
mydata <- datafile %>%
  read.csv(na.strings=c("NA", "-"), header=TRUE) %>%
  as.tibble
# left assignment, chain that reads left-to-right
# tibbles: 'opinionated' dataframes

#?



# ---> Accessing elements of dataframes: [i, j] and $

# 2.0 approach to filtering/selecting elements of your dataframe
x <- mydata %>%
  filter(strain_id %in% c("s73", "s115")) %>%
  select(strain_id, X1, X3) %>% 
  arrange(desc(strain_id), desc(X3) )  

#?





# Reshaping your data: long and wide dataframes:
# ...from wide to long
mydata.long <- mydata %>%
  mutate(id = 1:n()) %>% #add an identifier column
  gather(key = key, value = value
         , -strain_id, -id)

# why long data? Some functions/statistical
# models use 'long' data, e.g.:
mydata.long %>%
  filter(strain_id %in% c("s73", "s115")) %>%
  ggplot(., aes(x = strain_id, y=value, col=strain_id)) +
  facet_wrap(~key) +
  geom_boxplot() +
  geom_point() + 
  theme_bw() +
  theme(legend.title = element_text(size=18)
        , legend.text = element_text(size=16)
        ,  strip.text = element_text(size=22) 
        , axis.title = element_text(size=20, colour="red")
             , axis.text = element_text(size = 18))


#...and back to wide data
mydata.wide <- mydata.long %>%
  spread(key = key, value = value)
# ...this should recover the shape of 
#   original 'mydata'


#Descriptive statistics: 
#measures of central tendency, 
#measures of dispersion

mydata %>% 
  select(strain_id, X1) %>%
  filter(strain_id %in% c("s115", "s239", "s356", "s470")) %>%
  group_by(strain_id) %>%
  summarise_all(funs(m = mean, med = median
                     , sd = sd, sem = sd/(8^0.5)))

#Boxplots!! 
mydata %>% 
  select(strain_id, X1) %>%
  filter(strain_id %in% c("s115", "s239", "s356", "s470")) %>%
  ggplot(., aes(x=strain_id, y=X1, col=strain_id)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() + 
  theme(legend.title = element_text(size=18)
        , legend.text = element_text(size=16)
        ,  strip.text = element_text(size=22) 
        , axis.title = element_text(size=20, colour="red")
             , axis.text = element_text(size = 18))

#?





#Univariate statistical tests: t-test, ANOVA

#t-test
mydata.t <- mydata %>% 
  select(strain_id, X1) %>%
  filter(strain_id %in% c("s356", "s470"))

mydata.t %>%
  ggplot(., aes(x=strain_id, y=X1, col=strain_id)) +
  geom_boxplot() +
  geom_point()

mydata.a <- mydata %>% 
  select(strain_id, X1) %>%
  filter(strain_id %in% c("s356"))

mydata.b <- mydata %>% 
  select(strain_id, X1) %>%
  filter(strain_id %in% c("s470"))

shapiro.test(mydata.a$X1)
shapiro.test(mydata.b$X1)

# F Test for equality of variance
var.test(mydata.a$X1, mydata.b$X1)

#or, Levene Test for equality of variance
#----> library(car) #Levene in 'car'
leveneTest(X1 ~ strain_id, data=mydata.t)

#if data is normal, parametric t-test
t.test(mydata.a$X1, mydata.b$X1
            , var.equal = TRUE)

#else non-parametric Mann-Whitney U test   
wilcox.test(mydata.a$X1, mydata.b$X1)


 
#ANOVA, multiple group comparisons

mydata.aov <- mydata %>% 
  select(strain_id, X1) %>%
  filter(strain_id %in% c("s115", "s239", "s356", "s470")) 

mydata.aov %>% ggplot(., aes(x=strain_id, y=X1, col=strain_id)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() + 
  theme(legend.title = element_text(size=18)
        , legend.text = element_text(size=16)
        ,  strip.text = element_text(size=22) 
        , axis.title = element_text(size=20, colour="red")
             , axis.text = element_text(size = 18))


result <- aov(X1 ~ strain_id, data = mydata.aov)
summary(result)

TukeyHSD(result)#Tukey multiple comparisons of means

#Homogeneity of variance
leveneTest(X1 ~ strain_id, data=mydata.aov)

#Non-parametric alternative to ANOVA
kruskal.test(X1 ~ strain_id, data = mydata.aov)

