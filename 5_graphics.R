# Graphics - ggplot2
# Jabus Tyerman 11/20/2018
# jabus@delvbio.com

#1 ggplot2: Grammar of graphics
#   prepare data, set aes, adds layers
#2. Print to file
#     -use global variable to toggle printing 
#     figures to files (or screen)
#3. Faceting, aka trellis or panels
#4. Coord_flip for barcharts
#     -customized ordering of X factors

#Load packages
library(tidyverse) #specifically for dplylr, tidyr, ggplot2)

#script parameters
MAKE_FIG <- FALSE #T = write pdf to file, F  = display on screen




#Load data
datafile <- "joined_data.csv"
mydata <- datafile %>% read.csv %>% as.tibble

#1. GG
p <- ggplot(data = mydata, aes(x=X1, y=X2)) + 
  geom_point(aes(col=strain_id, size = Y)) +
#  geom_smooth(method="loess") +
  labs(
   title = "X2 vs X1, size proportionate to Yield"
   ,x = "X1 = OD"
  , y = "X2 = Titer"
  ) +
  theme(legend.position = "none"
        , title = element_text(size=30)
        , axis.title = element_text(size=20, colour="red")
          , axis.text = element_text(size = 18))

p


#2. saving figures to file (pdf, png)
pdf(file = "my_fig.pdf")
print (p)
dev.off()

#Use global variable MAKE_FIG as a boolean 
#flag for outputting figures to file (TRUE)
#or to screen (FALSE)

if (MAKE_FIG) pdf("my_fig.pdf")
print (p)
if (MAKE_FIG) dev.off()

if (MAKE_FIG) png("my_fig.png")
print (p)
if (MAKE_FIG) dev.off()

#3. Faceting
        #aka Multipanel figures
#(and 4. coord_flip)

mydata$treatment <- NA
mydata$treatment <- Map(function(x)
                               {ifelse(x=="s109"
                                       , "Control"
                                       , "Experiment")}
                        , mydata$strain_id) %>%
  unlist

my_favorite_strains <- c("s109", "s356", "s323", "s359")

mydata.2 <- mydata %>%
  filter(strain_id %in% my_favorite_strains) %>%
  select(-X5) 

#this line is key for re-ordering:
mydata.2$strain_id <- factor(mydata.2$strain_id
      , levels=mydata.2$strain_id[order(-mydata.2$Y)])


mydata.2 <- mydata.2 %>%  gather(key=key, value=value
         , -strain_id, -treatment)


p <- ggplot(data = mydata.2
            , aes(x=strain_id, y=value, fill=treatment)) +
  facet_wrap(~key, ncol=2) +
  coord_flip() +
  geom_bar(stat="identity") + 
  labs(
    x="Strain ID"
    , y="Value"
  ) +
  theme_bw() + 
  theme(legend.title = element_text(size=20, colour="red")
          , legend.text = element_text(size=16)
        ,  strip.text = element_text(size=22) 
        , axis.title = element_text(size=20, colour="red")
        , axis.text.x = element_text(size = 18
                                     , angle=90
                                     , vjust = 0.5)
        , axis.text.y = element_text(size = 18))
p

 




# Extra commands:
#graphics.off()

