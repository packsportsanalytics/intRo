# intRo
# Sports Analytics Club at NC State
# Authors: Graham Pash, Jason Thompson

##############################################################################
# Some Basics, including data types
##############################################################################

a <- 10 # this is an INTEGER variable
b <- "Wolfpack" # this is a CHARACTER variable (aka a string)
a
print(a)
b
print(b)

vec1 <- 1:5 # this is an (integer) VECTOR
vec2 <- c(1,2,4,5) # another integer vector - the c() function is very useful!
vec3 <- c("NC State","Wolfpack") # this is a character vector
print(vec1)
print(vec2)
print(vec3)

# What if we want to look at just a certain item from a vector?
vec1[3] # print the 3rd item/element from vec1
vec3[1] # print the 1st item/element from vec3
# SINGLE brackets

# Can we mix variable types in a vector? No
mixedvec <- c(1:4,"Wolfpack")
print(mixedvec)
class(mixedvec)

# We CAN mix variable types in a LIST!
testlist1 <- list(vec1,vec3)
testlist1[[1]] # reference list elements using DOUBLE brackets
testlist1[[2]]

# We can even mix object types in a list - lists just collect things
testlist2 <- list(24,c("Basketball","Soccer"))
testlist2[[1]]
testlist2[[2]]

mat <- matrix(1:12,nrow=4,ncol=3) # this is a MATRIX
mat
mat[2,3] # extracts item in 2nd ROW, 3rd COLUMN of the matrix
mat[4,] # extracts the 4th row, ALL columns
mat[,2] # extracts ALL rows, the 2nd column

##############################################################################
# install and load in multiple packages at once using a vector
##############################################################################

packs <- c("tidyverse","rvest")
# lapply(packs,install.packages,character.only = TRUE)
lapply(packs,library,character.only = TRUE)

###############################################################################
# Read in KenPom Data
###############################################################################

kenpom <- read_csv('kenpom_2019-02-11.csv',cols(Conf=col_factor(NULL)),col_names=T)

###############################################################################
# variable types in R + some data wrangling
###############################################################################

class(kenpom$Rk)    # Rank is an INTEGER
class(kenpom$AdjEM) # Adjusted Efficiency is a NUMERIC
class(kenpom$Conf)  # Conference is a FACTOR (because we read it in like this)
class(kenpom$`W-L`) # Record is a CHARACTER... let's fix that

# Separate "W-L" into "W" and "L" columns, make new WinPct column
kenpom %<>% separate('W-L',c("Won","Lost"),sep="-",convert=T) %>% 
  mutate(WinPct=Won/(Won+Lost))

###############################################################################
# baby's first plot!
###############################################################################

# Simple scatter plot of AdjO vs. AdjD - all 353 teams
g <- ggplot(kenpom) + 
  geom_point(aes(AdjO,AdjD))

print(g)

# let's color things by rank so we can see what's going on
# note if you don't write to a variable, the plot just shows up!

ggplot(data=kenpom,aes(x=AdjO,y=AdjD,color=Rk)) + 
  geom_point()

# now let's use a more standard color scale
g <- kenpom %>% ggplot(aes(x=AdjO,y=AdjD,color=Rk)) + 
  geom_point() + 
  scale_color_gradient(low="red",high="blue")

print(g)

g + geom_point() + 
  scale_color_gradient2(low="red",mid="grey",high="blue",midpoint=median(kenpom$Rk))

# you can also add custom labels, arrows, etc to plots!
g + geom_point() + 
  scale_color_gradient2(low="red",mid="grey",high="blue",midpoint=median(kenpom$Rk)) +
  geom_segment(aes(x=112,xend=125,y=118,yend=105),color="black",size=1.5,
               arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_text(label = 'BETTER',x = 125, y = 110,color = 'black') 


##############################################################################
# putting things together, let's only plot the P5 teams
##############################################################################

p5_confs <- c('ACC','B10','SEC','B12','P12')
p5_data <- kenpom %>% filter(Conf %in% p5_confs)

p5 <- ggplot(data=p5_data,aes(x=AdjO,y=AdjD,color=Conf)) + 
  geom_point()
print(p5)

# now let's look at the ACC's dominance!
kenpom %>% mutate(highlight_flag = ifelse(Conf == 'ACC',T,F)) %>%
  ggplot(aes(x=AdjO,y=AdjD)) + 
  geom_point(aes(color=highlight_flag)) + 
  scale_color_manual(values = c('black','red')) + 
  labs(color = "ACC", x = "Adjusted Offense", y = "Adjusted Defense", 
       title = "The ACC is Good", subtitle = "2018-2019 NCAA Men's Basketball",
       caption = "data source: kenpom.com\n author: Sports Analytics Club @ NC State")

###############################################################################
# let's predict Win% from some of our data
###############################################################################

winpct_model <- lm(WinPct ~ AdjEM, data = kenpom)
summary(winpct_model)

# plot our model against the data
preds <- predict(winpct_model)
kenpom <- kenpom %>% mutate('Pred_WinPct'=preds)
ggplot(data = kenpom, aes(x=AdjEM, y=WinPct)) + 
  geom_point(color = 'black') + 
  geom_line(color = 'red',data = kenpom, aes(x=AdjEM,y=Pred_WinPct))

# if we are just exploring different models, and aren't set on one...
ggplot(data = kenpom, aes(x=AdjEM,y=WinPct)) + 
  geom_point(color="black") + 
  geom_smooth(method = 'lm',se = F)

###############################################################################
# let's play with different models
###############################################################################

winpct_model2 <- lm(WinPct ~ AdjO + AdjD, data = kenpom)
summary(winpct_model)

# here's a good idea: who you play matters
winpct_model3 <- lm(WinPct ~ AdjO + AdjD + OppAdjEM, data = kenpom)
summary(winpct_model2)

##############################################################################
# scrape the table from the KenPom website
# then clean the resulting data frame
##############################################################################

url <- "https://kenpom.com/"
url %>% read_html() %>% html_table() -> df
df <- df[[1]] # only want the first (main) table

colnames(df) <- df[1,]
df <- df[-1,]
df <- df[!is.na(as.numeric(df$Rk)),] # drop column name rows
row.names(df) <- 1:nrow(df)

# handle duplicate column names by renaming everything
colnames(df) <- c('Rk','Team','Conf','W-L','AdjEM','AdjO','AdjO_Rk','AdjD','AdjD_Rk',
                  'AdjT','AdjT_Rk','Luck','Luck_Rk','OppAdjEM','OppAdjEM_Rk',
                  'OppO','OppO_Rk','OppD','OppD_Rk','NCAdjEM','NCAdjEM_Rk')

# subset only useful things
df %>% select(c('Rk','Team','Conf','W-L','AdjEM','AdjO','AdjD','AdjT','Luck',
                'OppAdjEM','OppO','OppD','NCAdjEM')) -> df

# save to file
# write.csv(df,file = paste0("kenpom_",Sys.Date(),".csv"),row.names = FALSE)
