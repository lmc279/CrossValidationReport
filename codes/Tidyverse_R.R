# This is a gentle introduction to R Tidyverse - Dr. Cohen
# *******************************************************

# Tidyverse
install.packages("tidyverse")
library(tidyverse)
# dplyr: manipulating data.frame
# purrr: working with functions
# ggplot2: visualization

# data packages
install.packages("dslabs")
library(dslabs)


# Tidy data
# Each row represents one observation and columns represent variables.
data(relig_income)
data(ChickWeight)

relig_income_tidy = pivot_longer(relig_income,cols = -religion, names_to = "income",values_to = "counts") 


# Working with Data.frame
# add a variable/column 
murders = mutate(murders, rate=total/population*100000)

# Sub setting with filter
filter(murders, rate < 1)


# Select: pick variables
new_table = select(murders, state, region, rate)
filter(new_table, rate <1)


# The pipe operator %>%
# With dplyr we can do a series of operations, for example select and then filter 
# using the pipe operator

murders %>% select(abb,region,rate) %>% filter(rate<0.7)

murders %>% select(region, rate) %>% filter(rate <0.7)

relig_income_tidy %>% select(income)

25 %>% sqrt() %>% log2()
log2(sqrt(25))


## summarizing data
data(heights)

# computes the average and standard deviation for females:

avg_sd = heights %>% filter(sex=="Female") %>%
  summarise(avg=mean(height),sdt=sd(height))

avg_sd


heights %>% 
  filter(sex == "Female") %>%
  summarize(rg = quantile(height, c(0, 0.5, 1)),avg=mean(height),sdt=sd(height))


# Let's compute the murder rate of the USA
# Recall that the USA murder rate is not the average of the state murder rates

avg_state_rates = murders %>% summarise(mean(rate)) %>% pull()

US.murder.rate = murders %>% summarise(rate=sum(total)/sum(population)*100000)

# Looking at the results they are data.frame even if it is just one number
murders %>% summarise(rate=sum(total)/sum(population)*100000) %>%
  pull(rate)


## Data Grouping
heights %>% group_by(sex)
# tibble: many tables same columns but not necessarily the same number of rows

heights %>% group_by(sex) %>% summarise(avg= mean(height),std=sd(height))

# let's compute the median murder rate in the four regions of the country:
murders %>% group_by(region) %>% summarise(median.rate=median(rate))  


## Arrange data frame 
murders %>% arrange(rate)

murders %>% 
  arrange(population) %>%
  head()

murders %>% top_n(10)


## purrr : apply functions
# apply the same function to each element of a vector 
compute_s_n=function(n){
  sum((1:n))
}

s_n = map_dbl(c(3,5,10,15,200), compute_s_n) 

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data= .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# This is not going to work because the output of group_by is a tibble
mtcars %>% group_by(cyl) %>% map(~ lm(mpg ~ wt, data=.x))

## ************** ggplot2 *******************
# data visualization
library(ggthemes)
library(ggrepel)

ggplot(murders)

murders %>% ggplot()


ggplot1 = murders %>% ggplot(mapping = aes(x=population/10^6, y=total)) 
ggplot1

ggplot1 + geom_point(aes(col=region), size = 3) +
  geom_text_repel(aes(label=abb))+
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(formula = "y~x", method = lm)+
  xlab("Populations in millions (log10 scale)") + 
  ylab("Total number of murders (log10 scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
      theme_dark()


  
  # ggplot2 package 
  library(ggplot2)
  library(ggthemes)
  library(ggrepel)
  library(ggcorrplot)
  
  
  # ggplot works with data frames and not individual vectors. 
  # You can keep enhancing the plot by adding more layers (and themes) to 
  # an existing plot created using the ggplot() function
  
  data(midwest)
  data(mpg)
  
  
  gg1 = ggplot(mpg,mapping = aes(x=cty,y=hwy)) +
    geom_point(aes(col=cyl)) +
    geom_hline(yintercept = mean(mpg$hwy),col="red")+
    geom_vline(xintercept = mean(mpg$cty),col="red")+
    geom_smooth(method = "lm",se = F)
  
  gg1
  
  
  # Scatterplot
  gg = ggplot(midwest, mapping = aes(x=area, y=poptotal))+ 
    geom_point() +
    theme_bw()
  gg
  
  # this is basic plot in R
  plot(poptotal~area,data = midwest)
  gg
  
  
  gg + geom_point(aes(col=state, size=popdensity)) + 
    geom_smooth(method="lm", se=FALSE) + 
    xlim(c(0, 0.1))  + 
    ylim(c(0, 500000)) + 
    labs(subtitle="Scatterplot", 
         y="Population", 
         x="Area", 
         title="Area vs. Population", 
         caption = "Source: midwest dataset this my dataset")
  
  
  # Counts Chart
  g <- ggplot(mpg, aes(cty, hwy))
  
  g + geom_count(col="tomato3", show.legend=F) +
    labs(subtitle="City vs highway mileage", 
         y="hwy miles per gallon", 
         x="cty miles per gallon", 
         title="Counts Plot",
         caption = "Source: mpg")
  
  
  # Correlations
  library(ggcorrplot)
  data(mtcars)
  cr <- round(cor(mtcars), 1)
  ggcorrplot(cr, 
             type = "upper", 
             lab = TRUE, 
             lab_size = 5.5, 
             method="square", 
             colors = c("red", "white", "green"), 
             title="Correlogram of mtcars",
             ggtheme = theme_light())
  
  # Histograms
  
  # Continuous 
  g <- ggplot(mpg, aes(displ)) +
    scale_fill_brewer(palette = "Spectral")
  
  g + geom_histogram(aes(fill=class),
                     binwidth = 0.5, 
                     col="black") +
    labs(title="Histogram with Auto Binning", 
         subtitle="Engine Displacement across Vehicle Classes")  
  
  g + geom_histogram(aes(fill=class), 
                     bins=15,  # change number of bins
                     col="black", 
                     size=2) +  
    labs(title="Histogram with Fixed Bins", 
         subtitle="Engine Displacement across Vehicle Classes")
  
  
  # Plot density 
  g <- ggplot(mpg, aes(cty))
  g
  g + geom_density(aes(fill=factor(cyl)),alpha=0.3) + 
    labs(title="Density plot", 
         subtitle="City Mileage Grouped by Number of cylinders",
         caption="Source: mpg",
         x="City Mileage",
         fill="# Cylinders")
  

  
  
  
  