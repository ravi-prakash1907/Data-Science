data("iris")
View(iris)
index <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7,0.3))
train <- iris[index==1,]
test <- iris[index==2,]

head(train)
library(RColorBrewer)
display.brewer.all()

#VISUALIZATON USING BASIC PLOT FUNCTIONS 

 # 1. scatter plot using plot() function 


plot(x= iris$Sepal.Length, y= iris$Petal.Length, xlab = "Sepal Length ", ylab =  "Petal Length", main = " Petal Length VS Sepal Length", col =  c("red","green ", "blue"))


 # 2. histogram : using hist() function

hist(iris$Petal.Length, col = "blue")

#3. dot plot :

dotchart(iris$Petal.Length ,ylab =  "frequency", xlab = "Petal Length", main= "Petal Length ", color = "blue")

# 4 .  box plot 

boxplot(iris$Petal.Length ~ iris$Species , col = c("red", "green", "blue"))

#5 . barchart 
barplot(iris$Petal.Length, col = c("red", "green","blue"), xlab = "Petal Length" , main =  "Petal Length")

#==================================================================================================================================

#VISUALISATION USING LATTICE PACKAGE FUNCTIONS
#LOADING LATTICE PACKAGE

library(lattice)


#using xyplot() function to produce scatter plot

# scatter plot between sepal.length and petal.length
xyplot(Petal.Length ~ Sepal.Length, data = train, main = " Petal Length VS Sepal Length")

# scatter plot between Sepal width and petal width
xyplot(Sepal.Width ~ Petal.Width, data = train , main =  " Sepal width VS Petal Width")


# color by group
xyplot(Sepal.Length ~ Petal.Length, groups = Species, data =train, auto.key = TRUE, main = " Sepal Length VS Petal Length")
xyplot(Sepal.Width ~ Petal.Width, groups = Species, data =train, auto.key = TRUE, main =  " Sepal width VS Petal Width")
 
#==================================================================================================================================

# now using BASIC 3D SCATTER PLOT

cloud(Sepal.Length ~ Sepal.Length * Petal.Width, groups = Species , data = train , auto.key = TRUE)

#=================================================================================================================================

#using the box plot

# betwwn petal width and sepal width 
bwplot(Petal.Width ~ Sepal.Width , data = train , xlab = "Sepal Width", ylab = "Petal Width" , main = "Petal Width VS sepal Width")
# between petal length and sepal length
bwplot(Petal.Length ~ Sepal.Length , data = train , xlab = "Sepal Length", ylab = "Petal Length" , main = "Petal Length VS sepal Length")

#==================================================================================================================================

#using Density plot

# for sepal length and petal length
densityplot(~ Sepal.Length, data = train, plot.points =  FALSE)
densityplot(~Petal.Length, data = train, plot.points = TRUE)

#plot with multiple groups.
densityplot(~Sepal.Length, group = Species, data = train, plot.points = FALSE, auto.key = TRUE)
densityplot(~Petal.Length, group = Species, data = train, plot.points = FALSE, auto.key = TRUE)

# using densityplot() for sepal width and petal width
densityplot(~Sepal.Width, group = Species, data = train, plot.points = FALSE, auto.key = TRUE)
densityplot(~Petal.Width, group = Species, data = train, plot.points = FALSE, auto.key = TRUE)


#=================================================================================================
                            
                                    # USING HISTOGRAM


# for sepal's length and width
histogram(~Sepal.Length, data = train)
histogram(~Sepal.Length, data = train)


#for petal's length and width
histogram(~Petal.Length, data = train) 
histogram(~Petal.Width, data = train)

#==================================================================================================================================
                       
                                    # USING dOTPLOT
# between sepal.length and petal.length
dotplot(~ Petal.Length, data = train,group =  Species,auto.key =TRUE, main = " Petal Length")

# between sepal.width and petal.width
dotplot(Sepal.Width ~ Petal.Width, data = train,group = Species, auto.key = TRUE, xlab = "SEPAL WIDTH", ylab = "PETAL WIDTH", main =  "Sepal Width VS Petal Width")

#==================================================================================================================================
 
                                     # USING STRIPPLOT

# between sepal length and petal length
stripplot(Sepal.Length ~ Petal.Length , data = train,group= Species, main = "Sepal Length VS Petal length")

#between sepal width and petal width
stripplot(Sepal.Width ~ Petal.Width , data = train , group =  Species ,main = "Sepal Width VS Petal Length")

#==================================================================================================================================

                                     # USING BARCHART
#between sepal length and petal length
barchart( ~ Petal.Length , data = train , auto.key = TRUE,group = Species , main =  " Petal Length")

#between sepal width and Petal width
barchart(Sepal.Width ~ Petal.Width , data = train , group = Species ,auto.key = TRUE, main = "sepal width VS Petal Width")

#==================================================================================================================================

#Now using ggplot2 package

library(ggplot2)

                                      # USING SCATTER PLOT


 
# between sepal Length and petal length

ggplot(data = train, aes(x= Petal.Length, y = Sepal.Length, color = Sepal.Length))  + geom_point() + xlab("Sepal Length ") + ylab("petal Length") + ggtitle("petal Length - Sepal Length") + scale_color_gradientn(colors=rainbow(4)) 
      

                                       #using jitter plot 

ggplot(data = train, aes(x= Petal.Length, y = Sepal.Length, color = Sepal.Length )) + geom_jitter(aes(color = Species))

                                   #showing this with line plot :)

ggplot(data = train, aes(x= Sepal.Length , y = Sepal.Width)) + geom_line(color = "Blue")
ggplot(data =train , aes(x= Petal.Length , y=  Petal.Width)) + geom_line(color = "Blue")
 
                                
                                 # plotting line and scatter plot together 
 
 ggplot(data = train , aes(x= Sepal.Length , y= Sepal.Width , color = Species)) + geom_point() + geom_line(color = "blue")
 ggplot(data = train , aes(x= Petal.Length , y= Petal.Width , color = Species)) + geom_point() + geom_line(color ="blue")

                          
                                          # facet wrap 
 
 ggplot(data = train , aes(Sepal.Length , Sepal.Width, color  =  Species)) +  geom_point() +  facet_wrap(~Species , nrow = 2 , ncol = 2)
 ggplot(data = train , aes(Sepal.Length , Sepal.Width, color  =  Species)) +  geom_point() + geom_smooth(se= FALSE) + facet_wrap(~Species)
    
                                         # facet grid
 
   ggplot(data = train , aes(Sepal.Length , Sepal.Width, color  =  Species)) +  geom_point() +  facet_grid(~Species )
   ggplot(data = train , aes(Sepal.Length , Sepal.Width, color  =  Species)) +  geom_point() +  facet_grid(~Species ) +  geom_smooth(se= FALSE)
                  
                                        #  using geom_area
   
  ggplot(data = train , aes(Sepal.Length ,fill = Species)) +  geom_area( stat = "bin")

        
                                        # using geom_dotplot
   
   ggplot(data = train , aes(Sepal.Length , fill  =  Species)) +  geom_dotplot(binwidth = 0.2)

   
                                        # using histogram 
   
   ggplot(data = train , aes(Petal.Length , fill  =  Species)) + geom_histogram() +  facet_wrap(~Species)  

   
                                    #using geom density  with mean 
    
   ggplot(data = train , aes(Petal.Length ,  fill  =  Species))  + geom_density() + geom_vline(aes(xintercept  = mean(Sepal.Length)) , color = "blue", linetype = "dashed" , size = 1 )
   ggplot(data = train , aes(x=Petal.Length ,  y = Petal.Width ,color =  Species))  + geom_density2d() 
   ggplot(data = train , aes(x=Petal.Length ,  y = Petal.Width )) + geom_bin2d()
                                        
                                              # USING BAR CHARTS 
    
   ggplot(data = train , aes(Petal.Length ,fill = Species )) + geom_bar(stat = "bin") 
   
                                    # using geom_smooth
   
   ggplot(data = train , aes(Sepal.Length , Sepal.Width,fill  =  Species)) + geom_smooth(method = lm)
  
                                        # box plot 
   ggplot(data = train , aes(x = Species , y = Sepal.Length)) + geom_boxplot(aes(fill= Species)) + ylab("Sepal Length") + ggtitle("iris boxplot") + stat_summary(fun.y = mean, geom = "point" ,  shape =5 , size =4)

                                                
#===================================================================================================================================  
#===================================================================================================================================