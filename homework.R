
library(ggplot2)

library(plotly)

# highway
graph1 <- plot_ly( mpg_sample, x= ~cyl, y=~hwy, type= 'box', name="On Highway")%>% layout( yaxis=list(range=c(5,45) , domain=c(0,0.85)), xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

#city
graph2<- plot_ly( mpg_sample, x= ~cyl, y=~cty, type= 'box', name="On city")%>% layout( yaxis=list(range=c(5,45) , domain=c(0,0.85)), xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

#plot both graph.
subplot(graph1,graph2)%>% layout( xaxis=list(title="(cylinder)"), yaxis=list(title="(mpg)"))




  
  ## 2.2 Relationships between displ (engine) and mileage (on highway and city)


# relation between displ and hwy mileage 
# regression line
rl <- lm(hwy ~ displ, data = mpg_sample)
displ_hwy<- plot_ly(mpg_sample, x=~displ,y=~hwy, name="On hwy") %>% add_markers(y=~hwy)%>% add_lines(x=~displ, y= fitted(rl))


# regression line
rl <- lm(cty ~ displ, data = mpg_sample,name="")

# relation between displ and cty mileage
displ_cty <- plot_ly(mpg_sample, x = ~displ, y = ~cty,name="On city") %>% add_markers(y=~cty)%>% add_lines(x=~displ, y= fitted(rl))

#plot both grah.
subplot(displ_hwy, displ_cty)%>% layout( xaxis=list(title="(displ)"), yaxis=list(title="(mpg)"))



    # highway
  qplot(displ,hwy, data =mpg_sample, color=drv)
  
  
  # summary
  with(mpg_sample, tapply(hwy,drv, summary))
  
  
  
  # city
  qplot(displ,cty, data =mpg_sample, color=drv)
  
  #summary
  with(mpg_sample, tapply(cty,drv, summary))

 
 


   # tranmission type we have are 
  table(mpg_sample$trans)

      # to hold 2 category "automatic" and "manul"
      trans2<-c();
      
      # loop over the trans 

      for(t in mpg_sample$trans){
        # here we are using gsub() to replace (w+) in automatic(s6) and make just "automatic" and append to tr
        trans2 <- c( trans2 , gsub ('\\(\\w+\\)', "", t) )
        
      }

      # now append new Variable "trans2" to mpg_sample

      mpg_sample$trans2<-as.factor(trans2)
      
      head(mpg_sample[,c("trans","trans2")])
      
      table(mpg_sample$trans2)


###  2.4.1 Are Vehicles with 'auto' transmission more fuel efficient than 'manual' ?




# highway mileage on both 'auto' and 'manual
fig1 <- plot_ly( mpg_sample, x= ~trans2, y=~hwy, type= 'box',boxpoints="all", jitter="0.01", name="On Highway")%>% layout( yaxis=list(range=c(5,45) , domain=c(0,0.85)), xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))



#city mileage on both 'auto' and 'manual'

fig2 <- plot_ly( mpg_sample, x= ~trans2, y=~cty, type= 'box',boxpoints="all", jitter="0.01", name="On City")%>% layout( tiltle="City mileage (auto vs manual)", yaxis=list( range=c(5,45) ,domain=c(0,0.85)) , xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))


# plot both graph
fig<- subplot(fig1,fig2) %>% plotly::layout(title="Highway vs City ", legend=list(x=0.028, y=1.038),margin=list(l=100,r=20,t=70,b=70), paper_bgcolor = 'rgb(248, 248, 255)',plot_bgcolor = 'rgb(248, 248, 255)',xaxis=list(title="(trans)"), yaxis=list(title="(mpg)") )

fig




ggplot(mpg_sample, aes(displ, hwy))+ geom_point(alpha=1/3) + facet_wrap(trans2~drv, nrow=2, ncol=3)+ geom_smooth(method="lm", se=FALSE, col="steelblue")+
  labs(x="displ", y="hwy", title="Transmission")










### the box plot

  
  
  ### The first thing we wanna do is seperate "auto" and "manual" transmission vehicle from dataset.
  
  
  
  
  
  
  
  



# manual transmission

mpg_sample.manual <- subset(mpg_sample, trans2== "manual");


# automatic transmission 

mpg_sample.auto <- subset(mpg_sample , trans2 == "auto");


## now we have both transmission data

## we will look  the state of f,4,r in each vechile with respective transmission to have clear picture





## 2.4.2 Performance (mileage) of (4,f,r) on 'auto' transmission

#on highway
fig1 <- plot_ly( mpg_sample.auto, x= ~drv, y=~hwy, type= 'box', name="On Highway")%>% layout( yaxis=list(range=c(5,45) , domain=c(0,0.85)), xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

#on city
fig2 <- plot_ly( mpg_sample.auto, x= ~drv, y=~cty, type= 'box',  name="On City")%>% layout( tiltle="City mileage of Vechile with 'Auto' transmission ", yaxis=list( range=c(5,45) ,domain=c(0,0.85)) , xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

# plot both graph
fig<- subplot(fig1,fig2) %>% layout(title="performance of (4,f,r) vechile on 'auto' transmission", legend=list(x=0.028, y=1.038),margin=list(l=100,r=20,t=70,b=70), paper_bgcolor = 'rgb(248, 248, 255)',plot_bgcolor = 'rgb(248, 248, 255)',yaxis=list(title="(mpg)"), xaxis=list(title=" Drive type "))

fig



# fin the sumary of (4,f,r) in 'auto' (hwy,city)

#on highway 

with(mpg_sample.auto, tapply(hwy, drv, summary))

#on city

with(mpg_sample.auto, tapply(cty, drv, summary))





  ### 2.4.2 Performance (mileage) of (4,r,f) drive on 'manual' tramission
  

# on highway
fig1 <- plot_ly( mpg_sample.manual, x= ~drv, y=~hwy, type= 'box', name="On Highway")%>% layout( yaxis=list(range=c(5,45) , domain=c(0,0.85)), xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

#on city
fig2 <- plot_ly( mpg_sample.manual, x= ~drv, y=~cty, type= 'box', name="On City")%>% layout(  yaxis=list( range=c(5,45) ,domain=c(0,0.85)) , xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

# plot both graph
fig<- subplot(fig1,fig2) %>% layout(title="Performance of (4,f,r) vechile on 'manual' transmission", legend=list(x=0.028, y=1.038),margin=list(l=100,r=20,t=70,b=70), paper_bgcolor = 'rgb(248, 248, 255)',plot_bgcolor = 'rgb(248, 248, 255)',yaxis=list(title="(mpg)"), xaxis=list(title=" Drive type ") )

fig



  
  ## 2.5.1 performance base on cylinder on 'auto' and 'manual' mode;
  
  
  




# on auto mode

fig1<- plot_ly(mpg_sample.auto, x=~cyl, y=~hwy, type="box",boxpoints="all", jitter="0.001", name="On Highway")%>% layout( yaxis=list(range=c(5,45) , domain=c(0,0.85)), xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

fig2<- plot_ly(mpg_sample.auto, x=~cyl, y=~cty,type='box', name="On City")%>% layout(  yaxis=list( range=c(5,45) ,domain=c(0,0.85)) , xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

fig<-subplot(fig1,fig2)%>% layout(yaxis=list(title="(mpg)"), xaxis=list(title=" cylinder "))

fig



## 2.5.6 performance of (4,5,6,8) cycl engine on 'manual' mode



fig1<- plot_ly(mpg_sample.manual, x=~cyl, y=~hwy, type="box",boxpoints="all", jitter="0.001", name="On Highway")%>% layout( yaxis=list(range=c(5,45) , domain=c(0,0.85)), xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

fig2<- plot_ly(mpg_sample.manual, x=~cyl, y=~cty,type='box',boxpoints="all", jitter="0.01", name="On City")%>% layout(  yaxis=list( range=c(5,45) ,domain=c(0,0.85)) , xaxis=list( title="Drive type ", showline=TRUE, showticklabels=TRUE, showgrid=TRUE))

fig<-subplot(fig1,fig2)%>% layout( legend=list(x=0.029, y=1.038, font=list(size=10)),margin=list(l=100,r=20,t=70,b=70), paper_bgcolor = 'rgb(248, 248, 255)',plot_bgcolor = 'rgb(248, 248, 255)',yaxis=list(title="(mpg)"), xaxis=list(title=" cylinder ")) 

fig









