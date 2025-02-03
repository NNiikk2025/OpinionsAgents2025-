rm(list=ls(all=TRUE))
#Function for program
#What is the opinion of agents with coordinats (i,j)
haveOpinion <- function(i,j, threshold)
{
  #Number of agents the same opinion
  tmp <- DA[max(1,i-1):min(nc,i+1),max(1,j-1):min(nr,j+1)]
  if (use_empty)
    s <- sum(tmp==DA[i,j] | tmp==1)-1
  else
    s <- sum(tmp==DA[i,j])-1
  #Определение настроения с учётом расположения дома на границе города
  if ((i==1 || i==nc) && (j==1 || j==nr)) #угловой дом
    s > min(threshold,3)
  else if (i==1 || i==nc || j==1 || j==nr) #дом на границе
    s > min(threshold,5)
  else
    s > threshold
} 
  #Генерация выборки размером n целочисленной случайной величины,
  #значения которой равномерно распределены на множестве {1,2,...,r}
  rdunif <- function(n,r) round(runif(n)*r+0.5)
 
  
  
 
  # agents domain task (rectangele)
  nr <- 3 #nr- number of rows
  nc <- 3 #nc- number of colums
  thresholdSaveOp <- 2 # Number of agents neighbors – save Opinion
  use_empty <- FALSE # Should agents without opinions be considered 
  # Color for drawing border
  borderstyle <- 'grey'
  # Drawing without borders 
  #borderstyle <- FALSE
  K <- 3 #Nimber of agents Opinions The first - without opinions
  probWithoutOpinions <- 0.1 #Prob for agents without opinions
  prob <- rep((1- probWithoutOpinions)/K,K) #Prob Distribution for agents
  #Color for different agents
  #First color for agent without opinion
  color <- c(rgb(1,1,1),
             rgb(1,0,0),rgb(0,1,0),rgb(0,0,1),
             rgb(1,1,0),rgb(1,0,1),rgb(0,1,1),
             rgb(.5,0,0),rgb(0,.5,0),rgb(0,0,.5),
             rgb(.5,.5,0),rgb(.5,0,.5),rgb(0,.5,.5))
  #Initial distribution of agents with opinion (uniform, normal, Poisson)
  DA <- matrix(runif(nr*nc),nrow=nr,ncol=nc)
  #DA <- matrix(rnorm(nr*nc, mean=0.5, sd = 0.5),nrow=nr,ncol=nc)
  #Assign  Opinion to agents (K)
  for (i in 1:length(prob)) DA[DA<=sum(prob[1:i])] <- i+1
  #Agents without Opinion
  DA[DA<1] <- 1
  #List of such agents (i,j)
  empty <- c()
  for (i in 1:nr)
    for (j in 1:nc)
      if (DA[i,j]==1) empty <- c(empty,i,j)
  #Initial configuration od agents
  plot(c(0,nc),c(0,nr),type='n',xlab='x ',ylab=' y',axes=FALSE)
  for (i in 1:nc)
    for (j in 1:nr)
      rect(i-1,j-1,i,j,
           col=color[DA[i,j]],border=borderstyle)
  counter <- 0 #Iteration counter
  while (counter < 3) #while (TRUE)
  {
    counter <- counter+1
    #Agents without opinion
    withoutOpinion <- c()
    for (i in 1:nc)
      for (j in 1:nr)
      {
    if (DA[i,j]==1 || haveOpinion(i,j,thresholdSaveOp)) next
        withoutOpinion <- c(withoutOpinion,i,j)
      }
    if (length(withoutOpinion)==0)
    {
      print('Sutiation don’t change')
      #  break
    }
    #Agents without Opinion 
    #next iteration step
    for (k in sample(seq(from=1,to=length(withoutOpinion),by=2)))
    {
      n <- rdunif(1,length(empty)/2)*2
      i <- withoutOpinion[k]
      j <- withoutOpinion[k+1]
      DA[empty[n-1],empty[n]] <- DA[i,j]
      rect(empty[n-1]-1,empty[n]-1,empty[n-1],empty[n],
           col=color[DA[i,j]],border=borderstyle)
      DA[i,j] <- 1
      if (length(empty)>2)
        empty <- empty[(1-n):(-n)]
      else
        empty <- c()
      empty <- c(empty,i,j)
      rect(i-1,j-1,i,j,col=color[1],border=borderstyle)
    }
    Sys.sleep(2.0)
    cat(c('Iteration number >',counter,'\n'))
}
cat(c('All iteration ',counter,'\n'))


