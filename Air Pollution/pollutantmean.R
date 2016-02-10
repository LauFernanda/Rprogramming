pollutantmean <- function(directory, pollutant, id = 1:332){
   files<-list.files(directory, full.names= TRUE,pattern="*.csv") ##hace lista de csv en carpeta
  Base_completa<- data.frame()  
  for(i in id){      
      Base_completa <- rbind(Base_completa, read.csv(files[i]))
  }
  if(pollutant== "sulfate"){
    n<-c(2)          
  }
 if (pollutant=="nitrate") {
    n<-c(3)    
  }
 m<-mean(Base_completa[,n],na.rm=TRUE)
 
  print(m)
}
