complete <- function(directory, id = 1:332) {
  files<-list.files(directory, full.names= TRUE,pattern="*.csv") ##hace lista de csv en carpeta
  observaciones<- data.frame()
  observacion <- vector()
  for(i in id){
       data <- read.csv(files[i])
       iscomplete<-complete.cases(data)
       data_complete<-data[iscomplete,]
       nobs<-nrow(data_complete)
       observacion<-c(i,nobs)
       observaciones<- rbind(observaciones,observacion)
  }
  names(observaciones) <- c("id","nobs")
  observaciones
}
