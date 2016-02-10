corr <- function(directory, threshold = 0) {
  files<-list.files(directory, full.names=TRUE,pattern="*.csv") ##hace lista de csv en carpeta
  tabla_nobs<-complete(directory)
  nobs<-tabla_nobs[,2]
  resultado<-vector()
  as.numeric(resultado)
  for(i in 1:332){
        if (nobs[i]>threshold){
      data<-read.csv(files[i])
      iscomplete<-complete.cases(data)
      data_complete<-data[iscomplete,]
      sulfate<- data_complete[,2]
      nitrate<- data_complete[,3]
      correlation<-cor(sulfate,nitrate)
      resultado<-c(resultado,correlation)
    }
    
      }
  print(resultado)
}
