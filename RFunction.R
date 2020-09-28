library('move')
library('foreach')
library('ggplot2')

rFunction <- function(data, minspeed=NULL)
{
  if (is.null(minspeed)) 
  {
    logger.info("You have not selected a minimum speed. Please change. Return full data set.")
    result <- data
  } else
  {
    logger.info(paste0("You have selected to segment for positions/tracks with speed > ",minspeed,"m/s"))
    
    data.split <- move::split(data)
    segm <- foreach(datai = data.split) %do% {
      print(namesIndiv(datai))
      if (!is.null(minspeed)) datai[speed(datai)>minspeed,] else datai
    }
    names (segm) <- names(data.split)
    
    segm_nozero <- segm[unlist(lapply(segm, length) > 0)] #remove list elements of length 0
    if (length(segm_nozero)==0) 
    {
      logger.info("Your output file contains no positions. Return NULL.")
      result <- NULL
    } else result <- moveStack(segm_nozero) #this gives timestamp error if empty list
  }
  
  #Artefakt, plot speed histogram with cut-off
  hist.tab <- foreach(datai = data.split, .combine=rbind) %do% {
    data.frame("speed"=speed(datai),"id"=namesIndiv(datai))
  }
  
  speed.plot <- ggplot(hist.tab, aes(x = speed, fill = id)) +
    geom_histogram(position = "identity", alpha = 0.2, bins = 100) +
    geom_vline(xintercept = minspeed,lty=2)
  
  pdf("speed_artefakt.pdf")
  print(speed.plot)
  dev.off() 
  logger.info("stored PDF artefact")

  png("speed_artefakt.png")
  print(speed.plot)
  dev.off()
  logger.info("stored PNG artefact")
  
  result
}