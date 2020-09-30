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
    data.frame("speed"=speed(datai),"id"=namesIndiv(datai), stringsAsFactors = FALSE)
  }
  logger.info(paste0('before: ', class(hist.tab$speed)))
  hist.tab$speed <- as.numeric(hist.tab$speed)
  logger.info(paste0('x after: ', class(hist.tab$speed)))
  # logger.info(paste0('y before: ', class(hist.tab$id)))
  # hist.tab$id <- as.numeric(hist.tab$id)
  # logger.info(paste0('y after: ', class(hist.tab$id)))
  speed.plot <- ggplot(hist.tab, aes(x = speed, fill = id)) +
    geom_histogram(position = "identity", alpha = 0.2, bins = 100) +
    geom_vline(xintercept = minspeed,lty=2) +
    scale_x_discrete() +
    scale_y_discrete()
  
  pdf(paste0(Sys.getenv(x = "ARTEFACTS_DIR", "/tmp/"), "speed_artefakt.pdf"))
  print(speed.plot)
  dev.off() 
  logger.info("stored PDF artefact")

  png(paste0(Sys.getenv(x = "ARTEFACTS_DIR", "/tmp/"), "speed_artefakt.pdf"))
  print(speed.plot)
  dev.off()
  logger.info("stored PNG artefact")
  
  result
}