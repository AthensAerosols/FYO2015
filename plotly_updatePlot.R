# load libraries and functions -----
library(plotly)
require(ggplot2)
require(plyr)
require(dplyr)

convert.tz <- function(x, from.tz = 'UTC') {
  as.POSIXct(format(as.POSIXct(paste(x), tz=from.tz), tz=Sys.timezone()))
}

# enter plotly credentials and file paths, etc. -----
Sys.setenv("plotly_username"="username")
Sys.setenv("plotly_api_key"="key")
sample.path <- "/path/" 
update.interval <- 10
plotname <- 'FYO'

# main loop
x <- 1
while (x < 3) {
  
  names  <- list.files(sample.path, pattern = '.txt', full.names = T)
  Sys.sleep(update.interval)
  names2 <- list.files(sample.path, pattern = '.txt', full.names = T)  
  
  if (length(names) != length(names2) | x == 1) {
    
    files <- names2 %>% 
      lapply(FUN = read.csv, skip = 2, head = T) %>% 
      setNames(as.character(strsplit(list.files(path = sample.path,
               pattern = "*.txt"), split = ".txt"))) %>% 
      ldply()
    colnames(files)[1:2] <- c('Group', 'Time')
    files$Time <- convert.tz(files$Time)
    x <- 2
  
    ggplot(data = files[-which(class(files$time) != "POSIXct"),]) + 
      geom_line(aes(x = Time, y = pm_filt, color = Group)) + 
      geom_point(aes(x = Time, y = pm_filt, color = Group), size = 0.1) + 
      ylab("PM (% Full Scale)") + ggtitle("PM Sensor Data") +
      theme_bw()
    ggplotly(filename = paste(plotname))
    print(last_plot())
  }
  
  else { next }

}

