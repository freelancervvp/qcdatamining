sample <- read.csv("samplesdata.csv")

plot(download_speed ~ network_type, data = sample[sample$download_speed<10000 & 
                                          sample$network_type %in% c("EDGE","HSDPA", "HSPA", "HSPAP","LTE", "UMTS"),])

