if (require(raster)& require(sf) & require(stringr)) {
  reproject.CRDX<- function(x){
    rotated.NP.coords<- data.frame(c("SAM", "CAM", "NAM", "EUR", "AFR", "WAS", "EAS", "CAS", "AUS", "ANT", "ARC", "MED", "MNA",  "SEA"),
                                   c( "-56.06" , "113.98",  "83","-162" , NA,"-123.34", "-64.7799987792969", "-103.39", "141.38",  "-166.92", "0", "198", NA, NA),
                                   c(70.60, 75.74, 42.50, 39.25, 90.00, 79.95,77.6100006103516, 43.48, 60.31,  6.08,  6.55, 39.25, 90.00,  90.00))
    
    
    if(class(x)=="character" & length(x)< 2 & strsplit(x, '[.]')[[1]][2]== "nc") {
      
      if (sum(strsplit(x, '[_]')[[1]][6]==c("MPI-CSC-REMO2009", "CLMcom-CCLM5-0-6", "SMHI-RCA4", "CLMcom-CCLM5-0-2","CLMcom-CCLM4-8-17", "SMHI-RCA4-SN"))==1){
        
        resolution<-as.numeric(str_sub(strsplit(strsplit(x, '[_]')[[1]][2], "[-]")[[1]][2], start=1, end=2))/100
        
        if (sum(resolution==c(0.11,0.22,0.25,0.44))==1 & nchar(str_sub(strsplit(strsplit(x1, '[_]')[[1]][2], "[-]")[[1]][2]))==2) {
          
          res=c(resolution,resolution)
          nc.domain<- which(rotated.NP.coords[,1]== strsplit(strsplit(x1, '[_]')[[1]][2], "[-]")[[1]][1])
          
          if (is.na( rotated.NP.coords[nc.domain,2])==TRUE){
            
            crs1<- paste0("+proj=ob_tran +o_proj=longlat +o_lat_p=",rotated.NP.coords[nc.domain,3],
                          " +lon_0=0 +ellps=sphere")
            
          }else if (is.na( rotated.NP.coords[nc.domain,2])==FALSE) {
            
            crs1<- paste0("+proj=ob_tran +o_proj=longlat +o_lat_p=", rotated.NP.coords[nc.domain,3], 
                          " +o_lon_p=",  rotated.NP.coords[nc.domain,2]," +lon_0=180 +ellps=sphere")
            
          } else {
            print(paste0("Error: Unknown CORDEX Domain: ", strsplit(strsplit(x, '[_]')[[1]][2], "[-]")[[1]][1]))
            return()
          }
          
          ras<- stack(x, varname=strsplit(x, '[_]')[[1]][1])
          nams<- names(ras)
          raster::crs(ras) <-st_crs(4326)$proj4string
          projected.raster = projectRaster(ras, 
                                           crs =crs1,
                                           res=res)
          ras.output= flip(flip(t(projected.raster), direction = "y"), direction = "x")
          names(ras.output)<-nams
          return(ras.output)
          
        } else {
          print(paste0("Error: Unknown resolution: ", str_sub(strsplit(strsplit(x1, '[_]')[[1]][2], "[-]")[[1]][2])))
          return()
        }
        
      }  else {
        print(paste0("Error: Can not reproject RCM :", strsplit(x, '[_]')[[1]][6]))
        return()
      }
    } else {
      print("Error: Requires a single file of netcdf")
      return()
    }
  }
}
