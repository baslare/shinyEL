hexFunction <- function(df,binwidth,season="2021",radiusFactor,player=TRUE, opp=FALSE, input, col_palette="amber"){
  if(player){
    dataSub <- data %>% filter(season == season)%>% filter(str_detect(name,input))
  }else if(!opp){
    dataSub <- data %>% filter(season == season)%>% filter(str_detect(team,input))
  }else{
    dataSub <- data %>% filter(season == season)%>% filter(str_detect(opp,input))
  }
  
  xbnds = hex_bounds(dataSub$xnew, binwidth)
  xbins = diff(xbnds) /binwidth 
  ybnds = hex_bounds(dataSub$ynew, binwidth)
  ybins = diff(ybnds) /binwidth
  col_palette <- col_palette
  
  
  
  bins <- hexbin(dataSub$xnew,
                 dataSub$ynew,
                 xbins = xbins,
                 xbnds = c(min(dataSub$xnew),max(dataSub$xnew)),
                 ybnds=c(min(dataSub$ynew),max(dataSub$ynew)),
                 shape = ybins/xbins,
                 IDs = T
                 
  )
  
  
  
  
  dataSub$hex_id <- bins@cID
  
  hex_stats<- dataSub %>% group_by(hex_id) %>% summarize(hex_attempts = n())
  
  subFG <- dataSub %>% filter(!miss)
  subFG <- subFG %>% group_by(hex_id) %>% summarize(fgCount = n())
  
  
  
  hex_stats  <- hex_stats %>% left_join(subFG, by="hex_id")
  hex_stats <- hex_stats %>% mutate_if(is.integer, ~replace(., is.na(.), 0))
  hex_stats$fgPct <- hex_stats$fgCount/hex_stats$hex_attempts
  
  sx = bins@xbins / diff(bins@xbnds)
  sy = (bins@xbins * bins@shape) / diff(bins@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers <- hcell2xy(bins)
  
  hex_coords <- bind_rows(lapply(1:bins@ncells, function(i) {
    tibble(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hex_id = bins@cell[i]
    )
  }))
  
  hex_data <- hex_stats %>% inner_join(hex_coords,by="hex_id")
  
  max_hex_attempts <- max(hex_data$hex_attempts)
  
  hex_data = mutate(hex_data,
                    radius_factor = radiusFactor + (1- radiusFactor) * log(hex_attempts) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y))
  
  ggplot(hex_data)+ 
    geom_path(data=features,aes(x,y,group=type),color="white",size=1) + 
    geom_polygon(aes(x=adj_x,
        y=adj_y,
        group=hex_id,fill=fgPct),
        color="transparent",
        size=1,
        alpha=0.9) +
    scale_fill_material(name="FG%",col_palette,labels=c("0%","25%","50%","75%","100%")) + 
    theme(legend.key.height = unit(10,"points"),
          legend.key.width = unit(100,"points"),
          legend.position = "bottom",
          panel.background = element_rect(fill="black"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(hjust=0.5,color="white"),
          plot.title=element_text(hjust = 0.5,color="white"),
          text = element_text(family="Bahnschrift",color="white"),
          plot.background = element_rect(fill="black",color="white",size=1),
          legend.background = element_rect(fill="black"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color="white"))  +
    coord_flip() +
    xlab("") +
    ylab("") + 
    ggtitle("Shot Hexagon Map",input) +
    scale_x_continuous(limits = c(NA,1240)) + 
    labs(caption = "baslare.net/euroleague")
  
  
}

heatFunction <- function(df, player=TRUE, season="2020", input, opp=FALSE, col_palette=c("black","#522149","purple","pink","white"), input_title=input){
  if(player){
    dataSub <- data %>% filter(season == season) %>% filter(str_detect(name,input))
  }else if(!opp){
    dataSub <- data %>% filter(season == season) %>% filter(str_detect(team,input))
  }else{
    dataSub <- data %>% filter(season == season) %>% filter(str_detect(opp,input))
  }
  
  
  ggplot(dataSub)  + 
    stat_density_2d(aes(xnew,ynew,fill=stat(density/max(density))),n=200,geom="raster",interpolate=T, contour = F) + 
    scale_fill_gradientn(name="Shot Density",colors = col_palette,breaks=c(0.25,0.75),labels=c("low","high")) + 
    geom_path(data=features,(aes(x,y,group=type)),color="white",size=1)  + 
    theme(legend.key.height = unit(10,"points"),
          legend.key.width = unit(100,"points"),
          legend.position = "bottom",
          panel.background = element_rect(fill="black"),
          plot.background = element_rect(fill="black",color="white",size=1),
          legend.background = element_rect(fill="black"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color="white"),
          axis.ticks = element_blank(),axis.text = element_blank(),
          panel.grid = element_blank(),plot.subtitle = element_text(color="white",hjust=0.5),
          plot.title=element_text(color="white",hjust = 0.5),
          text = element_text(family="Bahnschrift",color = "white")
          )+
    coord_flip() + xlab("") + ylab("")  +
    ggtitle("Shot Density Chart",
            subtitle = input_title) +
    scale_x_continuous(limits = c(NA,1240)) +
    labs(caption = "baslare.net/euroleague")
}

hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}
