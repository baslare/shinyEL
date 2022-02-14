require(tidyverse)

inp <- seq(1:676) - 1
outp <- sqrt(675*675 - inp*inp)
arc <- data.frame(cbind(x=inp,y=outp))
inp2 <- inp
outp2 <- -outp
arc <- rbind(arc,data.frame(x=inp2,y=outp2))
inp3 <- seq(1:300) - 1 - 157
outp3 <- rep(660,300)
arc <- rbind(arc,data.frame(x=inp3,y=outp3))
arc <- rbind(arc,data.frame(x=inp3,y=-outp3))
arc <- arc %>% arrange(desc(y))
arc <- arc %>% filter(y > -661 & y <661)

arc2x <- seq(1:181) - 1
arc2y <- sqrt(180*180 - arc2x*arc2x)
arc2 <- data.frame(cbind(x=(422 + arc2x),y=arc2y))
arc2 <- rbind(arc2,data.frame(x=(422+arc2x),y=-arc2y))
arc2 <- arc2 %>% arrange(desc(y))


ggplot(hex_data) + geom_polygon(aes(x=adj_x,y=adj_y,group=hex_id,fill=fgPct),color="white")+ 
  scale_fill_viridis(option = "inferno") + 
  theme(panel.background = element_rect(fill="black"),panel.grid = element_blank())  +
  geom_path(data=arc, aes(x=x,y=y),color="white") + coord_flip() + 
  geom_path(data=box2,aes(x=x,y=y),color="white") + 
  geom_path(data=arc2,aes(x=x,y=y),color="white") +
  geom_path(data=side,aes(x,y), color="white")


bxx <- seq(1:(581))- 158
byy <- rep(245,length(bxx))
box2 <- data.frame(cbind(x=bxx,y=byy))
box2 <- rbind(box2, data.frame(x=bxx,y=-byy))
bxx2 <- rep(422,491)
byy2 <- seq(1:491) - 246
box2 <- rbind(box2, data.frame(x=bxx2,y=-byy2))
box2 <- box2 %>% arrange(desc(y))

sidex <- seq(1:1400) - 159
sidey <- rep(-750,1400)
side <- data.frame(x=sidex,y=sidey)
side <- rbind(side, data.frame(x=sidex,y=-sidey))
side <- rbind(side, data.frame(x=rep(-158,1500),y=(seq(1:1500)-751)))
side <- side %>% arrange(desc(y))


arc3x <- seq(1:123) - 1
arc3y <- sqrt(122*122 - arc3x*arc3x)
arc3 <- data.frame(cbind(x=(arc3x),y=arc3y))
arc3 <- rbind(arc3,data.frame(x=(arc3x),y=-arc3y))
arc3 <- arc3 %>% arrange(desc(y))

boardx <- rep(122,181) -159
boardy <- seq(1:181) -91
board <- data.frame(x=boardx,y=boardy)

rimx <- seq(1:24) - 1
rimy <- sqrt(23*23 - rimx*rimx)
rim <- data.frame(cbind(x=(rimx),y=rimy))
rim <- rbind(rim,data.frame(x=(rimx),y=-rimy))
rim <- rim %>% arrange(desc(y))
rim2 <- data.frame(x=-1*(rim$x),y=rim$y)
comps <- list()
comps <- list(arc=arc,arc2=arc2,box=box2,side=side,arc3=arc3,board=board,rim=rim,rim2=rim2)
nams <- names(comps)
comp <- Map(function(x,y) x %>% mutate(type=y), x= comps, y=nams)
comp <- list()
for(i in 1:length(comps)){
  comp[[i]] <- comps[[i]] %>%  mutate(type=rep(nams[i],dim(comps[[i]])[1]))
}

features <- data.frame(matrix(nrow=0,ncol=3))
for(i in 1:length(comp)){
  features <- rbind(features, comp[[i]])
}


