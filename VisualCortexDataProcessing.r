#####################################################################
#Spike data supplied as in Data/spk_info.txt file.
#Read in the data as 64 bit integers.
#Collect the data in a single file (Data/spike.csv). Time is the first column in
#milliseconds and a one occurs  in the next ten columns if a neuron fires in that
#interval.
#######################################################################
spkFiles <- c('t00', 't02', 't04', 't08', 't10', 't18', 't23', 't25', 't26', 't27')

spikes <- mat.or.vec(7250,11)
spikes[,1] <- c(1:7250)/
colnames(spikes) <- c('time', spkFiles)
for (i in 1:10){
  nom <- paste0('Data/',spkFiles[i],'.spk')
  holdem <- readBin(nom, 'integer', n = 100000, size = 8, endian = 'little')
  lengt <- lengt + length(holdem)
  for (j in holdem){
    spikes[floor((j+50000)/100000),(i+1)] <- spikes[floor((j+50000)/100000),(i+1)]+1
  }
}
spikes <- as.data.frame(spikes)
spikes[,1] <- c(1:7250)/10
colnames(spikes)<- c('time', spkFiles)
write.csv(spikes, 'spikes.csv')


# Process the stimulis data and add a stimulis column to spikes

stim <- readBin('Data//drifting_bar.din', 'integer', n = 1000000, size = 8, endian = 'little')
dim(stim) <- c(2,115200)
stim <- t(stim)

spikes[,12] <- rep(-1, 7250)
names(spikes)[12] <- 'stim_angle'

for (i in 1:115200){spikes[floor((stim[i,1]+50000)/100000),12] <- stim[i,2]}

# Find the transitions in stimuli
trans <- c()
for (i in 2:7250){
  if ((spikes[i,12] - spikes[(i-1),12])*(spikes[i,12] + 1) != 0){trans <- c(trans, i)}
}


# average the spikes in tenth second intervals for each angle.

agg <- array(rep(0, 10*180*40), c(18,10,40))
  for(i in 1:144){
    for (j in 1:10){
      for (k in 0:39){
        agg[(spikes[trans[i],12]+1),j,(k+1)] <- agg[(spikes[trans[i],12]+1),j,(k+1)] + spikes[(trans[i]+k),(j+1)]
      }
    }
  }

# plot the beasts
farbe <- c('black', 'cyan', 'red', 'orange', 'brown', 'magenta', 'orangered', 'green', 'blue', 'violet')
for (i in 0:17){
  nom <- paste0('Images/Angle', 10*i,'.png')
  png(nom, width = 6, height = 5, units = 'in', res = 72)
  plot(time, rep(max(agg[(i+1),,])/2,40), type = 'n', ylab = 'spikes (/tenth sec)', xlab = 'time (sec)')
  for (j in 1:10){
    lines(time, agg[(i+1),j,], col = farbe[j])}
  title(main = paste0(10*i, ' degree bar'))
  dev.off()}


# The following command would not run from within R using the system command; run at
# the linux command line.
# montage Angle0.png Angle10.png Angle20.png Angle30.png Angle40.png Angle50.png 
#       Angle60.png Angle70.png Angle80.png Angle90.png Angle100.png Angle110.png 
#       Angle120.png Angle130.png Angle140.png Angle150.png Angle160.png 
#       Angle170.png -tile 3x6 -geometry +2+2 NeuronSpikes.png

####################################################################################
#
# To do Principle Component Analysis, want to concatenate the responses at each 
# orientation.
#
#####################################################################################

data <- agg[,1,]
for (i in 2:10){data <- cbind(data, agg[,i,])}

pc.data <- princomp(t(data), cor=TRUE, scale = TRUE)
load <- floor(loadings(pc.data)*100)/100
load <- as.data.frame(load[1:18,1:18])

# Place data in appropriate form for lattice
labels <- c()
for (i in 0:9){labels <- c(labels, paste0('0',i))}
for (i in 10:17){labels <- c(labels, as.character(i))}
grph <- cbind(rep(labels,18), rep(0,324), rep('hi', 324))
grph <- as.data.frame(grph)
names(grph)[1] <- 'Orientation'
names(grph)[2] <- 'Amplitude'
names(grph)[3] <- 'Component'
grph[,2] <- as.numeric(grph[,2])
for (i in 0:17){
  for (j in 1:18){
    grph[(18*i +j),2]<-load[j,(i+1)]
  }
}

Comp <- c()
for (i in 2:18){Comp <- c(Comp, rep(paste0('Component ',labels[i]),18))}
Comp <- Comp <- c(Comp, rep(paste0('Component ',18),18))
grph[,3]<-as.factor(Comp)

# plot the components on a lattice plot
library(lattice)
xyplot(Amplitude~Orientation | Component, grph, type = 'l', layout = c(3,6))

