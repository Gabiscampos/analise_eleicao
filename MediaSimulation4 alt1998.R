# Next Steps
# Make sure sim is running properly.
# parties with 0 seats
# parties where votes get down to zero STOP (vote change min of threshold and votes of j)
# one less seat votes missing if 0 seats
# Check sign on vote change

# Then - adapt so that it can run on the whole enchilada

# Then - apply and document


# Try for Media Simulation
require(foreign)
setwd("\\fs-eesp-01\eesp-bd-02\BASES-CEPESP\Camara\Scott")
require(readstata13)

# Sim data


# Read in Data and Collapse to Legenda Level

TT1<-	read.dta13("TTPlusLegenda_v1998.dta")

# FurtherSubset to keep just those with votes=0 or better
TT1<-subset(TT1,is.na(total_votos)==0 & codigo_cargo==6)

# Clean up dataset
TT1$sigla_uf<-droplevels(TT1$sigla_uf)

# Get data to merge in
VoteTotals<-aggregate(x=list(TT1$total_votos,TT1$elected),by=list(TT1$ano_eleicao,TT1$sigla_uf,TT1$codigo_cargo,TT1$codigo_legenda),FUN=function(x){sum(x,na.rm=TRUE)})
names(VoteTotals)<-c("ano_eleicao","sigla_uf","codigo_cargo","codigo_legenda","Votes","SeatsWon")

TotUFSeats<-aggregate(x=list(TT1$elected),by=list(TT1$ano_eleicao,TT1$sigla_uf,TT1$codigo_cargo),FUN=function(x){sum(x,na.rm=TRUE)})
names(TotUFSeats)<-c("ano_eleicao","sigla_uf","codigo_cargo","UFSeats")

# aggregate to party list (legenda list) totals

VotesByLegenda<-merge(x=VoteTotals,y=TotUFSeats,by.x=c("ano_eleicao","sigla_uf","codigo_cargo"),by.y=c("ano_eleicao","sigla_uf","codigo_cargo"))
dim(VotesByLegenda)
VotesByLegenda<-subset(VotesByLegenda,Votes!=0)
dim(VotesByLegenda)
#VBL<-subset(VotesByLegenda,codigo_cargo==6 & sigla_uf=="AC" & ano_eleicao==2002)

# Add quotient
Quotient<-aggregate(x=list(VotesByLegenda$Votes),by=list(VotesByLegenda$UFSeats,VotesByLegenda$ano_eleicao,VotesByLegenda$sigla_uf,VotesByLegenda$codigo_cargo),FUN=function(x){sum(x,na.rm=TRUE)})
names(Quotient)<-c("TempSeats","ano_eleicao","sigla_uf","codigo_cargo","TotVotes")
Quotient$Q<-Quotient$TotVotes/Quotient$TempSeats

VBL.1<-merge(x=VotesByLegenda,y=Quotient,by.x=c("ano_eleicao","sigla_uf","codigo_cargo"),by.y=c("ano_eleicao","sigla_uf","codigo_cargo"))


# Now a function to tearn Votes to Seats
# Get total number of votes
# Calculate and allocate quotient seats
# Measure distance to quotients via inter-legenda transfers
# Now allocate Medias, vote by vote? 
# 	Get min votes to change outcome. 
#   See if that matters
#   Run up from there until matters
#   Only do this for parties that had at least some votes.x


Votes2Seats<-function(Votes,Legenda,UFSeats){
  # Quotient Seats
  Votes[is.na(Votes)]<-0
  TotalVotes<-sum(Votes,na.rm=TRUE)
  
  Quotient<-TotalVotes/UFSeats
  QuotientSeats<-floor(Votes/Quotient)
  QuotientSeats
  TotalQuotientSeats<-sum(QuotientSeats,na.rm=TRUE)
  #print(QuotientSeats)
  # Now Media's
  
  MediaSeats2Allocate<-UFSeats-TotalQuotientSeats
  S<-QuotientSeats
  for(i in 1:MediaSeats2Allocate){
    #print(i)
    #print(S)
    MQuotient<-Votes/(S+1)
    #print(MQuotient[S!=0])
    S[S!=0]<-S[S!=0]+1*(MQuotient[S!=0]==max(MQuotient[S!=0]))
  }	
  #print(cbind(Legenda,Votes,S))
  #print(S)
  return(S)
}
Votes2Seats(Votes=VBL.1$Votes,Legenda=VBL.1$codigo_legenda,UFSeats=8)
#VBL2[,c("S","OneMoreQ","OneLessQ")]



# Now - cycle through each legenda and assess

# If zero seats, how many to get a quotient?

# If one seat, how many to:
# Get another quotient?
# Get another Media?
# Lose a quotient
# Lose another Media

# Run votes from MaxThresh down.

#	If no change at Max Thresh, Next!
#		If no seats, doesn't matter which party.'

# For losing a seats, start at x-MaxThresh
# if no seats, don't do this.
# if 1+ seats, start at minimum votes and check for changing. then stop.	

#1. Check if any seats.
#####################################
# Work on Party i
# PREREQUISITES
# NEED NUMSEATS
# NEED QUOTIENT
# NEED MAX THRESH
# NEED NUMSEATS ALLOCATED IN THAT STATE
# Need NumParties


VBL2<-merge(x=VBL.1,y=Quotient,by.x=c("ano_eleicao","sigla_uf","codigo_cargo"),by.y=c("ano_eleicao","sigla_uf","codigo_cargo"))

detach(VBL2)
detach(VBL2)
detach(VBL2)
detach(VBL2)
detach(VBL2)



attach(VBL2)


# NOW - NEED TO DO THIS FOR EVERY UF, ANO, FOR SOME CARGOS!
# POSSIBLY SUBSET AND CYCLE?
# I WOULD NEED A DATASET OF UF, ANO, CARGO

# What are the inputs here?

VoteSwaps<-function(X){
  try(detach(X))
  try(detach(X))
  attach(X)
  # Right before loop
  VoteSim<-Votes
  QSeats<-floor(Votes/Q)
  OneMoreQuotient <- (QSeats+1)*Q-Votes
  OneLessQuotient <- (Votes-(QSeats)*Q-1)*ifelse(SeatsWon==0,NA,1)
  
  TwoMoreQuotients <- (QSeats+2)*Q-Votes
  TwoLessQuotients <- Votes-(QSeats-1)*Q-1*ifelse(SeatsWon<2,NA,1)
  NumLegendas<-dim(X)[1]
  
  for(i in 1:NumLegendas){
    print(i)
    for(j in 1:NumLegendas){
      print(j)
      VoteSim<-Votes
      print(c(i,j))
      if(i!=j){
        print(paste("Now looking at party ",i," and party ",j,sep=""))
        Votes2GainSeat<-OneMoreQuotient
        Votes2LoseSeat<-OneLessQuotient
        Votes2Gain2Seats<-TwoMoreQuotients
        Votes2Lose2Seats<-TwoLessQuotients
        
        if(SeatsWon[i]>0){
          # Check for Min to Get One More Seat
          for(VoteChange in 1:min(TwoMoreQuotients[i],Votes[j])){
            
            # Transfer Votes from j to i
            VoteSim[i]<-Votes[i]+VoteChange
            VoteSim[j]<-Votes[j]-VoteChange
            
            # Now re-run election
            SimResults<-Votes2Seats(Votes=VoteSim,Legenda=codigo_legenda,UFSeats)
            
            # Did results change? If so, note votes required
            if(SimResults[i]>SeatsWon[i] & VoteChange<Votes2GainSeat[i]){
              print("Found an easier route to one more seat")	
              print(Votes)
              print(VoteSim)
              print(SeatsWon)
              print(SimResults)
              Votes2GainSeat[i]<-VoteChange
            }
            if(SimResults[i]>(1+SeatsWon[i]) & VoteChange<Votes2Gain2Seats[i]){
              print("Found an easier route to TWO more seats")	
              print(Votes)
              print(VoteSim)
              print(SeatsWon)
              print(SimResults)
              Votes2Gain2Seats[i]<-VoteChange
            }
            if(SimResults[j]<SeatsWon[j] & VoteChange<Votes2LoseSeat[j]){
              print("Found an easier route to one fewer seat")	
              Votes2LoseSeat[j]<- VoteChange
            }
            if(SimResults[j]<(SeatsWon[j]-1) & VoteChange<Votes2Lose2Seats[j]){
              print("Found an easier route to one fewer seat")	
              Votes2Lose2Seats[j]<- VoteChange
            }
            
          }
        }
      }
    }}
  
  return(cbind(ano_eleicao,sigla_uf,codigo_cargo,codigo_legenda,SeatsWon,Votes2LoseSeat,Votes2GainSeat,Votes2Lose2Seats,Votes2Gain2Seats)	)
  detach(X)
}


counter<-1
for(Year in c(1998,2002,2006,2010,2014)){
  #for(Year in c(2014)){
  for(State in c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")){
    #	for(State in c("AC","AP")){
    for(Office in c(6)){
      print(paste("Beginning: ",Year,State,Office))
      VBL<-subset(VBL.1,codigo_cargo==Office & sigla_uf==State & ano_eleicao==Year)
      VoteSwapsTemp<-VoteSwaps(VBL)
      if(counter==1){
        print("Creating First Results Dataframe")
        LegendaVoteChange<-VoteSwapsTemp
      }
      if(counter>1){
        print("Adding results to Dataframe")
        #print(VoteSwapsTemp)
        #print(LegendaVoteChange)
        LegendaVoteChange<-rbind(LegendaVoteChange,VoteSwapsTemp)
      }
      print(paste("Dimensionality of LegendaVoteChange: ",dim(LegendaVoteChange)))
      counter<-counter+1			 
      # Now send VBL to a function to get votes needed for media and quotient seats.
      try(detach(X),silent=TRUE)
      try(detach(X),silent=TRUE)
      try(detach(X),silent=TRUE)
    }
  }
}

# LVC2014<-as.data.frame(LegendaVoteChange)
#save(LVC2014,file="LVC2014,Rdata")
#save(LegendaVoteChange,file="LegendaVoteChange.Rdata")

# Now Merge with existing dataset
#load("LVC2014.Rdata")
#LVC2014<-as.data.frame(LVC2014)
#load('LegendaVoteChange.Rdata')
#LVC20022010<-subset(as.data.frame(LegendaVoteChange),ano_eleicao!=2014)

#LVCAll<-rbind(LVC20022010,LVC2014)


LVC<-as.data.frame(LegendaVoteChange)
write.dta(LVC,file="QandMVotes_v2.dta")