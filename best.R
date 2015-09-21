best <- function(state,outcome){
        data = read.csv("outcome-of-care-measures.csv", colClasses="character")
        if( any(data[,7] == state )){
                if(outcome == "heart attack"){
                        StateHospital <- data[data[,7]==state,]
                        StateHospital[,11] <- as.numeric(StateHospital[,11])
                        bestSH <- StateHospital[order(StateHospital[,11]),]
                        topsh <- bestSH[bestSH[,11]==bestSH[1,11],]
                        thebest <- topsh[order(topsh[,2]),]
                        bestest <- thebest[1,2]
                        thebest <- as.character(bestest)
                        print(thebest)
                        
                }
                else if(outcome == "heart failure"){
                        StateHospital <- data[data[,7]==state,]
                        StateHospital[,17] <- as.numeric(StateHospital[,17])
                        bestSH <- StateHospital[order(StateHospital[,17]),]
                        topsh <- bestSH[bestSH[,17]==bestSH[1,17],]
                        thebest <- topsh[order(topsh[,2]),]
                        bestest <- thebest[1,2]
                        thebest <- as.character(bestest)
                        print(thebest)
                }
                else if(outcome == "pneumonia"){
                        StateHospital <- data[data[,7]==state,]
                        StateHospital[,23] <- as.numeric(StateHospital[,23])
                        bestSH <- StateHospital[order(StateHospital[,23]),]
                        topsh <- bestSH[bestSH[,23]==bestSH[1,23],]
                        thebest <- topsh[order(topsh[,2]),]
                        bestest <- thebest[1,2]
                        thebest <- as.character(bestest)
                        print(thebest)
                }
                else
                        {stop("invalid outcome")}
        }
        else 
                {stop("invalid state")
        }
}