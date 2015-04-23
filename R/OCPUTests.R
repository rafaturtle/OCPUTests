testPlot <- function(){
        require(SixSigma)
        ss.study.ca(ss.data.ca$Volume, rnorm(42, 753, 3),
                             	LSL = 740, USL = 760, T = 750, alpha = 0.05,
                    f.sub = "Winery Project")
}



DemoHist4 <- function(xTag = "Moisture", cTag = "APCOn"){
        
        require(ggplot2)
        require(plyr)
        
        data <- getData2()
        data <- data[data$Spec == "WMP",]
        data$xTag = data[,xTag]
        data$cTag = factor(data[,cTag])
        
        means <- ddply(data, cTag, summarise, xTag.mean=mean(xTag))
        means$cTag = factor(means[,cTag])
        sds <- ddply(data, cTag, summarise, xTag.sd=sd(xTag))
        sds$cTag = factor(means[,cTag])
        sds$plus = sds$xTag.sd + means$xTag.mean
        sds$minus =  means$xTag.mean - sds$xTag.sd
        
        p <- 
                ggplot(data, aes(x=xTag, fill=cTag)) + 
                geom_histogram(binwidth=.30, alpha=.5, position="identity") + 
                geom_vline(data=means, aes(xintercept=xTag.mean,  colour=cTag),
                           linetype="dashed", size=1) +
                geom_vline(data=sds, aes(xintercept=plus,  colour=cTag),
                           linetype="solid", size=0.5) +
                geom_vline(data=sds, aes(xintercept=minus,  colour=cTag),
                           linetype="solid", size=0.5) +    
                xlab(xTag) +
                scale_fill_discrete(name = cTag) 
        
        p
}

DemoQcc4 <- function(yTag = "Moisture",xTag = "Date", cTag = "APCOn"){
        require(qcc)
        data <- getData2()
        data <- data[data$Spec == "WMP",]
        data <- data[data$APCOn == 1,]
        data$Category = factor(data[,cTag])
        data$Date <- as.Date(as.character(data$DateTime), "%Y-%m-%d")
        data$xTag = data[,xTag]
        data$xTag = as.factor(data[,xTag])
        data$yTag = data[,yTag]
        data$sample = 1:length(data[,1])
        
        
        grps <- qcc.groups(data$yTag,data$Date)
        p <- qcc(grps[,1:25],type="xbar",data.name = yTag,add.stats = FALSE)
        p <- qcc(grps[,25:45],type="xbar",data.name = yTag,add.stats = FALSE)
        
        print(p)
        
}

DemoScat4 <- function(yTag = "Moisture",xTag = "Date", cTag = "APCOn"){
        data <- getData2()
        data$xTag = data[,xTag]
        data$yTag = data[,yTag]
        data$Category = factor(data[,cTag])
        library(ggplot2)
        
        q <- ggplot(data, aes(x=xTag, y=yTag,color=Category)) +
                geom_point(shape=1) +    # Use hollow circles
                geom_smooth() +
                labs(x = xTag,y = yTag,title = paste(xTag,yTag,sep=" "))
        print(q)
}

rChart <- function(){
        library(rCharts)
        names(iris) = gsub("\\.", "", names(iris))
        rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
        
}

randomDairyData2 <- function(date,batch,samples,APC){
        batchSize = samples / batch
        specs <- c("SMP LH","SMP MH","WMP")
        
        if (APC == TRUE){
                ThrouputSD = 0.3
                ThrouputMean = 6.3
                RateMean = 100
                RateSD = 5
                ThermalEfficiencyMean = 0.71
                ThermalEfficiencySD = 0.48
                MoistureSD = 0.388
                MoistureMean = 3.9
                MoistureMeanWMP = 3.25
                SolidsSD = 4.8
                SolidsMean = 53.38
                SolidsMeanWMP = 52.83
                ProteinSMP = 1.2
                ProteinWMP = 2.0
                ProteinMeanSMPLH = 33.0
                ProteinMeanSMPMH = 33.0
                ProteinMeanWMP = 25
                FatSMP = 0.7
                FatWMP = 1.3
                FatMeanSMP = 0.62
                FatMeanWMP = 26
        }
        else{
                ThrouputSD = 0.4
                ThrouputMean = 6.0
                RateMean = 100
                RateSD = 8
                ThermalEfficiencyMean = 0.68
                ThermalEfficiencySD = 0.42
                MoistureSD = 0.588
                MoistureMean = 3.8
                MoistureMeanWMP = 3.1
                SolidsMean = 51.38
                SolidsMeanWMP = 50.23
                SolidsSD = 8.2
                ProteinSMP = 2.2
                ProteinWMP = 2.4
                ProteinMeanSMPLH = 34.5
                ProteinMeanSMPMH = 34.0
                ProteinMeanWMP = 26 
                FatSMP = 1.2
                FatWMP = 2.3
                FatMeanSMP = 0.8
                FatMeanWMP = 27
        }
        
        
        
        df <- data.frame(DateTime = as.POSIXct(date)- 60*30*1:samples,
                         OnProduct = round(rnorm(n = samples,mean = 0.6,sd = 0.1), digits = 0),
                         APCOn = APC,
                         Throughput = rnorm(samples,mean=ThrouputMean,sd=ThrouputSD),
                         Rate =  rnorm(samples,mean=RateMean,sd=RateSD),
                         ThermalEfficiency  = rnorm(samples,mean=ThermalEfficiencyMean,sd=ThermalEfficiencySD),
                         Spec = rep(x = (c(rep(specs[1],samples/batchSize),rep(specs[2],samples/batchSize),rep(specs[3],samples/batchSize))),batchSize/3),
                         Moisture  = NA,
                         Solids = NA,
                         Protein = NA,
                         Fat = NA
        )
        df['Day of Week'] = strftime(df$DateTime,'%A')
        df$Week = format(df$DateTime+3, "%U")
        df$Year = format(df$DateTime, "%Y")   
        df$Month =  format(df$DateTime, "%m")  
        df$Day =  format(df$DateTime, "%d")  
        df$Day[1]
        
        df$Spec[df['Day of Week'] == "Monday" | df['Day of Week'] == "Tuesday"] = "SMP LH"
        df$Spec[df['Day of Week'] == "Wednesday" | df['Day of Week'] == "Thursday"  | df['Day of Week'] == "Friday"] = "SMP MH"
        df$Spec[df['Day of Week'] == "Saturday" | df['Day of Week'] == "Sunday"] = "WMP"
        
        df$Moisture[df$Spec=="SMP LH"] <- rnorm(length(df$Moisture[df$Spec=="SMP LH"]),mean = MoistureMean,sd = MoistureSD )
        df$Moisture[df$Spec=="SMP MH"] <- rnorm(length(df$Moisture[df$Spec=="SMP MH"]),mean = MoistureMean,sd = MoistureSD)
        df$Moisture[df$Spec=="WMP"] <- rnorm(length(df$Moisture[df$Spec=="WMP"]),mean = MoistureMeanWMP,sd = MoistureSD )
        
        df$Solids[df$Spec=="SMP LH"] <- rnorm(length(df$Solids[df$Spec=="SMP LH"]),mean = SolidsMean,sd = SolidsSD)
        df$Solids[df$Spec=="SMP MH"] <- rnorm(length(df$Solids[df$Spec=="SMP MH"]),mean = SolidsMean,sd = SolidsSD)
        df$Solids[df$Spec=="WMP"] <- rnorm(length(df$Solids[df$Spec=="WMP"]),mean = SolidsMeanWMP,sd = SolidsSD)
        
        df$Protein[df$Spec=="SMP LH"] <- rnorm(length(df$Protein[df$Spec=="SMP LH"]),mean = ProteinMeanSMPLH,sd = ProteinSMP)
        df$Protein[df$Spec=="SMP MH"] <- rnorm(length(df$Protein[df$Spec=="SMP MH"]),mean = ProteinMeanSMPMH,sd = ProteinSMP)
        df$Protein[df$Spec=="WMP"] <- rnorm(length(df$Protein[df$Spec=="WMP"]),mean = ProteinMeanWMP,sd = ProteinWMP)
        
        df$Fat[df$Spec=="SMP LH"] <- rnorm(length(df$Fat[df$Spec=="SMP LH"]),mean = 0.9,sd = FatSMP)
        df$Fat[df$Spec=="SMP MH"] <- rnorm(length(df$Fat[df$Spec=="SMP MH"]),mean = 0.8,sd = FatSMP)
        df$Fat[df$Spec=="WMP"] <- rnorm(length(df$Fat[df$Spec=="WMP"]),mean = 26.3,sd = FatWMP)
        
        
        df
}

getData2 <- function(){
        
        batch = 100
        samples <- 600
        date = Sys.Date()
        APC = 0
        APCOff <- randomDairyData2(date = date,batch = batch, samples = samples,APC)
        samples <- 1200
        date2 = min(APCOff$DateTime)
        APC = 1
        APCOn <-randomDairyData2(date = date,batch = batch, samples = samples,APC)
        data <- rbind(APCOff,APCOn)
        
        data
        
}