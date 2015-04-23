sixSigmaDemos <- function(){
        
        require(SixSigma)
        
        data <- getData2()
        
        vector <- data$Moisture
        
        
        SixSigma::ss.ca.cp(x = vector,LSL = 2.6,USL = 3.6,f.na.rm = T)
        
        SixSigma::ss.ca.cpk(x = vector,LSL = 2.6,USL = 3.6,f.na.rm = T)
        
        SixSigma::ss.ca.z(x = vector,LSL = 2.6,USL=3.6)
        
        library(qcc)
        X1 = matrix(c(72, 56, 55, 44, 97, 83, 47, 88, 57, 26, 46, 
                      49, 71, 71, 67, 55, 49, 72, 61, 35, 84, 87, 73, 80, 26, 89, 66, 
                      50, 47, 39, 27, 62, 63, 58, 69, 63, 51, 80, 74, 38, 79, 33, 22, 
                      54, 48, 91, 53, 84, 41, 52, 63, 78, 82, 69, 70, 72, 55, 61, 62, 
                      41, 49, 42, 60, 74, 58, 62, 58, 69, 46, 48, 34, 87, 55, 70, 94, 
                      49, 76, 59, 57, 46), ncol = 4)
        X2 = matrix(c(23, 14, 13, 9, 36, 30, 12, 31, 14, 7, 10, 
                      11, 22, 21, 18, 15, 13, 22, 19, 10, 30, 31, 22, 28, 10, 35, 18, 
                      11, 10, 11, 8, 20, 16, 19, 19, 16, 14, 28, 20, 11, 28, 8, 6, 
                      15, 14, 36, 14, 30, 8, 35, 19, 27, 31, 17, 18, 20, 16, 18, 16, 
                      13, 10, 9, 16, 25, 15, 18, 16, 19, 10, 30, 9, 31, 15, 20, 35, 
                      12, 26, 17, 14, 16), ncol = 4)
        X = list(X1 = X1, X2 = X2)
        
        q = mqcc(vector, type = "T2")
        summary(q)
        ellipseChart()
        ellipseChart(q, show.id = TRUE)
        
        effect <- "Flight Time"
        causes.gr <- c("Operator", "Environment", "Tools", "Design",
                       "Raw.Material", "Measure.Tool")
        causes <- vector(mode = "list", length = length(causes.gr))
        causes[1] <- list(c("operator #1", "operator #2", "operator #3"))
        causes[2] <- list(c("height", "cleaning"))
        causes[3] <- list(c("scissors", "tape"))
        causes[4] <- list(c("rotor.length", "rotor.width2", "paperclip"))
        causes[5] <- list(c("thickness", "marks"))
        causes[6] <- list(c("calibrate", "model"))
        ss.ceDiag(effect, causes.gr, causes, sub = "Paper Helicopter Project")
        
        ss.study.ca(xST = data$Moisture,LSL = 2.6,USL = 3.6,Target = 3.1)
        
        ss.cc("mr",data[1:25,],CTQ = "Moisture")
        
        ss.ci(x = Moisture,data=data,alpha = 0.05,sub="Moisture Test",xname = "Moisture",digits = 2)
        
        
        ss.lfa(lfa.data = data,lfa.ctq = "Moisture",lfa.Delta = 0.5,lfa.Y0 = 3.1,lfa.L0 = 0.0003,lfa.output = "both")
        
        
        boxplot(formula = Moisture ~ Spec,data = data)
        ss.rr(var = Moisture,part = Spec,data = data)
        d <- ss.data.rr
        example(ss.ci)
        
        example(ss.study.ca)
        example(ss.rr)
        example(ss.lf)
        example(ss.lfa)
        example(ss.ceDiag)
        example(ss.pMap)
        example(ss.ca.yield)
        example(ss.ca.z)
        example(ss.ca.cp)
        example(ss.ca.cpk)
        example(ss.cc)
        
        library(qcc)
        with(ss.data.pb3,
             plot(qcc,stockouts,orders,type="p"))
}