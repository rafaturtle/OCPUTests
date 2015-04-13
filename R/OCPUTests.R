testPlot <- function(){
        require(SixSigma)
        ss.study.ca(ss.data.ca$Volume, rnorm(42, 753, 3),
                             	LSL = 740, USL = 760, T = 750, alpha = 0.05,
                    f.sub = "Winery Project")
}