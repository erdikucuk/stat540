> min.y <- 0
> max.y <- 20
> num.samples <- 1000
> 
  > y <- seq(min.y, max.y, length = num.samples)
> plot(c(0, 20), c(0, 0.5), xlab = "y", ylab = "f(y)", main = "Gamma probability density function", 
       +      type = "n")
> lines(y, dgamma(y, shape = 1, scale = 2), col = "red")
> 
  > lines(y, dgamma(y, shape = 2, scale = 2), col = "green")
> lines(y, dgamma(y, shape = 3, scale = 2), col = "blue")
> lines(y, dgamma(y, shape = 5, scale = 1), col = "magenta")
> lines(y, dgamma(y, shape = 9, scale = 0.5), col = "black")
> set.seed(1)
> gamma.shape <- 10
> gamma.scale <- 0.1
> rgamma(n = 5, shape = gamma.shape, scale = gamma.scale)
[1] 0.7667251 1.4040808 1.3826660 1.0820993 0.5346417
> set.seed(1)
> y <- rgamma(n = 1000, shape = gamma.shape, scale = gamma.scale)
> 
  > estimated.density <- density(y)
> plot(estimated.density, col = "blue")
> rug(y, col = "orange")
> 
  > x <- seq(0, 3, length = 1000)
> true.density <- dgamma(x, shape = gamma.shape, scale = gamma.scale)
> lines(x, true.density, col = "red")
> set.seed(1)
> 
  > gamma.shape <- 10
> gamma.scale <- 0.1
> true.mean <- gamma.shape * gamma.scale
> true.variance <- gamma.shape * gamma.scale^2
> n <- 100
> 
  > y <- rgamma(n, shape = gamma.shape, scale = gamma.scale)
> 
  > y.mean <- mean(y)
> 
  > true.mean - y.mean
[1] -0.004187919
> var(y)
[1] 0.0703188
> mean(y)
[1] 1.004188
> sum(y)/n
[1] 1.004188
> sum((y - mean(y))^2)/(length(y) - 1)
[1] 0.0703188
> num.samp <- 100
> samp.size <- 10
> y <- matrix(rgamma(n = num.samp * samp.size, shape = gamma.shape, scale = gamma.scale), 
              +             nrow = num.samp, ncol = samp.size)
> y.mean <- apply(y, 1, mean)
> y.mean
[1] 0.9103668 1.0178316 0.9764216 1.0058544 1.0394049 0.9667876 0.9893966 1.0610398 0.8525848 0.8413490 1.0245479 0.9172752 1.1945426 1.1178750 1.0838958
[16] 1.0067265 0.8594102 1.0558253 0.8830296 0.9924915 0.9463517 0.8226194 0.8729441 1.0491358 0.9744058 1.0462263 1.0973876 0.9338320 1.0440707 0.9918885
[31] 0.9613634 1.0427261 0.7708971 0.8940714 1.0576761 0.8306351 1.1369274 0.9039078 1.0465212 0.9072024 0.9181539 1.0173492 0.9902054 0.7411605 1.0496122
[46] 0.8947874 0.9932095 0.9953301 0.9225450 0.9654365 0.9034105 0.8956095 0.9272936 1.0390661 1.0615646 1.2257674 0.9902700 0.9385601 1.0248994 1.0288119
[61] 1.0962702 0.9840935 0.9775460 0.9671430 0.9138345 1.0576816 1.1554507 0.9600337 0.9551657 1.1901151 0.9063287 0.7897731 0.9606766 0.9781005 0.9017182
[76] 1.0059300 1.0392256 0.9752507 1.0467518 0.9535800 1.0570633 0.9483669 0.9040800 1.0890368 0.9679271 0.7019180 0.9599776 0.9913878 1.0792835 0.9765248
[91] 1.1988165 0.9397533 0.9588658 1.0824421 1.0193469 0.9996232 1.0581830 1.0050966 1.0351040 0.8456010
> mean.diff <- y.mean - true.mean
> head(mean.diff)
[1] -0.089633160  0.017831632 -0.023578357  0.005854411  0.039404889 -0.033212433
> boxplot(mean.diff)
> gammaSampleMean <- function(gamma.shape, gamma.scale, num.samp, samp.size) {
  +     y <- matrix(rgamma(n = num.samp * samp.size, shape = gamma.shape, scale = gamma.scale), 
                    +                 nrow = num.samp, ncol = samp.size)
  +     
    +     y.mean <- rowMeans(y)
  +     
    +     return(y.mean)
  + }
> 
  > samp.sizes <- c(10, 100, 1000, 10000)
> 
  > names(samp.sizes) <- paste0("n=", samp.sizes)
> 
  > samp.sizes
n=10   n=100  n=1000 n=10000 
10     100    1000   10000 
> num.samp <- 100
> y.mean <- sapply(samp.sizes, gammaSampleMean, num.samp = num.samp, gamma.shape = gamma.shape, 
                   +                  gamma.scale = gamma.scale)
> boxplot(y.mean - true.mean, xlab = "Sample size (n)", ylab = expression(bar(Y)[n] - 
                                                                            +                                                                             mu))
> samp.size
[1] 10
> y.mean
n=10     n=100    n=1000   n=10000
[1,] 0.8756928 1.0197017 0.9972830 0.9956370
[2,] 1.0407306 0.9629492 1.0095767 1.0023590
[3,] 1.1720665 1.0127968 0.9889721 1.0018692
[4,] 0.9642501 1.0277757 0.9992708 0.9979524
[5,] 1.1249375 1.0668992 0.9855133 1.0040407
[6,] 1.1253408 0.9938143 0.9993136 0.9948940
[7,] 1.0782357 0.9586093 0.9880259 0.9987881
[8,] 1.0584460 0.9496158 0.9997083 1.0008274
[9,] 1.0411451 1.0117888 1.0124211 0.9983399
[10,] 0.9465127 1.0192113 1.0116733 0.9963452
[11,] 1.0777984 0.9949145 0.9973319 1.0030805
[12,] 0.8487217 1.0090894 0.9932284 0.9987434
[13,] 1.2625568 1.0555459 0.9976301 0.9993911
[14,] 0.9878156 0.9760876 1.0069794 1.0032106
[15,] 0.8425649 1.0047920 1.0049270 0.9983578
[16,] 1.0744436 1.0175528 1.0079230 0.9982788
[17,] 1.0234749 0.9948691 1.0086605 1.0047254
[18,] 0.9156850 1.0031448 1.0041963 0.9987222
[19,] 0.9505250 0.9767122 1.0195527 1.0040245
[20,] 1.0519633 0.9753168 0.9986123 0.9999173
[21,] 1.0341599 1.0033802 0.9881650 0.9989676
[22,] 1.1265208 1.0623912 0.9872783 0.9971472
[23,] 0.9151108 0.9874636 0.9947408 1.0054027
[24,] 1.0739459 0.9409821 1.0020201 1.0043186
[25,] 1.0535605 1.0040692 1.0002329 0.9981214
[26,] 0.9563221 1.0106713 1.0018758 1.0069347
[27,] 1.0539207 0.9853526 1.0060580 0.9942721
[28,] 1.1158298 0.9435585 0.9970219 0.9974163
[29,] 0.9586612 1.0224739 0.9996041 1.0031002
[30,] 0.9511177 0.9522904 1.0114952 0.9986564
[31,] 1.0941213 0.9947109 0.9809881 1.0018605
[32,] 0.8789790 1.0515509 1.0234537 0.9944539
[33,] 0.8942131 0.9955791 1.0112536 0.9976855
[34,] 0.9490979 1.0266755 0.9902550 1.0025260
[35,] 1.1492567 1.0201168 0.9907530 1.0047551
[36,] 0.9679645 1.0410276 1.0052079 0.9982676
[37,] 0.9232236 0.9883578 1.0026044 0.9917620
[38,] 0.9881896 1.0114143 1.0001076 0.9964179
[39,] 0.9124711 0.9813581 0.9961181 1.0071827
[40,] 0.9467467 1.0496722 0.9948793 0.9983987
[41,] 1.0924252 0.9433768 0.9818018 0.9979523
[42,] 1.0345401 1.0146832 1.0079334 1.0021605
[43,] 1.0620398 0.9663270 0.9998978 0.9995375
[44,] 1.0222980 0.9792750 0.9982332 1.0034877
[45,] 1.2504853 0.9765624 1.0024275 1.0061924
[46,] 0.8601939 1.0495800 0.9979580 1.0001250
[47,] 1.0873711 1.0123437 1.0022402 1.0053611
[48,] 1.0163937 1.0131300 0.9853435 1.0046050
[49,] 0.9761196 0.9685268 0.9989106 1.0020680
[50,] 1.1043128 1.0175986 0.9852929 1.0021275
[51,] 1.0009465 1.0647821 1.0066577 0.9980436
[52,] 0.9144467 1.0228306 0.9941264 1.0041013
[53,] 0.7784193 1.0141485 1.0048422 0.9950632
[54,] 1.0594803 1.0370085 0.9925678 0.9975414
[55,] 1.0263906 0.9883623 1.0063156 1.0032844
[56,] 1.0571383 1.0372105 1.0125455 0.9991562
[57,] 0.8184071 0.9969154 1.0126601 0.9976691
[58,] 1.1045598 1.0602194 0.9930512 0.9935648
[59,] 0.9865891 1.0054229 1.0037608 0.9965029
[60,] 1.0496322 1.0140854 1.0087039 0.9970291
[61,] 0.9625622 1.0339009 0.9985166 1.0026052
[62,] 0.9001236 0.9493714 1.0074616 0.9957667
[63,] 1.1170196 1.0233661 0.9843223 0.9998044
[64,] 0.9561572 0.9449701 1.0084756 0.9988803
[65,] 1.1658734 1.0314333 1.0044062 0.9948818
[66,] 1.0948312 0.9721260 1.0118692 0.9974821
[67,] 1.0130645 0.9595148 1.0142387 1.0000242
[68,] 1.0291771 1.0019710 1.0147653 0.9977779
[69,] 1.0436036 1.0118477 1.0212665 1.0007128
[70,] 1.0766152 0.9914631 1.0027338 0.9997157
[71,] 0.9431953 0.9502035 0.9944072 0.9999979
[72,] 0.9699768 1.0109623 1.0003117 1.0000184
[73,] 0.8484715 0.9806909 0.9966989 0.9970861
[74,] 1.0204434 1.0087292 1.0090277 1.0030107
[75,] 1.2628554 1.0212128 1.0044343 0.9961644
[76,] 1.2036880 1.0584171 0.9935700 0.9984432
[77,] 1.0492204 1.0106295 0.9955487 1.0011532
[78,] 1.0574672 1.0040910 0.9998637 0.9991253
[79,] 1.0715237 0.9665690 1.0015040 1.0000441
[80,] 1.2396141 1.0105982 0.9980179 0.9976339
[81,] 0.9951734 1.0102745 0.9846826 0.9972224
[82,] 1.1599260 0.9789776 0.9950730 0.9955985
[83,] 1.0002495 0.9798022 0.9997737 0.9984427
[84,] 1.0243585 0.9902830 1.0052405 1.0022950
[85,] 0.8835503 0.9629600 0.9917421 0.9999267
[86,] 1.1162570 0.9648251 0.9906772 1.0007599
[87,] 1.2216761 1.0524720 1.0005126 1.0000181
[88,] 0.8260037 1.0004556 0.9966740 1.0030908
[89,] 1.0533160 1.0152119 1.0048603 0.9998987
[90,] 0.9859324 1.0232411 0.9958913 0.9984513
[91,] 0.8437638 1.0012380 0.9984809 0.9964173
[92,] 1.0118877 1.0406398 1.0072739 1.0004545
[93,] 1.0744398 0.9833270 1.0090688 1.0016127
[94,] 1.0599813 0.9952918 1.0004770 1.0005842
[95,] 0.9458446 1.0013769 1.0024436 0.9971225
[96,] 1.0234475 1.0173573 1.0094888 0.9983644
[97,] 0.9906684 0.9786923 0.9876404 1.0022589
[98,] 0.9750467 0.9750828 0.9922074 0.9994412
[99,] 0.7851263 1.0137275 1.0085242 1.0025555
[100,] 0.9803802 1.0366261 0.9850294 1.0045588
> z.n <- (sqrt(samp.size) * (y.mean - true.mean))/sqrt(true.variance)
> hist(z.n, probability = TRUE, xlab = expression(Z[n]))
> y <- seq(min(z.n), max(z.n), length = 1000)
> dens <- dnorm(y, mean = 0, sd = 1)
> 
  > lines(y, dens, col = "red")
> gammaNormalisedMean <- function(gamma.shape, gamma.scale, num.samp, samp.size) {
  +     # Compute the true values
    +     true.mean <- gamma.shape * gamma.scale
  +     
    +     true.variance <- gamma.shape * gamma.scale^2
  +     
    +     # Draw a sample
    +     y <- matrix(rgamma(n = num.samp * samp.size, shape = gamma.shape, scale = gamma.scale), 
                      +                 nrow = num.samp, ncol = samp.size)
  +     
    +     y.mean <- rowMeans(y)
  +     
    +     # Compute normalised values
    +     z.n <- (sqrt(samp.size) * (y.mean - true.mean))/sqrt(true.variance)
  +     
    +     return(z.n)
  + }
> plotNormalComparison <- function(gamma.shape, gamma.scale, num.samp, n) {
  +     z.n <- gammaNormalisedMean(gamma.shape, gamma.scale, num.samp, n)
  +     
    +     # It will be nice to have a title
    +     fig.title <- paste0("n=", n)
  +     
    +     # Plot a histogram
    +     hist(z.n, probability = TRUE, main = fig.title, xlab = expression(Z[n]))
  +     
    +     # Compute the normal density and overlay it on the plot in red
    +     y <- seq(min(z.n), max(z.n), length = 1000)
  +     
    +     dens <- dnorm(y, mean = 0, sd = 1)
  +     
    +     lines(y, dens, col = "red")
  + }
> n <- 10
> plotNormalComparison(gamma.shape, gamma.scale, num.samp, n)
> n <- 100
> plotNormalComparison(gamma.shape, gamma.scale, num.samp, n)
> n <- 1000
> 
  > plotNormalComparison(gamma.shape, gamma.scale, num.samp, n)
> n <- 10
> 
  > z.n <- gammaNormalisedMean(gamma.shape, gamma.scale, num.samp, n)
> 
  > ks.test(z.n, pnorm, mean = 0, sd = 1)

One-sample Kolmogorov-Smirnov test

data:  z.n
D = 0.1017, p-value = 0.2522
alternative hypothesis: two-sided

> n <- 10000
> 
  > z.n <- gammaNormalisedMean(gamma.shape, gamma.scale, num.samp, n)
> 
  > ks.test(z.n, pnorm, mean = 0, sd = 1)

One-sample Kolmogorov-Smirnov test

data:  z.n
D = 0.0558, p-value = 0.9145
alternative hypothesis: two-sided

> install.packages("lattice")
trying URL 'http://cran.rstudio.com/bin/macosx/contrib/3.0/lattice_0.20-24.tgz'
Content type 'application/x-gzip' length 705141 bytes (688 Kb)
opened URL
==================================================
  downloaded 688 Kb


The downloaded binary packages are in
/var/folders/g2/xvx3nby927d7whpqyh7748y80000gn/T//RtmpoILozq/downloaded_packages
> library("lattice", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
> y.mean.tall <- data.frame(n = rep(names(samp.sizes), each = num.samp), x = as.vector(y.mean))
> head(y.mean.tall)
n         x
1 n=10 0.8756928
2 n=10 1.0407306
3 n=10 1.1720665
4 n=10 0.9642501
5 n=10 1.1249375
6 n=10 1.1253408
> bwplot(x - true.mean ~ as.factor(n), y.mean.tall, xlab = "Sample size (n)", 
         +        ylab = expression(bar(Y)[n] - mu))
> densityplot(~x - true.mean, y.mean.tall, groups = n, auto.key = TRUE, xlab = expression(bar(Y)[n] - 
                                                                                            +                                                                                             mu))