
#
# Code for the Extend vignette
#

# Plot unbalanced A x B x C designs


transects0 <- expand.grid(year=1:5, site=letters[1:3], transect=1:5)

set.seed(1234)
transects <- transects0[sample(1:75, 30),]

plotABC <- function(data, x, y, colF=function(i,j,k) "cornflowerblue") {

    f <- function(x) as.factor(data[[x]])

    X <- data.frame(x=f(x), y=f(y))

    z <- xtabs( ~ x + y, data=X)

    x <- dimnames(z)[[1]]; nx <- length(x)
    y <- dimnames(z)[[2]]; ny <- length(y)

    xplot <- rep(seq_along(x), times=dim(z)[2]); dim(xplot) <- dim(z)
    yplot <- rep(seq_along(y), each=dim(z)[1]); dim(yplot) <- dim(z)

    old.par <- par(no.readonly=TRUE); on.exit(par(old.par))

    fin <- par("fin")
    plt0 <- c(0.2, 0.9, 0.2, 0.9)

    plt1 <- plt0

    xin0 <- fin[1]*(plt0[2]-plt0[1])
    yin0 <- fin[2]*(plt0[4]-plt0[3])

    if(xin0/yin0 > nx/ny) {

        xin1 <- yin0*nx/ny
        d <- (xin0-xin1)/2

        plt1[1] <- plt0[1] + d/fin[1]
        plt1[2] <- plt0[2] - d/fin[1]

    } else {

        yin1 <- xin0*ny/nx
        d <- (yin0-yin1)/2

        plt1[3] <- plt0[3] + d/fin[2]
        plt1[4] <- plt0[4] - d/fin[2]

    }

    par(plt=plt1)

    plot(
        xplot, yplot,
        xaxt="n", yaxt="n", xlab="", ylab="",
        xlim=range(xplot)+c(-1,1)/2, ylim=range(yplot)+c(-1,1)/2,
        asp=1, bty="o", usr=c(0,0,6,4), plt=plt1
    )

    #box("plot")
    #box("figure", col="blue", lwd=3)

    axis(1, seq_len(nx), x, lwd=0, lwd.tick=1)
    axis(2, seq_len(ny), y, lwd=0, lwd.tick=1)

    title(xlab="Year", ylab="Site")

    w <- 0.75

    xadj <- 0.1
    yadj <- 0.15

    xshadow <- 0.03
    yshadow <- 0.05

    for(i in rev(seq_len(nrow(z)))) for(j in rev(seq_len(ncol(z)))) {

        for(k in seq_len(z[i,j])) {

            x <- xplot[i,j] + (k-1)*xadj - w/2
            y <- yplot[i,j] + (k-1)*yadj - w/2

            if(k > 1) {

                rect(x-xshadow, y-yshadow, x+w-xadj, y+w-yadj, col=simr:::alpha("black", 1/5), border=NA)
            }

            rect(x, y, x+w, y+w, col=colF(i,j,k), xpd=TRUE)
        }
    }
}

myCF <- function(i,j,k) if(k > 2) "palevioletred" else "cornflowerblue"
myCF <- function(i,j,k) if(i > 4) "palevioletred" else "cornflowerblue"

plotABC(transects, "year", "site", myCF)
