tourn1 = function (name1, decision1, chipstart1 = 1000, bigstart1 = 20, 
    inc1 = 1.5, winners1 = 3, myfast1 = 2, t1 = 0.5, t2 = 1, 
    graphiccutoff1 = 0.1, lowercut1 = 30) 
{
    nplayers1 = length(name1)
    plot(c(0, nplayers1 + 1), c(lowercut1, chipstart1 * nplayers1), 
	pch = name1[1:nplayers1], type = "n", xlab = "player number", 
	ylab = "chips", log = "y")
    chip1 = rep(chipstart1, nplayers1)
    text(x = c(1:nplayers1), y = chip1, cex = 2, labels = name1[1:nplayers1], 
	srt = 270, col = 2)
    chip7 = chip1
    big1 = round(bigstart1)
    sm1 = round(big1/2)
    blinds1 = c(sm1, big1)
    nleft1 = nplayers1
    plleft = 1:nplayers1
    places1 = rep(0, winners1)
    stp3 = 0
    while (stp3 < 1) {
	if (nleft1 > 20.5) {
	    ntable1 = floor(nleft1/10)
	    pl1 = sample(nleft1)
	    tables1 = list(tbnums = rep(10, ntable1))
	    for (j in c(1:ntable1)) {
		tables1[[1 + j]] = plleft[pl1[(1:10) + (j - 1) * 
		10]]
	    }
	}
	if ((nleft1 < 20.5) && (nleft1 > 10.5)) {
	    ntable1 = 2
	    pl1 = sample(nleft1)
	    thalf1 = ceiling(nleft1/2)
	    bhalf1 = nleft1 - thalf1
	    tables1 = list(tbnums = c(thalf1, bhalf1))
	    tables1[[2]] = plleft[pl1[1:thalf1]]
	    tables1[[3]] = plleft[pl1[(thalf1 + 1):nleft1]]
	}
	if (nleft1 < 10.5) {
	    ntable1 = 1
	    pl1 = sample(nleft1)
	    tables1 = list(tbnums = nleft1)
	    tables1[[2]] = plleft[pl1]
	}
	cat("\n Big blind is ", blinds1[2], "\n")
	for (i in 1:ntable1) {
	    k = 0
	    for (j in 1:4) {
		chip3 = chip1[tables1[[1 + i]]]
		k = k + 1
		if (k > tables1[[1]][i]) 
		k = 1
		cat(j)
		x32 = hand1(tables1[[1]][i], tables1[[1 + i]], 
		    chip3, blinds1, k, ntable1, myfast1, t1, t2, 
		    chipstart1, lowercut1, decision1)
		chip2 = x32$chips2
		chip1[tables1[[1 + i]]] = chip2
		chipdif8 = (abs(chip1 - chip7)/pmax(chip1, chip7, 
			rep(1, nplayers1)) > graphiccutoff1)
		#if (x32$draw1 > 1) {
		#  text(x = c(1:nplayers1), y = chip1, cex = 2, 
		#labels = name1[1:nplayers1], srt = 270, col = 2)
		#  chip7 = chip1
		#}
		#else 
		if (sum(chipdif8) > 0.5) {
		    text(x = c(1:nplayers1)[chipdif8], y = chip7[chipdif8], 
			cex = 2, col = "white", labels = name1[chipdif8], 
			srt = 270)
		    text(x = c(1:nplayers1)[chipdif8], y = chip7[chipdif8], 
			cex = 2, col = "white", labels = name1[chipdif8], 
			srt = 270)
		    text(x = c(1:nplayers1)[chipdif8], y = chip1[chipdif8], 
			cex = 2, col = 2, labels = name1[chipdif8], 
			srt = 270)
		    chip7[chipdif8] = chip1[chipdif8]
		}
		j1 = sum(chip2 < 0.5)
		if (j1 > 0.5) {
		    j2 = tables1[[i + 1]][c(1:tables1[[1]][i])[chip2 < 
		    0.5]]
		    j3 = j2[order(chip3[j2], decreasing = TRUE)]
		    j4 = min(winners1, nleft1)
		    nleft1 = nleft1 - j1
		    if (nleft1 < winners1 - 0.5) 
		    places1[(nleft1 + 1):j4] = j3[1:(j4 - nleft1)]
		    cat("\n Eliminated: ", j2, ".....", nleft1, 
			" players remaining.\n")
		    tables1[[1]][i] = tables1[[1]][i] - j1
		    tables1[[1 + i]] = tables1[[1 + i]][chip2 > 
		    0.5]
		}
		if (nleft1 < 1.5) 
		break
	    }
	    if (nleft1 < 1.5) 
	    break
	}
	big1 = round(blinds1[2] * inc1)
	sm1 = round(big1/2)
	blinds1 = c(sm1, big1)
	plleft = c(1:nplayers1)[chip1 > 0.5]
	nleft1 = length(plleft)
	if (nleft1 < 1.5) {
	    stp3 = 2
	    places1[1] = plleft
	    z2 = winners1 + 1
	    plot(c(0, nplayers1 + 1), c(lowercut1, chipstart1 * 
		    nplayers1), type = "n", xlab = "player number", 
		ylab = "chips", log = "y")
	    text(1 * nplayers1/z2, nplayers1 * chipstart1, "1st:", 
		col = 4, cex = 2)
	    text(2 * nplayers1/z2, nplayers1 * chipstart1, "2nd:", 
		col = 4, cex = 2)
	    text(3 * nplayers1/z2, nplayers1 * chipstart1, "3rd:", 
		col = 4, cex = 2)
	    if (z2 > 4.5) 
	    for (z1 in c(4:winners1)) text(z1 * nplayers1/z2, 
		nplayers1 * chipstart1, paste(z1, "th:"), col = 4, 
		cex = 2)
	    for (z1 in c(1:winners1)) text(z1 * nplayers1/(winners1 + 
		    1), (nplayers1/2) * chipstart1, name1[places1[z1]], 
		col = 4, cex = 2)
	}
    }
    places1
}
 ## end of tourn1

