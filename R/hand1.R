hand1 = function (numattable1, playerseats1, chips1, blinds1, dealer1, 
    ntable1, myfast1, t1, t2, chipstart1, lowercut1, decision1,
name1 = c("vera","william","xena","yosef","zelda")) 
{
    chips2 = chips1
    if (numattable1 < 1.5) 
    return(chips2)
    b3 = deal1(numattable1)
    b4 = bid1(numattable1, playerseats1, chips1, blinds1, dealer1, 
	b3, ntable1, decision1)
    b5 = bid2(numattable1, playerseats1, blinds1, dealer1, b3, 
	b4, 2, ntable1, decision1)
    b6 = bid2(numattable1, playerseats1, blinds1, dealer1, b3, 
	b5, 3, ntable1, decision1)
    b7 = bid2(numattable1, playerseats1, blinds1, dealer1, b3, 
	b6, 4, ntable1, decision1)
    chips2 = calcwin1(numattable1, playerseats1, b3, b7)
    draw1 = 0
    u21 = runif(1)
    if (((max(chips2/(chips1 + 0.01)) > 1.99) && (u21 < t1)) || 
	((max(chips1/(chips2 + 0.01)) > 99) && (u21 < t2))) {
	if (myfast1 < 1) {
	    text(1, lowercut1, "click to continue", cex = 0.7)
	    locator(1)
	}
	if(myfast1 < 1) mygraphics1(numattable1, playerseats1, chips1, blinds1, 
	    dealer1, b3, b4, b5, b6, b7, chips2, ntable1, myfast1, 
	    chipstart1, name1, lowercut1)
	draw1 = 2
    }
    list(chips2 = chips2, draw1 = draw1)
} ## end of hand1

