
# custom function to make plotting points semi-transparent

makeTransparent <- function(black, alpha = 200) {
     newColor <- col2rgb(black)
     apply(newColor, 2, function(curcoldata)			{
          rgb(red = curcoldata[1],
               green = curcoldata[2],
               blue = curcoldata[3],
               alpha = alpha,
               maxColorValue = 255)
     })
}

tBlack  <- makeTransparent("gray40")
tRed    <- makeTransparent("red")
tOrange <- makeTransparent("orange")