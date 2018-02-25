library(colorscience)

chromaticity.diagram.color(conversionFunction=CIE1931XYZ2CIE1976uv, 
                           xlim=c(0, 0.7),
                           ylim=c(0, 0.65),
                           xlab="u'",ylab="v'")
library(png)
im <- png::readPNG("spectral_image/Spectral Image made in Wolfram Mathematica.png")

rasterImage(im, xleft = -0.014, xright = 0.714, ybottom = -0.015, ytop = 0.664)


XYZ <- spectra2XYZ(MaterialReferenceData[,c('wavelength','BlueSky')])
uv <- CIE1931XYZ2CIE1976uv(XYZ)
points(x = uv[1], y = uv[2], col = rgb(XYZ2RGB(XYZ)), pch = 19, lw = 9)


