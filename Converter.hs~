module Converter () where

import Numeric

data Color = HEX Int Int Int | RGB Float Float Float | HSV Float Float Float | HSL Float Float Float | CMYK Float Float Float Float
           deriving (Show, Eq)

mod1 :: Float -> Int -> Float
mod1 a b
     | a < 0 = mod1 (a + fromIntegral b) b
     | a >= fromIntegral b = mod1 (a - fromIntegral b) b
     | otherwise = a

hsv2rgb (HSV h s v) = RGB r g b
        where r = (r' + m) * 255
              g = (g' + m) * 255
              b = (b' + m) * 255
              c = v * s
              m = v - c
              x = c * (1 - abs (mod1 (h / 60) 2 - 1))
              (r', g', b')
                   | h >= 0 && h < 60 = (c, x, 0)
                   | h >= 60 && h < 120 = (x, c, 0)
                   | h >= 120 && h < 180 = (0, c, x)
                   | h >= 180 && h < 240 = (0, x, c)
                   | h >= 240 && h < 300 = (x, 0, c)
                   | h >= 300 && h < 360 = (c, 0, x)

rgb2hsv (RGB r g b) = HSV h s v
             where r' = r / 255
                   g' = g / 255
                   b' = b / 255
                   set = [r', g', b']
                   cmax = maximum set
                   cmin = minimum set
                   delta = cmax - cmin
                   v = cmax
                   s
                        | delta == 0 = 0
                        | otherwise = delta / cmax
                   h
                        | cmax == r' = 60 * (mod1 ((g' - b') / delta) 6)
                        | cmax == g' = 60 * ((b' - r') / delta + 2)
                        | cmax == b' = 60 * ((r' - g') / delta + 4)

hsl2rgb (HSL h s l) = RGB r g b
        where r = (r' + m) * 255
              g = (g' + m) * 255
              b = (b' + m) * 255
              c = (1 - abs (2 * l - 1)) * s
              m = l - c / 2
              x = c * (1 - abs (mod1 (h / 60) 2 - 1))
              (r', g', b')
                   | h >= 0 && h < 60 = (c, x, 0)
                   | h >= 60 && h < 120 = (x, c, 0)
                   | h >= 120 && h < 180 = (0, c, x)
                   | h >= 180 && h < 240 = (0, x, c)
                   | h >= 240 && h < 300 = (x, 0, c)
                   | h >= 300 && h < 360 = (c, 0, x)

rgb2hsl (RGB r g b) = HSL h s l
             where r' = r / 255
                   g' = g / 255
                   b' = b / 255
                   set = [r', g', b']
                   cmax = maximum set
                   cmin = minimum set
                   delta = cmax - cmin
                   l = (cmax + cmin) / 2
                   s
                        | delta == 0 = 0
                        | otherwise = delta / (1 - abs (2 * l - 1))
                   h
                        | cmax == r' = 60 * (mod1 ((g' - b') / delta) 6)
                        | cmax == g' = 60 * ((b' - r') / delta + 2)
                        | cmax == b' = 60 * ((r' - g') / delta + 4)

cmyk2rgb (CMYK c m y k) = RGB r g b
               where r = 255 * (1 - c) * (1 - k)
                     g = 255 * (1 - m) * (1 - k)
                     b = 255 * (1 - y) * (1 - k)

rgb2cmyk (RGB r g b) = CMYK c m y k
                where c = (1 - r' - k) / (1 - k)
                      m = (1 - g' - k) / (1 - k)
                      y = (1 - b' - k) / (1 - k)
                      r' = r / 255
                      g' = g / 255
                      b' = b / 255
                      k = 1 - maximum [r', g', b']

