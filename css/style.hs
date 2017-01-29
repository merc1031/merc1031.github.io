{-# LANGUAGE OverloadedStrings #-}
import Data.Bits
import Clay
import Clay.Font
import qualified Clay.Display as CD
import qualified Clay.Text as CT
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = TIO.putStrLn $ renderWith compact [] myStylesheet


myStylesheet :: Css
myStylesheet =
  html ? do
    noMargin
    set4 padding $ px 0
    let
      bgHexColor = 0xd1dbbd
      textColor = grayish 30
    backgroundColor $ rgbHex bgHexColor
    color textColor
    overflowY scroll
    body ? do
      width (px bodyPageWidth)
      set22 margin (px 0) auto
      hr ? do
        noMargin
        height (px 1)
  where
    noMargin = margin 0 0 0 $ px 0
    set4 f x = f x x x x
    set22 f x y = f x y x y
    bodyPageWidth = 1000

rgbHex :: Int -> Color
rgbHex rgb'
  | rgb' > 0xffffff || rgb' < 0 = error "invalid hex range"
  | otherwise = rgb rr gg bb
  where
    rr = fromIntegral $ shiftR rgb' 16 .&. 0xFF
    gg = fromIntegral $ shiftR rgb' 8 .&. 0xFF
    bb = fromIntegral $ rgb' .&. 0xFF

