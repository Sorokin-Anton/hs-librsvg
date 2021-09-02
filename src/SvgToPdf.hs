{-# LANGUAGE LambdaCase #-}
module SvgToPdf where

import Graphics.Rendering.Cairo.LibRSvg
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Graphics.Rendering.Cairo (withPDFSurface, renderWith)

loadSvg :: FilePath -> IO Svg
loadSvg path = do
  result <- fromBuffer =<< BS.readFile path
  case result of
    Left bs -> fail $ BSC.unpack bs
    Right sp -> pure sp

svgToPdf :: FilePath -> FilePath -> IO ()
svgToPdf inp out = do
  svgPtr <- loadSvg inp
  (sizeX, sizeY) <- dimensions svgPtr
  success <- withPDFSurface out (fromIntegral sizeX) (fromIntegral sizeY) $
    \pdf -> renderWith pdf (render svgPtr)
  if success
    then return ()
    else fail "svgToPdf: Unknown error"
