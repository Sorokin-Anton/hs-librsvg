{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SvgToPdf where

import Graphics.Rendering.Cairo.LibRSvg
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Graphics.Rendering.Cairo (withPDFSurface, renderWith)

class CanContainInput a where
  readInput :: a -> IO ByteString

instance CanContainInput FilePath where
  readInput = BS.readFile

instance CanContainInput ByteString where
  readInput = return

loadSvg :: CanContainInput a => a -> IO Svg
loadSvg inp = do
  result <- fromBuffer =<< readInput inp
  case result of
    Left bs -> fail $ BSC.unpack bs
    Right sp -> pure sp

svgToPdf :: CanContainInput a => a -> FilePath -> IO ()
svgToPdf inp out = do
  svgPtr <- loadSvg inp
  (sizeX, sizeY) <- dimensions svgPtr
  success <- withPDFSurface out (fromIntegral sizeX) (fromIntegral sizeY) $
    \pdf -> renderWith pdf (render svgPtr)
  if success
    then return ()
    else fail "svgToPdf: Unknown error"
