{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where


import Options.Generic
import SvgToPdf

data Options w = Options
  {from :: w ::: FilePath <?> "Path to the existing SVG file"
  , to :: w ::: FilePath <?> "Path to pdf file which will be created"}
  deriving stock Generic

instance ParseRecord (Options Wrapped)

main :: IO ()
main = do
  opt <- unwrapRecord "svgtopdf"
  svgToPdf (from opt) (to opt)
