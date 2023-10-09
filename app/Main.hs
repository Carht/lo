module Main (main) where

import Lib


data TipoArchivo = Directorio | Archivo | LinkSimbolico | Otro deriving (Show, Eq)

main :: IO ()
main = someFunc
