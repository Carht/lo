module Main (main) where

import Lib


data TipoArchivo = Directorio | Archivo | LinkSimbolico | Otro deriving (Show, Eq)

data ArchivoCompleto = Archivos {
  tipo :: TipoArchivo,
  ruta :: FilePath,
  tamano :: FileOffset
  } deriving (Show, Eq)

main :: IO ()
main = someFunc
