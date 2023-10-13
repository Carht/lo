module Tipos
  (
    TipoArchivo(..),
    ArchivoCompleto(..),
  ) where

import System.Posix.Types (FileOffset)

data TipoArchivo = Directorio | Archivo | LinkSimbolico | Otro deriving (Show, Eq)

data ArchivoCompleto = Archivos {
  tipo :: TipoArchivo,
  ruta :: FilePath,
  tamano :: FileOffset
  } deriving (Show, Eq)

instance Ord ArchivoCompleto where
  compare (Archivos {tipo = t, ruta = a, tamano = s}) (Archivos {tipo = u, ruta = b, tamano = z})
    | t == u = EQ
    | a == b = EQ
    | s == z = EQ
    | s < z  = LT
    | s > z  = GT

