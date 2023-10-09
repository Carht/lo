module Main (main) where

import Lib
import System.Posix.Files (FileStatus, isDirectory, isRegularFile, isSymbolicLink,
                          getSymbolicLinkStatus)
import System.Posix.Types (FileOffset)

data TipoArchivo = Directorio | Archivo | LinkSimbolico | Otro deriving (Show, Eq)

data ArchivoCompleto = Archivos {
  tipo :: TipoArchivo,
  ruta :: FilePath,
  tamano :: FileOffset
  } deriving (Show, Eq)

estatusArchivo :: FileStatus -> TipoArchivo
estatusArchivo estatus
  | isDirectory estatus    = Directorio
  | isSymbolicLink estatus = LinkSimbolico
  | isRegularFile estatus  = Archivo
  | otherwise              = Otro

tipoArchivo :: FilePath -> IO TipoArchivo
tipoArchivo ruta = do
  estatus <- getSymbolicLinkStatus ruta
  return $ estatusArchivo estatus

main :: IO ()
main = someFunc
