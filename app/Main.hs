module Main (main) where

import Lib
import Control.Monad (forM)
import System.FilePath ((</>))
import System.Directory (listDirectory)
import System.Posix.Files (FileStatus, isDirectory, isRegularFile, isSymbolicLink,
                          getSymbolicLinkStatus, fileSize)
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

tamanoArchivo :: FilePath -> IO FileOffset
tamanoArchivo ruta = do
  estatus <- getSymbolicLinkStatus ruta
  return $ fileSize estatus

rutaCompleta :: FilePath -> IO [FilePath]
rutaCompleta ruta = do
  archivos <- listDirectory ruta
  archivosRutas <- forM archivos $ \nombreSimple -> do
    let rutaCompleta = ruta </> nombreSimple
    return [rutaCompleta]
  return $ concat archivosRutas

archivosCompletos :: FilePath -> IO [ArchivoCompleto]
archivosCompletos ruta = do
  rutas <- rutaCompleta ruta
  archivosComp <- forM rutas $ \unArchivo -> do
    tipo <- tipoArchivo unArchivo
    tamano <- tamanoArchivo unArchivo
    return [Archivos tipo unArchivo tamano]
  return $ concat archivosComp
  
main :: IO ()
main = someFunc