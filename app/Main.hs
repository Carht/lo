{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Lib()
import Data.List (sort, sortBy)
import Control.Monad (forM)
import System.FilePath ((</>))
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Data.Function (on)
import System.Posix.Files (FileStatus, isDirectory, isRegularFile, isSymbolicLink,
                          getSymbolicLinkStatus, fileSize)
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
    |  s < z = LT
    |  s > z = GT

estatusArchivo :: FileStatus -> TipoArchivo
estatusArchivo estatus
  | isDirectory estatus    = Directorio
  | isSymbolicLink estatus = LinkSimbolico
  | isRegularFile estatus  = Archivo
  | otherwise              = Otro

ordenLista :: Ord b => [(a, b)] -> [(a, b)]
ordenLista = sortBy $ flip $ on compare snd

tipoArchivo :: FilePath -> IO TipoArchivo
tipoArchivo rutaIn = do
  estatus <- getSymbolicLinkStatus rutaIn
  return $ estatusArchivo estatus

tamanoArchivo :: FilePath -> IO FileOffset
tamanoArchivo rutaIn = do
  estatus <- getSymbolicLinkStatus rutaIn
  return $ fileSize estatus

rutaCompleta :: FilePath -> IO [FilePath]
rutaCompleta rutaIn = do
  archivos <- listDirectory rutaIn
  archivosRutas <- forM archivos $ \nombreSimple -> do
    let rutaComp = rutaIn </> nombreSimple
    return [rutaComp]
  return $ concat archivosRutas

archivosCompletos :: FilePath -> IO [ArchivoCompleto]
archivosCompletos rutaIn = do
  rutas <- rutaCompleta rutaIn
  archivosComp <- forM rutas $ \unArchivo -> do
    tipoA <- tipoArchivo unArchivo
    tamanoA <- tamanoArchivo unArchivo
    return [Archivos tipoA unArchivo tamanoA]
  return $ concat archivosComp

archivosCompletosOrd :: FilePath -> IO [ArchivoCompleto]
archivosCompletosOrd rutaIn = do
  rutas <- archivosCompletos rutaIn
  return $ sort rutas

tamanoArchivos :: FilePath -> IO [ArchivoCompleto]
tamanoArchivos rutaIn = do
  tipoArch <- tipoArchivo rutaIn
  case tipoArch of
    Archivo -> do
      tamanoArch <- tamanoArchivo rutaIn
      return [Archivos tipoArch rutaIn tamanoArch]
    LinkSimbolico -> do
      tamanoLink <- tamanoArchivo rutaIn
      return [Archivos tipoArch rutaIn tamanoLink]
    Otro -> do
      tamanoOtro <- tamanoArchivo rutaIn
      return [Archivos tipoArch rutaIn tamanoOtro]
    Directorio -> archivosCompletosOrd rutaIn

soloTamanos :: FilePath -> IO [FileOffset]
soloTamanos rutaIn = do
  archivosComp <- tamanoArchivos rutaIn
  let tamanoIn = tamano <$> archivosComp
  return tamanoIn

sumTamanos :: FilePath -> IO FileOffset
sumTamanos rutaIn = do
  tamanos <- soloTamanos rutaIn
  case tamanos of
    []        -> return $ 4096
    _ -> return $ foldr (+) 0 tamanos

tamanoArchivosR :: FilePath -> IO [(FilePath, FileOffset)]
tamanoArchivosR rutaIn = do
  rutas <- tamanoArchivos rutaIn
  tamanos <- mapM sumTamanos $ ruta <$> rutas
  let soloRutas = ruta <$> rutas
  return $ zip soloRutas tamanos

tamanoOrd :: FilePath -> IO [(FilePath, FileOffset)]
tamanoOrd rutaIn =
  tamanoArchivosR rutaIn >>= \archivoLista -> let {ordenada = ordenLista archivoLista} in return ordenada

archivosIO :: FilePath -> IO [String]
archivosIO rutaIn = do
  archivos <- tamanoOrd rutaIn
  return $ map show archivos

salida :: FilePath -> IO ()
salida rutaIn = do
  archivos <- archivosIO rutaIn
  putStrLn . unlines $ archivos
  
main :: IO ()
main = getArgs >>= \case
  [] -> print "ok"
  [rutaIn] -> salida rutaIn
