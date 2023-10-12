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

-- Busca archivos completos en un directorio inmediato, similar a "ls"
archivosCompletos :: FilePath -> IO [ArchivoCompleto]
archivosCompletos rutaIn = do
  rutas <- rutaCompleta rutaIn
  archivosComp <- forM rutas $ \unArchivo -> do
    tipoA <- tipoArchivo unArchivo
    tamanoA <- tamanoArchivo unArchivo
    return [Archivos tipoA unArchivo tamanoA]
  return $ concat archivosComp

-- Busca en un árbol de directorios recursivamente
archivosCompletosR :: FilePath -> IO [ArchivoCompleto]
archivosCompletosR rutaIn = do
  rutas <- rutaCompleta rutaIn
  archivosComp <- forM rutas $ \unArchivo -> do
    tipoA <- tipoArchivo unArchivo
    tamanoA <- tamanoArchivo unArchivo
    return [Archivos tipoA unArchivo tamanoA]
    if (tipoA == Directorio)
      then archivosCompletosR unArchivo
      else return [Archivos tipoA unArchivo tamanoA]
  return $ concat archivosComp

-- Busca en un árbol de directorios pero realiza un compendio de la suma de los subdirectorios
archivosCompletosSum :: FilePath -> IO [ArchivoCompleto]
archivosCompletosSum rutaIn = do
  rutas <- rutaCompleta rutaIn
  archivosComp <- forM rutas $ \unArchivo -> do
    tipoA <- tipoArchivo unArchivo
    tamanoA <- tamanoArchivo unArchivo
    return [Archivos tipoA unArchivo tamanoA]
    if (tipoA == Directorio)
      then do
        completoInterno <- archivosCompletosSum unArchivo
        let tamanoInterno = tamano <$> completoInterno
            tamanoInSum = foldr (+) 0 tamanoInterno
        return [Archivos tipoA unArchivo tamanoInSum]
      else return [Archivos tipoA unArchivo tamanoA]
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
    Directorio -> archivosCompletosSum rutaIn

soloTamanos :: FilePath -> IO [FileOffset]
soloTamanos rutaIn = do
  archivosComp <- tamanoArchivos rutaIn
  let tamanoIn = tamano <$> archivosComp
  return tamanoIn

sumTamanos :: FilePath -> IO FileOffset
sumTamanos rutaIn =
  soloTamanos rutaIn >>= \case
    [] -> return $ 4096
    tamanos -> return $ foldr (+) 0 tamanos

tamanoArchivosR :: FilePath -> IO [(FilePath, FileOffset)]
tamanoArchivosR rutaIn = do
  rutas <- tamanoArchivos rutaIn
  tamanos <- mapM sumTamanos $ ruta <$> rutas
  let soloRutas = ruta <$> rutas
  return $ zip soloRutas tamanos

tamanoOrd :: FilePath -> IO [(FilePath, FileOffset)]
tamanoOrd rutaIn = ordenLista <$> tamanoArchivosR rutaIn

archivoToStr :: Show b => [(String, b)] -> [String]
archivoToStr [] = []
archivoToStr (x:xs) = [fst x, show . snd $ x] <> archivoToStr xs

unirTiposStr :: [a] -> [[a]]
unirTiposStr [] = []
unirTiposStr (a:b:r) = [a:b:[]] <> unirTiposStr r

salidaFormato :: [[String]] -> [String]
salidaFormato [] = []
salidaFormato (h:t) = [head h <> " \t\t" <> unwords (tail h)] <> salidaFormato t

salida :: FilePath -> IO ()
salida rutaIn = do
  archivos <- salidaFormato <$> unirTiposStr <$> archivoToStr <$> tamanoOrd rutaIn
  putStrLn . unlines $ archivos

usoExtendido :: IO ()
usoExtendido = putStr . unlines $
  [ "LO(1)"
  ,""
  ,"Nombre"
  ,"     lo - Lista archivos Ordenados por tamaño."
  ,""
  ,"RESUMEN"
  ,"    lg Archivo | Directorio"
  ,""
  ,"DESCRIPCIÓN"
  ,"    Lista archivos ordenados de mayor a menor."
  ,
   ""
  ,"    -h"
  ,"      Retorna este texto."
  ,""
  ,"    -v | --version"
  ,"      Muestra la versión del software."
  ,""
  ,"AUTOR"
  ,"    Escrito por Charte."
  ,""
  ,"Reporte de bugs"
  ,"    En el repositorio https://gitlab.canaima.softwarelibre.gob.ve/lo"
  ,"    Ruta alternativa: <echarte@tutanota.com>"
  ,""
  ,"COPYRIGHT"
  ,"  Copyright (R) 2023. Licencia GPLv3+: GNU GPL version 3 o posteriores <https://gnu.org/licenses/gpl.html>"
  ,""
  ]

usoResumen :: IO ()
usoResumen = putStr . unlines $
  [ "RESUMEN"
  , "    ./lo ruta"
  ]

version :: IO ()
version = putStr . unlines $
  ["Lista archivos por orden de tamaño 0.1.0"]
  
main :: IO ()
main = getArgs >>= \case
  [] -> usoResumen >> exitWith (ExitFailure 1)
  ["-h"] -> usoExtendido >> exitWith (ExitFailure 1)
  ["-v"] -> version >> exitWith (ExitFailure 1)
  ["--version"] -> version >> exitWith (ExitFailure 1)
  [rutaIn] -> salida rutaIn
