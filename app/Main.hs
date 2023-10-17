{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Lib (toGigabytes)
import Tipos
import Text.Printf
import Data.List (sortBy)
import Control.Monad (forM)
import System.FilePath ((</>))
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Data.Function (on)
import System.Posix.Files (FileStatus, isDirectory, isRegularFile, isSymbolicLink,
                          getSymbolicLinkStatus, fileSize)
import System.Posix.Types (FileOffset)

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

archivosCompletosR :: FilePath -> IO [ArchivoCompleto]
archivosCompletosR rutaIn = do
  rutas <- rutaCompleta rutaIn
  archivosComp <- forM rutas $ \unArchivo -> do
    tipoA <- tipoArchivo unArchivo
    tamanoA <- tamanoArchivo unArchivo
    if (tipoA == Directorio)
      then archivosCompletosR unArchivo
      else return [Archivos tipoA unArchivo tamanoA]
  return $ concat archivosComp

archivosCompletosSum :: FilePath -> IO [ArchivoCompleto]
archivosCompletosSum rutaIn = do
  rutas <- rutaCompleta rutaIn
  archivosComp <- forM rutas $ \unArchivo -> do
    tipoA <- tipoArchivo unArchivo
    tamanoA <- tamanoArchivo unArchivo
    if (tipoA == Directorio)
      then do
        completoInterno <- archivosCompletosSum unArchivo
        let tamanoInterno = tamano <$> completoInterno
            tamanoInSum = foldr (+) 4096 tamanoInterno
        return [Archivos tipoA unArchivo tamanoInSum]
      else return [Archivos tipoA unArchivo tamanoA]
  return $ concat archivosComp

tamanoArchivos :: FilePath -> (FilePath -> IO [ArchivoCompleto]) -> IO [ArchivoCompleto]
tamanoArchivos rutaIn fn = do
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
    Directorio -> fn rutaIn

rutasYtamanos :: [ArchivoCompleto] -> [[String]]
rutasYtamanos [] = []
rutasYtamanos archivosComp = salidaHumana
  where
    rutasIn = ruta <$> archivosComp
    tamanosIn = tamano <$> archivosComp
    paraOrdenar = zip rutasIn tamanosIn
    ordenado = ordenLista paraOrdenar
    salidaHumana = toGigabytes ordenado

maxLongitud :: [[String]] -> [Int]
maxLongitud [] = []
maxLongitud (x:xs) = length (head x) : maxLongitud xs

maximoLista :: (Foldable t, Ord b, Num b) => t b -> b
maximoLista lstEnteros = foldr max 0 lstEnteros

salidaHumanaSum :: FilePath -> IO ()
salidaHumanaSum rutaIn = do
  archivos <- rutasYtamanos <$> tamanoArchivos rutaIn archivosCompletosSum
  let maximo = maximoLista . maxLongitud $ archivos
      salidaIn = (\x -> printf ("%-" <> show maximo <> "s%5s") (head x) (head . tail $ x)) <$> archivos
  mapM_ putStrLn salidaIn

salidaHumanaTodo :: FilePath -> IO ()
salidaHumanaTodo rutaIn = do
  archivos <- rutasYtamanos <$> tamanoArchivos rutaIn archivosCompletosR
  let salidaIn = (\archivo -> printf "%-150s%11s" (head archivo) (head . tail $ archivo)) <$> archivos
  mapM_ putStrLn salidaIn

usoExtendido :: IO ()
usoExtendido = putStr . unlines $
  [ "LO(1)"
  ,""
  ,"Nombre"
  ,"     lo - Lista archivos Ordenados por tamaño."
  ,""
  ,"RESUMEN"
  ,"    lo ruta o path"
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
  ,"    -r ruta"
  ,"      Muestra todos los archivos de un árbol de directorios."
  ,""
  ,"AUTOR"
  ,"    Escrito por Charte."
  ,""
  ,"Reporte de bugs"
  ,"    En el repositorio https://github.com/Carht/lo"
  ,"    Ruta alternativa: <echarte@tutanota.com>"
  ,""
  ,"COPYRIGHT"
  ,"  Copyright (R) 2023. Licencia GPLv3+: GNU GPL version 3 o posteriores <https://gnu.org/licenses/gpl.html>"
  ,""
  ]

usoResumen :: IO ()
usoResumen = putStr . unlines $
  [ "RESUMEN"
  , "    lo ruta o path"
  , "    lo [-h]"
  , "    lo [-v | --version]"
  , "    lo -r ruta"
  ]

version :: IO ()
version = putStr . unlines $
  ["Lista archivos por orden de tamaño 0.1.2"]
  
main :: IO ()
main = getArgs >>= \case
  [] -> usoResumen >> exitWith (ExitFailure 1)
  ["-h"] -> usoExtendido >> exitWith (ExitFailure 1)
  ["-v"] -> version >> exitWith (ExitFailure 1)
  ["--version"] -> version >> exitWith (ExitFailure 1)
  ["-r", rutaIn] -> salidaHumanaTodo rutaIn
  [rutaIn] -> salidaHumanaSum rutaIn
