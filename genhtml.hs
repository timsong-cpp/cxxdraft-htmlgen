{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedStrings #-}

import Render (imgDir, outputDir, SectionFileStyle(..))
import Document (Draft(..), figures)
import Load14882 (load14882)
import Prelude hiding ((++), (.), writeFile)
import System.Directory (createDirectoryIfMissing, copyFile, setCurrentDirectory, getCurrentDirectory)
import System.Environment (getArgs)
import Util

import Toc (writeTocFile)
import SectionPages (writeSectionFiles, writeFullFile, writeFiguresFile, writeTablesFile, writeIndexFiles, writeFootnotesFile)

data CmdLineArgs = CmdLineArgs
	{ repo :: FilePath
	, sectionFileStyle :: SectionFileStyle }

readCmdLineArgs :: [String] -> CmdLineArgs
readCmdLineArgs = \case
	[repo, read -> sectionFileStyle] -> CmdLineArgs{..}
	[repo] -> CmdLineArgs{sectionFileStyle=WithExtension,..}
	_ -> error "param: path/to/repo"

main :: IO ()
main = do
	cwd <- getCurrentDirectory
	CmdLineArgs{..} <- readCmdLineArgs . getArgs

	setCurrentDirectory $ repo ++ "/src"
	draft@Draft{..} <- load14882

	setCurrentDirectory cwd
	putStrLn $ "Writing to " ++ outputDir
	createDirectoryIfMissing True outputDir
	createDirectoryIfMissing True (outputDir ++ imgDir)
	copyFile "14882.css" (outputDir ++ "/14882.css")
	writeTocFile sectionFileStyle draft
	writeIndexFiles sectionFileStyle index
	writeFiguresFile sectionFileStyle (figures draft)
	writeTablesFile sectionFileStyle draft
	writeFootnotesFile sectionFileStyle draft
	writeFullFile sectionFileStyle draft
	writeSectionFiles sectionFileStyle draft
