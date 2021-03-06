{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module LaTeXBase
 ( MathType(..), LaTeXUnit(..), LaTeX, TeXArg, ArgKind(..), concatRaws, hasCommand
 , matchCommand, lookForCommand, matchEnv, mapTeX, renderLaTeX, mapTeXRaw, isTeXEnv
 , trim, trimr, texStripInfix, isCodeblock, isMath, needsSpace, texStripPrefix, allUnits ) where

import Data.Monoid ((<>))
import Data.String (fromString)
import Prelude hiding ((.), (++), writeFile, dropWhile)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Util
import Data.Char (isSpace, isAlphaNum)
import Util ((.), (++), textStripInfix, dropTrailingWs)
import Control.Arrow (first, second)

data MathType = Parentheses | Square | Dollar
	deriving (Eq, Show)

data ArgKind = FixArg | OptArg
	deriving (Eq, Show)

type TeXArg = (ArgKind, LaTeX)

data LaTeXUnit
	= TeXRaw Text
	| TeXComm String [TeXArg]
	| TeXEnv String [TeXArg] LaTeX
	| TeXMath MathType LaTeX
	| TeXLineBreak
	| TeXBraces LaTeX
	deriving (Eq, Show)

isTeXEnv :: String -> LaTeXUnit -> Bool
isTeXEnv x (TeXEnv y _ _) = x == y
isTeXEnv _ _ = False

type LaTeX = [LaTeXUnit]

lookForCommand :: String -> LaTeX -> [[TeXArg]]
lookForCommand n = (snd .) . matchCommand (n ==)

allUnits :: LaTeX -> [LaTeXUnit]
allUnits [] = []
allUnits (x : y) = x : z ++ allUnits y
	where
		z = case x of
			TeXMath _ l -> allUnits l
			TeXBraces l -> allUnits l
			TeXComm _ a -> (snd . a) >>= allUnits
			TeXEnv _ a l -> (l : snd . a) >>= allUnits
			_ -> []

matchCommand :: (String -> Bool) -> LaTeX -> [(String,[TeXArg])]
matchCommand f x = [(str, as) | TeXComm str as <- allUnits x, f str]

hasCommand :: (String -> Bool) -> LaTeX -> Bool
hasCommand f = not . null . matchCommand f

matchEnv :: (String -> Bool) -> LaTeX -> [(String,[TeXArg],LaTeX)]
matchEnv f x = [(str, as, l) | TeXEnv str as l <- allUnits x, f str]

mapTeX :: (LaTeXUnit -> Maybe LaTeX) -> LaTeX -> LaTeX
mapTeX f = concatMap g
	where
		g :: LaTeXUnit -> LaTeX
		g (f -> Just x) = x
		g (TeXComm c a) = [TeXComm c (h . a)]
		g (TeXBraces x) = [TeXBraces (mapTeX f x)]
		g (TeXMath t b) = [TeXMath t (mapTeX f b)]
		g (TeXEnv n a b) = [TeXEnv n (h . a) (mapTeX f b)]
		g x = [x]
		h = second (mapTeX f)

renderLaTeX :: LaTeX -> Text
renderLaTeX = mconcat . (renderUnit .)

renderUnit :: LaTeXUnit -> Text
renderUnit (TeXRaw t) = t
renderUnit (TeXComm name [])
	| dropTrailingWs name `elem` ["left", "right", "bigl", "bigr", "big", "small", "smaller"] = pack $ "\\" <> name
	| otherwise = "\\" <> fromString name <> "{}"
renderUnit (TeXComm name args) = "\\" <> pack (fromString name) <> renderArgs args
renderUnit (TeXEnv name args c) =
	"\\begin{" <> fromString name <> "}"
	<> renderArgs args
	<> renderLaTeX c
	<> "\\end{" <> fromString name <> "}"
renderUnit (TeXMath Dollar l) = "$" <> renderLaTeX l <> "$"
renderUnit (TeXMath Square l) = "\\[" <> renderLaTeX l <> "\\]"
renderUnit (TeXMath Parentheses l) = "\\(" <> renderLaTeX l <> "\\)"
renderUnit TeXLineBreak = "\\\\"
renderUnit (TeXBraces l) = "{" <> renderLaTeX l <> "}"

renderArgs :: [TeXArg] -> Text
renderArgs = mconcat . (renderArg .)

renderArg :: TeXArg -> Text
renderArg (FixArg, l) = "{" <> renderLaTeX l <> "}"
renderArg (OptArg, l) = "[" <> renderLaTeX l <> "]"

mapTeXRaw :: (Text -> LaTeXUnit) -> (LaTeX -> LaTeX)
mapTeXRaw f = map go
	where
		go :: LaTeXUnit -> LaTeXUnit
		go (TeXRaw t) = f t
		go (TeXComm s args) = TeXComm s (second (go .) . args)
		go (TeXEnv s args body) = TeXEnv s (second (go .) . args) (go . body)
		go (TeXBraces l) = TeXBraces $ go . l
		go t@(TeXMath _ _) = t
		go t@TeXLineBreak = t

concatRaws :: LaTeX -> LaTeX
concatRaws (TeXRaw a : TeXRaw b : more) = concatRaws (TeXRaw (a ++ b) : more)
concatRaws (TeXComm s args : more) = TeXComm s (second concatRaws . args) : concatRaws more
concatRaws (TeXEnv s args bd : more) = TeXEnv s (second concatRaws . args) (concatRaws bd) : concatRaws more
concatRaws (TeXBraces x : more) = TeXBraces (concatRaws x) : concatRaws more
concatRaws (x : more) = x : concatRaws more
concatRaws [] = []

texStripPrefix :: Text -> LaTeX -> Maybe LaTeX
texStripPrefix t (TeXRaw s : y) = case Text.stripPrefix t s of
	Just "" -> Just y
	Just s' -> Just (TeXRaw s' : y)
	Nothing -> Nothing
texStripPrefix _ _ = Nothing

texStripInfix :: Text -> LaTeX -> Maybe (LaTeX, LaTeX)
texStripInfix t = go
	where
		go [] = Nothing
		go (x : rest)
			| TeXRaw s <- x
			, Just (y, z) <- textStripInfix t s
				= Just (h y, h z ++ rest)
			| otherwise = first (x :) . go rest
		h "" = []
		h x = [TeXRaw x]

dropWhile :: (Char -> Bool) -> LaTeX -> LaTeX
dropWhile p (TeXRaw x : y) = case Text.dropWhile p x of
	"" -> dropWhile p y
	x' -> TeXRaw x' : y
dropWhile _ x = x

invisible :: LaTeXUnit -> Bool
invisible (TeXComm "index" _) = True
invisible _ = False

dropWhileEnd :: (Char -> Bool) -> LaTeX -> LaTeX
dropWhileEnd _ [] = []
dropWhileEnd p x
	| invisible (last x) = dropWhileEnd p (init x) ++ [last x]
	| TeXRaw y <- last x = init x ++ case Text.dropWhileEnd p y of
		"" -> []
		a -> [TeXRaw a]
	| otherwise = x

-- These dropWhile and dropWhileEnd only make a half-hearted effort, in that
-- they don't handle adjacent TeXRaws, but we don't need that.

triml, trimr, trim :: LaTeX -> LaTeX
triml = dropWhile isSpace
trimr = dropWhileEnd isSpace
trim = triml . trimr

isMath :: LaTeXUnit -> Bool
isMath (TeXMath _ _) = True
isMath (TeXComm "ensuremath" _) = True
isMath (TeXEnv "eqnarray*" _ _) = True
isMath _ = False

isCodeblock :: LaTeXUnit -> Bool
isCodeblock (TeXEnv "codeblock" _ _) = True
isCodeblock (TeXEnv "codeblockdigitsep" _ _) = True
isCodeblock _ = False

needsSpace :: LaTeX -> Bool
	-- In the sense of \xspace
needsSpace [] = False
needsSpace (z : y)
	| invisible z = needsSpace y
	| otherwise = go z
	where
		go :: LaTeXUnit -> Bool
		go (TeXMath _ x) = needsSpace x
		go (TeXComm "link" [(_, x), _]) = needsSpace x
		go (TeXComm "texttt" _) = True
		go (TeXComm "mathsf" [(_, x)]) = needsSpace x
		go (TeXComm "mathscr" [(_, x)]) = needsSpace x
		go (TeXComm "tcode" [(_, x)]) = needsSpace x
		go (TeXComm "textit" [(_, x)]) = needsSpace x
		go (TeXComm "grammarterm_" _) = True
		go (TeXComm "index" _) = False
		go (TeXComm "sqrt" _) = True
		go (TeXComm "ensuremath" ((_, x) : _)) = needsSpace x
		go (TeXComm "texorpdfstring" [_, (_, x)]) = needsSpace x
		go (TeXComm " " []) = False
		go (TeXComm "," []) = False
		go (TeXComm "~" []) = True
		go (TeXComm "&" []) = False
		go (TeXBraces x) = needsSpace x
		go (TeXRaw t) = Util.startsWith (\c -> isAlphaNum c || (c `elem` ("~&-!*(" :: String))) t
		go _ = True -- error $ "needsSpace: unexpected: " ++ show x
