{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections #-}

module LaTeXUtil where

import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), texmap)
import Prelude hiding ((.), (++), writeFile, dropWhile)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Util
import Data.Maybe (isJust, fromJust)
import Data.Map (Map)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Map as Map
import Util ((.), (++), getDigit, stripInfix)
import Control.Arrow (first)

texFromArg :: TeXArg -> LaTeX
texFromArg (FixArg t) = t
texFromArg (OptArg t) = t
texFromArg (SymArg t) = t
texFromArg _ = error "no"

mapTeXArg :: (LaTeX -> LaTeX) -> (TeXArg -> TeXArg)
mapTeXArg f (FixArg t) = FixArg (f t)
mapTeXArg f (OptArg t) = OptArg (f t)
mapTeXArg _ x = x

texTail :: LaTeX -> LaTeX
texTail (TeXSeq _ t) = t
texTail _ = error "Not a sequence"

mapTeXRaw :: (Text -> LaTeX) -> (LaTeX -> LaTeX)
mapTeXRaw f = go
	where
		go :: LaTeX -> LaTeX
		go (TeXRaw t) = f t
		go (TeXComm s args) = TeXComm s (mapTeXArg go . args)
		go (TeXEnv s args body) = TeXEnv s (mapTeXArg go . args) (go body)
		go (TeXBraces l) = TeXBraces $ go l
		go TeXEmpty = TeXEmpty
		go (TeXSeq x y) = TeXSeq (go x) (go y)
		go t@(TeXCommS _) = t
		go t@(TeXComment _) = t
		go t@(TeXMath _ _) = t
		go t@(TeXLineBreak _ _) = t

mapTeX :: (LaTeX -> Maybe LaTeX) -> (LaTeX -> LaTeX)
mapTeX f = texmap (isJust . f) (fromJust . f)

concatRaws :: LaTeX -> LaTeX
concatRaws l =
		let (a, b) =  go "" l
		in if b == "" then a else a ++ TeXRaw b
	where
		ppp :: Text -> (LaTeX -> LaTeX)
		ppp "" = id
		ppp t = TeXSeq (TeXRaw t)
		go :: Text -> LaTeX -> (LaTeX, Text)
		go pre t@TeXEmpty = (t, pre)
		go pre (TeXRaw s) = (TeXEmpty, pre ++ s)
		go pre (TeXEnv s args bd) = (ppp pre $ TeXEnv s (mapTeXArg concatRaws . args) (concatRaws bd), "")
		go pre (TeXComm s args) = (ppp pre $ TeXComm s (mapTeXArg concatRaws . args), "")
		go pre (TeXCommS s) = (ppp pre $ TeXCommS s, "")
		go pre (TeXBraces x) = (ppp pre $ TeXBraces (concatRaws x), "")
		go pre (TeXComment t) = (ppp pre $ TeXComment t, "")
		go pre (TeXMath m t) = (ppp pre $ TeXMath m t, "")
		go pre t@(TeXLineBreak _ _) = (ppp pre t, "")
		go pre (TeXSeq x y) =
			let
				(x', s) = go pre x
				(y', s') = go s y
			in
				(x' ++ y', s')

data Command = Command
	{ arity :: !Int
	, body :: !LaTeX }
	deriving Show

data Environment = Environment
	{ begin, end :: !LaTeX
	, defaultArgs :: ![LaTeX] }
	deriving Show

data Macros = Macros
	{ commands :: Map String Command
	, environments :: Map Text Environment
	, counters :: Map Text Int }
	deriving Show

instance Monoid Macros where
	mempty = Macros mempty mempty mempty
	mappend x y = Macros (commands x ++ commands y) (environments x ++ environments y) (counters x ++ counters y)

recognizeEnvs :: LaTeX -> LaTeX
recognizeEnvs (TeXSeq b@(TeXComm "begin" (FixArg (TeXRaw n) : aa)) rest) =
		case findEnd n rest of
			Nothing -> b ++ recognizeEnvs rest
			Just (x, y) -> TeXSeq (TeXEnv (unpack n) aa (recognizeEnvs x)) (recognizeEnvs y)
	where
		findEnd :: Text -> LaTeX -> Maybe (LaTeX, LaTeX)
		findEnd n' (TeXSeq (TeXComm "end" [FixArg (TeXRaw m)]) rest') | n' == m =
			Just (TeXEmpty, rest')
		findEnd n' (TeXSeq x y) = first (TeXSeq x) . findEnd n' y
		findEnd _ _ = Nothing
recognizeEnvs (TeXSeq x y) = recognizeEnvs x ++ recognizeEnvs y
recognizeEnvs z = z

extractText :: LaTeX -> Text
extractText (TeXRaw s) = s
extractText (TeXSeq a b) = extractText a ++ extractText b
extractText TeXEmpty = ""
extractText _ = error "extractText"

replArgs :: [LaTeX] -> LaTeX -> LaTeX
replArgs args = mapTeXRaw (replaceArgsInString args . unpack)

replaceArgsInString :: [LaTeX] -> String -> LaTeX
replaceArgsInString args = concatRaws . go
	where
		go :: String -> LaTeX
		go ('#':'#':more) = TeXRaw "#" ++ go more
		go ('#':c:more)
			| Just i <- getDigit c =
			(args !! (i-1)) ++
			go more
		go (c : more) = TeXRaw (pack [c]) ++ go more
		go [] = TeXEmpty

eval :: Macros -> [Text] -> LaTeX -> (LaTeX, Macros)
eval macros@Macros{..} dontEval l = case l of

	TeXEnv e a stuff
		| Just Environment{..} <- Map.lookup (pack e) environments
		, not (pack e `elem` dontEval) -> (, mempty) $
			(if e == "TableBase" then TeXEnv e [] else id) $
			fst $ ev $
			recognizeEnvs $
				replArgs ((fst . ev . texFromArg . a) ++ defaultArgs) begin
					-- Note: Just appending defaultArgs is wrong in general but
					--       correct for the couple of uses we're dealing with.
				++ stuff
				++ end
		| otherwise -> (, mempty) $ TeXEnv e (mapTeXArg (fst . ev) . a) (fst $ ev stuff)

	x@(TeXRaw _) -> (x, mempty)

	TeXCommS "ungap" -> mempty

	TeXComm "newcounter" [FixArg name']
		| name <- extractText name' ->
			(mempty, Macros mempty mempty (Map.singleton name 0))

	TeXComm "setcounter" [FixArg name', FixArg newValue']
		| name <- extractText name'
		, newValue <- extractText newValue' ->
			(mempty, Macros mempty mempty (Map.singleton name (read $ unpack newValue)))

	TeXComm "addtocounter" [FixArg name', FixArg  addend']
		| name <- extractText name'
		, addend <- extractText addend'
		, Just value <- Map.lookup name counters ->
			(mempty, Macros mempty mempty (Map.singleton name (value + (read $ unpack addend))))

	TeXComm "value" [FixArg name']
		| name <- extractText name'
		, Just value <- Map.lookup name counters ->
			(TeXRaw $ pack $ show value, macros)
		| otherwise -> error $ "value: No such counter: " ++ show (concatRaws name')

	TeXComm "newenvironment" [FixArg (TeXRaw name), FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e [])) mempty)
	TeXComm "newenvironment" [FixArg (TeXRaw name), OptArg _, FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e [])) mempty)
	TeXComm "newenvironment" [FixArg (TeXRaw name), OptArg _, OptArg x, FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e [x])) mempty)
	TeXComm "newenvironment" x -> error $ "unrecognized newenv: " ++ show x
	TeXComm "newcommand" (FixArg (TeXCommS s) : _)
		| pack s `elem` dontEval -> mempty
	TeXComm "newcommand" [FixArg (TeXCommS name), OptArg (TeXRaw argcount), FixArg body]
		-> (mempty, Macros (Map.singleton name (Command (read (unpack argcount)) body)) mempty mempty)
	TeXComm "newcommand" [FixArg (TeXCommS name), FixArg body]
		-> (mempty, Macros (Map.singleton name (Command 0 body)) mempty mempty)

	TeXComm c args
		| Just Command{..} <- Map.lookup c commands
		, length args >= arity ->
			let
				(x,y) = splitAt arity args
				body' :: LaTeX
				body' = replArgs (map (fst . ev . texFromArg) x) body
			in
				(, mempty) $ fst (ev body') ++ mconcat (map (fst . ev . texFromArg) y)
	TeXComm c [] -> ev (TeXComm c [FixArg TeXEmpty])
	TeXComm c args -> (TeXComm c (mapTeXArg (fst . ev) . args), mempty)
	TeXCommS c
		| Just Command{..} <- Map.lookup c commands
		, arity == 0 -> ev body
	TeXSeq x y ->
		let (x', m) = ev x in
		let (y', m') = eval (m ++ macros) dontEval y in
			(x' ++ y', m' ++ m)
	TeXBraces x -> (TeXBraces $ fst $ ev x, mempty)
	TeXMath t m -> (TeXMath t $ fst $ ev m, mempty)
	x -> (x, mempty)
  where ev = eval macros dontEval

texStripInfix :: Text -> LaTeX -> Maybe (LaTeX, LaTeX)
texStripInfix t = go
	where
		go (TeXRaw (unpack -> stripInfix (unpack t) -> Just ((pack -> x), (pack -> y))))
			= Just (TeXRaw x, TeXRaw y)
		go (TeXSeq x y)
			| Just (x', x'') <- go x = Just (x', x'' ++ y)
			| Just (y', y'') <- go y = Just (x ++ y', y'')
		go _ = Nothing

rmseqs :: LaTeX -> [LaTeX]
rmseqs (TeXSeq x y) = rmseqs x ++ rmseqs y
rmseqs x = [x]

dropWhile :: (Char -> Bool) -> LaTeX -> LaTeX
dropWhile p (TeXSeq x y) = TeXSeq (dropWhile p x) y
dropWhile p (TeXRaw x) = TeXRaw (Text.dropWhile p x)
dropWhile _ x = x

invisible :: LaTeX -> Bool
invisible (TeXComm "index" _) = True
invisible (TeXComment _) = True
invisible (TeXSeq x y) = invisible x && invisible y
invisible _ = False

dropWhileEnd :: (Char -> Bool) -> LaTeX -> LaTeX
dropWhileEnd p (TeXSeq x y)
	| invisible y = TeXSeq (dropWhileEnd p x) y
	| otherwise = TeXSeq x (dropWhileEnd p y)
dropWhileEnd p (TeXRaw x) = TeXRaw (Text.dropWhileEnd p x)
dropWhileEnd _ x = x

-- These dropWhile and dropWhileEnd only make a half-hearted effort, in that
-- they don't handle TeXRaws sequenced together, but we don't need that.

triml, trimr, trim :: LaTeX -> LaTeX
triml = dropWhile isSpace
trimr = dropWhileEnd isSpace
trim = triml . trimr

isMath :: LaTeX -> Bool
isMath (TeXMath _ _) = True
isMath (TeXComm "ensuremath" _) = True
isMath (TeXEnv "eqnarray*" _ _) = True
isMath _ = False

rmWsAfterCommS :: LaTeX -> LaTeX
	-- Needed for "\dcr x" --> "--x"
rmWsAfterCommS y
		| isMath y = y -- don't touch math because it will be re-serialized
		               -- to TeX before passing on to MathJax
		| otherwise = f y
	where
		f (TeXSeq (TeXCommS x) m)
			| not (x `elem` words "{ & # %") = TeXSeq (TeXCommS x) (rmWsAfterCommS $ dropWhile (== ' ') m)
		f x@TeXEmpty = x
		f x@(TeXRaw _) = x
		f x@(TeXCommS _) = x
		f x@(TeXEnv "codeblock" _ _) = x
		f x@(TeXEnv "itemdecl" _ _) = x
		f (TeXEnv x a z) = TeXEnv x (map (mapTeXArg rmWsAfterCommS) a) (rmWsAfterCommS z)
		f x@(TeXComment _) = x
		f (TeXComm x a) = TeXComm x (map (mapTeXArg rmWsAfterCommS) a)
		f (TeXBraces x) = TeXBraces (rmWsAfterCommS x)
		f x@(TeXLineBreak _ _) = x
		f (TeXSeq x a) = TeXSeq (rmWsAfterCommS x) (rmWsAfterCommS a)
		f x = error $ "rmWsAfterCommS: " ++ show x

needsSpace :: LaTeX -> Bool
	-- In the sense of \xspace
needsSpace TeXEmpty = False
needsSpace (TeXMath _ x) = needsSpace x
needsSpace (TeXSeq (TeXComm "index" _) x) = needsSpace x
needsSpace (TeXSeq x _) = needsSpace x
needsSpace (TeXComm "texttt" _) = True
needsSpace (TeXComm "mathsf" [FixArg x]) = needsSpace x
needsSpace (TeXComm "mathscr" [FixArg x]) = needsSpace x
needsSpace (TeXComm "tcode" [FixArg x]) = needsSpace x
needsSpace (TeXComm "textit" [FixArg x]) = needsSpace x
needsSpace (TeXComm "grammarterm_" _) = True
needsSpace (TeXComm "index" _) = False
needsSpace (TeXComm "sqrt" _) = True
needsSpace (TeXComm "ensuremath" (x : _)) = needsSpace $ texFromArg x
needsSpace (TeXComm "texorpdfstring" [_, FixArg x]) = needsSpace x
needsSpace (TeXCommS " ") = False
needsSpace (TeXCommS ",") = False
needsSpace (TeXCommS "~") = False
needsSpace (TeXCommS "&") = False
needsSpace (TeXBraces x) = needsSpace x
needsSpace (TeXRaw t) = Util.startsWith (\c -> isAlphaNum c || (c `elem` ("~&-!*(" :: String))) t
needsSpace x = error $ "needsSpace: unexpected: " ++ show x
