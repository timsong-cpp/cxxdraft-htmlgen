{-# LANGUAGE OverloadedStrings, ViewPatterns, LambdaCase #-}

module Sentences (splitIntoSentences, isActualSentence) where

import LaTeXBase (LaTeXUnit(..), triml, LaTeX, ArgKind(FixArg))
import Data.Text (isPrefixOf, isSuffixOf, stripPrefix)
import qualified Data.Text as Text
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isSpace, isDigit, isAlphaNum, isUpper, isLower)
import Util ((++), textStripInfix, dropTrailingWs)
import RawDocument

startsSentence :: RawElement -> Bool
startsSentence (RawLatexElement e) | [TeXRaw x] <- triml [e], x /= "" = isUpper (Text.head x)
startsSentence _ = False

unitContinuesSentence :: LaTeXUnit -> Bool
unitContinuesSentence (TeXComm " " []) = True
unitContinuesSentence (TeXRaw txt) = "," `isPrefixOf` txt
unitContinuesSentence _ = False

elemContinuesSentence :: RawElement -> Bool
elemContinuesSentence (RawLatexElement u) = unitContinuesSentence u
elemContinuesSentence _ = False

elemsContinueSentence :: [RawElement] -> Bool
elemsContinueSentence (RawLatexElement (TeXRaw "") : more) = elemsContinueSentence more
elemsContinueSentence (x : _) = elemContinuesSentence x
elemsContinueSentence _ = False

splitIntoSentences :: [RawElement] -> [[RawElement]]
splitIntoSentences = go []
	where
		go [] [] = []
		go [] (RawLatexElement (TeXRaw "\n") : y) = go [] y
		go [] (x@(RawExample _) : y) = [x] : go [] y
		go [] (x@(RawNote _ _) : y) = [x] : go [] y
		go partial (x@(RawCodeblock _) : y) | z : _ <- rmIndices y, startsSentence z = (partial ++ [x]) : go [] y
		go x [] = [x]
		go x z@(e : y)
			| Just (s, rest) <- breakSentence z = (x ++ s) : go [] rest
			| otherwise = go (x ++ [e]) y
		rmIndices (RawLatexElement (TeXRaw "\n") : RawLatexElement (TeXComm "index" _) : x) = rmIndices x
		rmIndices x = x

breakSentence :: [RawElement] -> Maybe ([RawElement] {- sentence -}, [RawElement] {- remainder -})
breakSentence (e@(RawLatexElement (TeXMath _ math)) : more)
    | f (reverse math) = Just ([e], more)
    where
        f :: LaTeX -> Bool
        f (TeXRaw y : z) | all isSpace (Text.unpack y) = f z
        f (TeXComm "text" [(FixArg, a)] : _) = f (reverse a)
        f (TeXComm "mbox" [(FixArg, a)] : _) = f (reverse a)
        f (TeXRaw y : _) = "." `isSuffixOf` (Text.pack $ dropTrailingWs $ Text.unpack y)
        f _ = False
breakSentence (b@(RawLatexElement TeXLineBreak) : more) = Just ([b], more)
breakSentence (b@(RawLatexElement (TeXComm "break" [])) : more) = Just ([b], more)
breakSentence (RawLatexElement (TeXRaw (textStripInfix "." -> (Just ((++ ".") -> pre, post)))) : more)
    = f pre post
  where
   f pre post
    | not (("(." `isSuffixOf` pre) && (")" `isPrefixOf` post))
    , not (Text.length post > 0 && isLower (Text.head post))
    , not (Text.length pre > 1 && Text.length post > 0 && isAlphaNum (Text.last $ Text.init pre) && isDigit (Text.head post))
    , not (elemsContinueSentence (RawLatexElement (TeXRaw post) : more))
    , not (Text.length pre >= 2 && ("." `isSuffixOf` pre) && isUpper (Text.last $ Text.init pre))
    , not ("e.g." `isSuffixOf` pre)
    , not ("i.e." `isSuffixOf` pre) =
        let
            post' = Text.stripStart post
            (pre', post'') = case stripPrefix ")" post' of
                Just z -> (pre ++ ")" , Text.stripStart z)
                Nothing -> (pre, post')
            more' = if post'' == "" then more else RawLatexElement (TeXRaw post'') : more
            (maybefootnote, more'') = case more' of
                fn@(RawLatexElement (TeXComm "footnoteref" _)) : z -> ([fn], z)
                _ -> ([], more')
            sentence = [RawLatexElement (TeXRaw pre')] ++ maybefootnote
        in
            Just (sentence, more'')
    | Just ((++ ".") -> pre', post') <- textStripInfix "." post = f (pre ++ pre') post'
    | otherwise = Nothing
breakSentence (enum@(RawEnumerated _ (last -> rawItemContent -> last -> RawTexPara (last -> el))) : more)
    | Just _ <- breakSentence [el] = Just ([enum], more)
breakSentence _ = Nothing

isActualSentence :: [RawElement] -> Bool
isActualSentence (RawEnumerated _ _ : _) = False
isActualSentence l = any p l
	where
		yes = words $
			"link tcode noncxxtcode textit ref grammarterm indexedspan " ++
			"defnx textbf textrm textsl textsc deflinkx"

		q :: LaTeXUnit -> Bool
		q (TeXRaw s) = not $ all isSpace $ Text.unpack s
		q (TeXComm c _) | c `elem` yes = True
		q (TeXEnv c _ _) | c `elem` yes = True
		q (TeXEnv "indexed" _ body) = any q body
		q (TeXBraces body) = any q body
		q _ = False

		p :: RawElement -> Bool
		p (RawLatexElement u) = q u
		p RawEnumerated{} = True
		p _ = False