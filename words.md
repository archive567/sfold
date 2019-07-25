words
=====

This is a test bed for word counting. It currently includes:

-   some literate programming style experiments.
-   optparse-generic style app design
-   using the
    [streaming](https://hackage.haskell.org/package/streaming-0.1.4.5/docs/Streaming.html)
    library.

Next stage of development (if any) will be to look at remote computing
and map-reduce haskell-style.

todo
----

-   \[ \] performance testing See
    [this](https://www.reddit.com/r/haskell/comments/5x2g0r/streaming_package_vs_pipes_conduit_question_on/)
    reddit conversation
-   \[ \] try out [Blast](https://github.com/jcmincke/Blast)

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
--------------------------------------------------------------------------------------------------------

``` {.haskell}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
```

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
------------------------------------------------------------------------------------

``` {.haskell}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
```

[libraries](https://www.stackage.org/)
--------------------------------------

-   [protolude](https://www.stackage.org/package/protolude)
-   [optparse-generic](https://www.stackage.org/package/optparse-generic)
-   [streaming](https://hackage.haskell.org/package/streaming-0.1.4.5/docs/Streaming.html)
-   [streaming-bytestring](http://hackage.haskell.org/package/streaming-bytestring)

``` {.haskell}
import Data.Default
import Data.Text as Text
import GHC.Base (String)
import Options.Generic
import Protolude
import Readme.Lhs hiding (Plain, Output)

import qualified Control.Foldl as L
import qualified Data.ByteString.Streaming.Char8 as B
import qualified Data.ByteString.Streaming.HTTP as HTTP
import qualified Data.Map as Map
import qualified Streaming as S
import qualified Streaming.Prelude as S
```

code
----

-   [hoogle](https://www.stackage.org/package/hoogle)

``` {.haskell}
data Input = FileIn String | UrlIn String
    deriving (Read, Show, Generic)

instance ParseField Input
instance ParseRecord Input

instance Default Input where
    def = FileIn "words/other/fake.txt"
    -- defUrl = UrlIn "http://www.gutenberg.org/files/4300/4300-0.txt"

data Opts w =
    Opts
    { input :: w ::: Maybe Input <?> "input source - local file or url?"
    , topn :: w ::: Maybe Int <?> "Top n word counts"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

-- | take a ByteString (A streaming library bytestring) and make a text word stream
wordStream :: Monad m => Int -> B.ByteString m r -> S.Stream (S.Of Text) m ()
wordStream n s =
    s &
    B.words &
    B.denull &
    S.take n &
    S.mapped B.toStrict & -- the strict wall of pain
    S.map ( decodeUtf8 >>>
            toLower >>>
            Text.split (not . (`Protolude.elem` ['a'..'z']))) &
    S.concat &
    S.filter (/="")

-- | map count of unique words
wordCount :: L.Fold Text (Map Text Int)
wordCount = L.Fold step Map.empty identity
  where
    step x a = Map.insertWith (+) a 1 x

-- | top n word counts
faves :: Int -> Map Text Int -> [(Text,Int)]
faves n =
    Protolude.take n .
    sortBy (\(_,x) (_,y) -> compare y x) .
    Map.toList

-- | create a html table format from the word counts
mkTable :: [(Text,Int)] -> Text
mkTable ws = h <> sep <> b <> sep <> t
  where
    sep = "\n"
    h = "\n<table>"
    t = "</table>"
    b = Text.concat
        ((\x -> "<tr>\n" <> x <> "\n</tr>\n") .
          (\(w,n) -> "<th>" <> w <> "</th>\n" <> "<th>\n" <>
            show n <> "\n</th>\n") <$> ws)

-- | fold that counts words from a streaming bytestring
foldWords :: Monad m => B.ByteString m r -> m (Map Text Int)
foldWords s = L.purely S.fold_ wordCount (wordStream 10000 s)

-- | run an url stream
fromUrl :: String -> IO (Map Text Int)
fromUrl f = do
    req <- HTTP.parseRequest f
    man <- HTTP.newManager HTTP.tlsManagerSettings
    HTTP.withHTTP req man $ \resp -> foldWords (HTTP.responseBody resp)

-- | run a file stream
fromFile :: FilePath -> IO (Map Text Int)
fromFile f = HTTP.runResourceT (foldWords (B.readFile f))

main :: IO ()
main = do
    opts :: Opts Unwrapped <- unwrapRecord "counting words, haskell style"
    let n = fromMaybe 10 (topn opts)
    let i = fromMaybe def (input opts)
    Protolude.putStrLn ("Top " <> show n <> " word counts ..." :: Text)
    ws <- case i of
      FileIn f ->  faves n <$> fromFile f
      UrlIn u -> faves n <$> fromUrl u
    void $ runOutput
      ("words/app/words.lhs", LHS)
      ("words.md", GitHubMarkdown) $
        output "results" $ Native $ (:[]) $
          table mempty ["word", "count"] [AlignLeft, AlignRight] mempty
          ((\(t, c) -> [t, show c]) <$> ws)
```

output
======

| word    |  count|
|:--------|------:|
| et      |    182|
| in      |    113|
| est     |     70|
| se      |     65|
| ad      |     64|
| ut      |     57|
| numquam |     50|
| ne      |     45|
| quod    |     44|
| non     |     39|

tests
-----

-   [doctest](https://www.stackage.org/package/doctest)

``` {.haskell}
-- | doctests
-- >>> faves 10 <$> fromFile "other/fake.txt"
-- [("et",182),("in",113),("est",70),("se",65),("ad",64),("ut",57),("numquam",50),("ne",45),("quod",44),("non",39)]
```

------------------------------------------------------------------------

Powered by [haskell](https://haskell-lang.org/),
[stack](https://docs.haskellstack.org/en/stable/README/) and
[pandoc](http://pandoc.org/).