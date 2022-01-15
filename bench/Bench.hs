{-# language OverloadedStrings, PackageImports #-}
module Main where

import "rope-utf16" Data.Rope.UTF16.Internal.Text
import Control.Monad
import Criterion
import qualified Criterion.Main as Criterion
import Data.Monoid
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Unsafe
import qualified Yi.Rope as Yi
import qualified "rope-utf16" Data.Rope.UTF16 as Finger
import qualified "rope-utf16-splay" Data.Rope.UTF16 as Splay
import System.Random

textModify :: Int -> Int -> Text -> Text
textModify i j t = do
  let (pre, post) = split16At i t
      (_, post') = split16At (j - i) post
      repl = Text.replicate (j - i) "a"
  pre <> repl <> post'

fingerModify :: Int -> Int -> Finger.Rope -> Finger.Rope
fingerModify i j t = do
  let (pre, post) = Finger.splitAt i t
      (_, post') = Finger.splitAt (j - i) post
      repl = Text.replicate (j - i) "a"
  pre <> Finger.fromText repl <> post'

yiModify :: Int -> Int -> Yi.YiString -> Yi.YiString
yiModify i j t = do
  let (pre, post) = Yi.splitAt i t
      (_, post') = Yi.splitAt (j - i) post
      repl = Text.replicate (j - i) "a"
  pre <> Yi.fromText repl <> post'

splayModify :: Int -> Int -> Splay.Rope -> Splay.Rope
splayModify i j t = do
  let (pre, post) = Splay.splitAt i t
      (_, post') = Splay.splitAt (j - i) post
      repl = Text.replicate (j - i) "a"
  pre <> Splay.fromText repl <> post'

modifys :: (r -> Int) -> (Int -> Int -> r -> r) -> Int -> [Int] -> r -> r
modifys len modify modLen indices r =
  foldr ($) r
  [ modify pre (pre + modLen)
  | i <- indices
  , let pre = i `mod` len r
  ]

steps :: Int
steps = 1000

getInp :: Int -> IO (Text, [Int])
getInp n = return (Text.replicate (n `div` 6) "abc123", take steps [0,5..])

getRandInp :: Int -> IO (Text, [Int])
getRandInp n = do
  let inp = Text.replicate (n `div` 6) "abc123"
  indices <- replicateM steps $ getStdRandom (randomR (0, Unsafe.lengthWord16 inp))
  return (inp, indices)

main :: IO ()
main = Criterion.defaultMain
  [ bgroup ("inputlen " <> show len <> ", " <> show steps <> " iterations")
    [ env (getInp len) $ \ ~(inp, indices) ->
      bgroup "linear mods"
      [ bgroup ("mods sized " <> show modLen)
        [ -- bench "FingerTree" $ nf (Finger.toText . modifys Finger.length fingerModify modLen indices . Finger.fromText) inp
         bench "Yi.Rope" $ nf (Yi.toText . modifys Yi.length yiModify modLen indices . Yi.fromText) inp
        , bench "SplayTree" $ nf (Splay.toText . modifys Splay.length splayModify modLen indices . Splay.fromText) inp
        ]
      | modLen <- [10]
      ]
    , env (getRandInp len) $ \ ~(inp, indices) ->
      bgroup "random mods"
      [ bgroup ("mods sized " <> show modLen)
        [ -- bench "FingerTree" $ nf (Finger.toText . modifys Finger.length fingerModify modLen indices . Finger.fromText) inp
         bench "Yi.Rope" $ nf (Yi.toText . modifys Yi.length yiModify modLen indices . Yi.fromText) inp
        , bench "SplayTree" $ nf (Splay.toText . modifys Splay.length splayModify modLen indices . Splay.fromText) inp
        ]
      | modLen <- [10]
      ]
    , env (getInp len) $ \ ~(inp, _) ->
      bgroup "toText . fromText"
      [
      -- bench "FingerTree" $ nf (Finger.toText . Finger.fromText) inp
       bench "Yi.Rope" $ nf (Yi.toText . Yi.fromText) inp
      , bench "SplayTree" $ nf (Splay.toText . Splay.fromText) inp
      ]
    ]
  | len <- [1000, 10000, 100000]
  ]
