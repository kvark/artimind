module Main
( main
) where

import Data.List
import qualified AI.Core as Ai
import qualified Env

type State = ( Env.World, Env.Cat )
state	:: Int -> (( String, Ai.Heat ), State)
state 0 = (( "init", 0 ), ( Env.World 0, Env.makeCat ))
state n = let
	(w,b) = snd (state (n-1))
	(w',b',name,result) = Ai.cycle w b
	in ((name,result),(w',b'))


message	:: Int -> ([String],State)
message 0 = ([], snd $ state 0)
message n = let
	(s,(w0,b)) = message (n-1)
	w1 = Ai.advance w0
	(w2,b',act,result) = Ai.cycle w1 b
	s1 = "Step " ++ show n
	s2 = "\tAction " ++ show act
	s3 = "\tResult " ++ show result
	brain = Env.getBrain b
	s4 = "\t" ++ show w0 ++ " -> " ++ show w1
	s5 = "\t" ++ show brain
	msg = [s1++s2++s3, s4,s5]
	in (msg++s, (w2,b'))

main	:: IO ()
main = do
	putStrLn $ intercalate "\n" $ reverse $ fst $ message 10
