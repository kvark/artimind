module Main
( main
) where

import qualified AI.Core as Ai
import qualified Env

iterFun	:: (Ai.World w, Ai.Body b, Ai.Think w b t) => ([Ai.Sensor w b],(w,b,t)) -> ((String,Ai.Energy),(w,b,t))
iterFun (s,(w,b,t)) =
	let	(actor,description) = Ai.decide t s
		(w',b',result) = actor (w,b)
		t' = Ai.learn t (actor,result)
	in	((description,result), (w',b',t'))

sensors = [Ai.feelConst 0]
state :: Int -> (( String, Ai.Energy ),( Env.World, Env.Body, Env.TestMind ))
state 0 = (( "init", 0 ), ( Env.World 0, Env.Body, Env.TestMind ))
state n = iterFun (sensors, snd $ state (n-1))

message 0 = ("", snd $ state 0)
message n =
	let	(s,prev) = message (n-1)
		(info,cur) = iterFun (sensors,prev)
		s1 = "\nStep " ++ show n
		s2 = "\tAction " ++ show (fst info)
		s3 = "\tResult " ++ show (snd info)
	in	(s ++ s1 ++ s2 ++ s3, cur)

main = do
	putStrLn $ fst $ message 10

