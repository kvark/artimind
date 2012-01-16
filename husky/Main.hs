module Main
( main
) where

import qualified AI.Core as Ai
import qualified Env


iterFun	:: (Ai.World w, Ai.Body b, Ai.Think t) => (Ai.Sensor,(w,b,t)) -> (String,(w,b,t))
iterFun (s,(w,b,t)) =
	let	(actor,description) = Ai.decide t s
		(w',b',result) = actor (w,b)
		t' = Ai.learn t (actor,result)
	in	(description, (w',b',t'))

sensor = Ai.constFeel 0
--state :: (Ai.World w, Ai.Body b, Ai.Think t) => Int -> (String,(w,b,t))
state 0 = ("init", (Env.World,Env.Body,Ai.ZeroMind))
state n = iterFun (sensor, snd $ state (n-1))


main = return ()

