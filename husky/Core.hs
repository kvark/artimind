{-# OPTIONS -XImpredicativeTypes #-}

module AI.Core
(	Body, World, Sensor, Actor,
	Think, decide, learn, ZeroMind
) where


class Body b
class World w

class (Show s) => Sensor s where
	read	:: (World w, Body b) => s -> (w,b) -> Int

class (Show a) => Actor2 a where
	perform	:: (World w, Body b) => a -> (w,b) -> (w,b) 

type Actor = forall w b. (World w, Body b) => (w,b) -> (w,b)

class Think t where
	decide	:: (Sensor s) => t -> s -> (Actor,String)
	learn	:: t -> Actor -> t


data ZeroMind = ZeroMind	{}
instance Think ZeroMind where
	decide ZeroMind _ = (id,"none")
	learn ZeroMind _ = ZeroMind
