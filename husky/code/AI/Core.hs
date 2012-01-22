module AI.Core
( World (advance), Heat, Sensor, Actor, Ignot, Choice
, Think (alloc,decide,learn)
, Body (addSensors,addActors,stepUp,stepDown,cycle)
, ZeroMind (ZeroMind)
, feelConst, actIdle, chooseFirst, chooseMax
) where

import Data.List (maximumBy)


class (Show w) => World w where
	advance	:: w -> w

type Heat		= Int				-- input/output energy
type Sensor w	= w -> Heat			-- input signal
type Actor w	= w -> (w,Heat)		-- output signal, changes the world
type Ignot x	= (x,Heat)			-- charged internal handle
type Choice x	= [Ignot x] -> x	-- choose a handle among charged ones


feelConst	:: (World w) => Heat -> w -> Heat
feelConst val _ = val

actIdle	:: (World w) => Actor w
actIdle w = (w,-1)


class (Eq x, Show t) => Think t x | t->x where
	-- obtain a new handle for input/output --
	alloc	:: t -> Int -> (t,[x])
	-- given a list of charged handles and a list of output handles
	-- produce a corresponding list of output charged handles --
	decide	:: t -> [Ignot x] -> [x] -> [Ignot x]
	-- adapt to chosen handle with response --
	learn	:: t -> ([Ignot x],Ignot x) -> t


data ZeroMind = ZeroMind deriving (Show)
instance Think ZeroMind () where
	alloc t n = (t,take n (repeat ()))
	decide _ _ outputs = zip outputs (repeat (1::Heat))
	learn t _ = t


chooseFirst	:: Choice x
chooseFirst = fst . head

compareHeat	:: (Ord a) => (b,a) -> (b,a) -> Ordering
compareHeat (_,y1) (_,y2) = compare y1 y2

chooseMax 	:: Choice x
chooseMax = fst . maximumBy compareHeat


class (World w) => Body b w | b->w where
	addSensors	:: b -> [(String,Sensor w)] -> b
	addActors	:: b -> [(String,Actor w)] -> b
	stepUp		:: w -> b -> (String,Actor w)
	stepDown	:: w -> b -> (String,Heat) -> b
	cycle		:: w -> b -> (w,b,String,Heat)
	cycle world body = let
		(name,actor) = stepUp world body
		(newWorld,response) = actor world
		newBody = stepDown world body (name,response)
		in (newWorld,newBody,name,response)
