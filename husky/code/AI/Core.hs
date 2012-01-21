module AI.Core
( World, Heat, Sensor, Actor, Ignot, Choice
, Think (alloc,decide,learn)
, Body (addSensors,addActors,stepUp,stepDown,cycle)
, ZeroMind (ZeroMind)
, feelConst, actIdle, chooseFirst, chooseMax
) where

import Data.List (maximumBy)


class World w

type Heat		= Int				-- input/output energy
type Sensor w	= w -> Heat			-- input signal
type Actor w	= w -> (w,Heat)		-- output signal, changes the world
type Ignot x	= (x,Heat)			-- charged internal handle
type Choice x	= [Ignot x] -> Maybe x	-- choose a handle among charged ones


feelConst	:: (World w) => Heat -> w -> Heat
feelConst val _ = val

actIdle	:: (World w) => Actor w
actIdle w = (w,0)


class (Eq x) => Think t x | t->x where
	-- obtain a new handle for input/output --
	alloc	:: t -> Int -> (t,[x])
	-- given a list of charged handles and a list of output handles
	-- produce a corresponding list of output charged handles --
	decide	:: t -> [Ignot x] -> [x] -> [Ignot x]
	-- adapt to chosen handle with response --
	learn	:: t -> Ignot x -> t


data ZeroMind = ZeroMind
instance Think ZeroMind () where
	alloc t n = (t,take n (repeat ()))
	decide _ _ outputs = zip outputs (repeat (1::Heat))
	learn t _ = t

toMaybe	:: a -> (a->Bool) -> Maybe a
toMaybe x predicate
	| predicate	x	= Just x
	| otherwise		= Nothing

chooseFirst	:: Choice x
chooseFirst = Just . fst . head

compareHeat	:: (Ord a) => (b,a) -> (b,a) -> Ordering
compareHeat (x1,y1) (x2,y2) = compare y1 y2

chooseMax 	:: Choice x
chooseMax al
		| heat>0	= Just act
		| otherwise	= Nothing
	where (act,heat) = maximumBy compareHeat al


class (World w) => Body b w | b->w where
	addSensors	:: b -> [(String,Sensor w)] -> b
	addActors	:: b -> [(String,Actor w)] -> b
	stepUp		:: w -> b -> Maybe (String,Actor w)
	stepDown	:: b -> (String,Heat) -> b
	cycle		:: w -> b -> (w,b,String,Heat)
	cycle world body = case (stepUp world body) of
		Nothing	-> (world,body,"no",0)
		Just (name,actor)	-> let
				(newWorld,response) = actor world
				newBody = stepDown body (name,response)
			in (newWorld,newBody,name,response)
