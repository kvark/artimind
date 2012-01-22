module AI.Person
( Person
, extractBrain
, makePerson
)where

import Data.Maybe (fromJust)
import Data.List (find)
import AI.Core


data (World w, Think t x) => Person w t x =
	Person t (Choice x) [(x,String,Sensor w)] [(x,String,Actor w)]

extractBrain	:: (World w, Think t x) => Person w t x -> t
extractBrain (Person brain _ _ _) = brain
attachHand	:: x -> (String,y) -> (x,String,y)
attachHand hand (name,q) = (hand,name,q)
getHand		:: (x,y,z) -> x
getHand (hand,_,_) = hand
getName		:: (y,String,z) -> String
getName (_,name,_) = name


instance (World w, Think t x) => Body (Person w t x) w where
	addSensors (Person brain chooser sensors actors) input = let
			(newBrain,handles) = alloc brain (length input)
			ns = zipWith attachHand handles input
		in	Person newBrain chooser (ns++sensors) actors
	addActors (Person brain chooser sensors actors) act = let
			(newBrain,handles) = alloc brain (length act)
			na = zipWith attachHand handles act
		in	Person newBrain chooser sensors (na++actors)
	stepUp world (Person brain chooser sensors actors) = let
			getSignal (hand,_,sense) = (hand,sense world)
			inputs = map getSignal sensors
			outHandles = map getHand actors
			decision = decide brain inputs outHandles
			choice = chooser decision
			target = find ((==choice) . getHand) actors
			(_,name,act) = fromJust target
		in (name,act)
	stepDown world (Person brain chooser sensors actors) (name,heat) = let
			getSignal (h,_,sense) = (h,sense world)
			inputs = map getSignal sensors
			target = find ((==name) . getName) actors
			(hand,_,_) = fromJust target
			newBrain = learn brain (inputs,(hand,heat))
		in	Person newBrain chooser sensors actors


makePerson	:: (World w, Think t x) =>
	t -> (Choice x) -> [(String,Sensor w)] -> [(String,Actor w)] -> Person w t x
makePerson brain chooser sensors actors = let
		p0 = Person brain chooser [] []
		p1 = addSensors p0 sensors
		p2 = addActors p1 actors
	in	p2
