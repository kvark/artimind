module AI.Person	(
Person
, makePerson
)where

import Data.Maybe (fromJust)
import Data.List (find)
import AI.Core


data (World w, Think t x) => Person w t x =
	Person t (Choice x) [(x,String,Sensor w)] [(x,String,Actor w)]

attachHand hand (name,q) = (hand,name,q)
getHand (hand,_,_) = hand
getName (_,name,_) = name


instance (World w, Think t x) => Body (Person w t x) w where
	addSensors (Person brain chooser sensors actors) sin = let
			(newBrain,handles) = alloc brain (length sin)
			ns = zipWith attachHand handles sin
		in	Person newBrain chooser (ns++sensors) actors
	addActors (Person brain chooser sensors actors) act = let
			(newBrain,handles) = alloc brain (length act)
			na = zipWith attachHand handles act
		in	Person newBrain chooser sensors (na++actors)
	stepUp world (Person brain chooser sensors actors) = let
			getSignal (hand,name,sense) = (hand,sense world)
			inputs = map getSignal sensors
			outHandles = map getHand actors
			decision = decide brain inputs outHandles
			choice = chooser decision
			target = find ((==choice) . getHand) actors
			(_,name,act) = fromJust target
		in	(name,act)
	stepDown (Person brain chooser sensors actors) (name,heat) = let
			target = find ((==name) . getName) actors
			(hand,_,_) = fromJust target
			newBrain = learn brain (hand,heat)
		in	Person newBrain chooser sensors actors


makePerson	:: (World w, Think t x) =>
	t -> (Choice x) -> [(String,Sensor w)] -> [(String,Actor w)] -> Person w t x
makePerson brain chooser sensors actors = let
		p0 = Person brain chooser [] []
		p1 = addSensors p0 sensors
		p2 = addActors p1 actors
	in	p2
