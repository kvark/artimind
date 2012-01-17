module AI.Core
(	Body, World,
	Strength, Sensor, Actor,
	Think, decide, learn,
	constFeel, idleActor, ZeroMind(ZeroMind)
) where


class Body b
class World w

type Strength = Int
type Actor w b = (w,b) -> (w,b,Strength)
type Sensor w b = (w,b) -> Strength


class Think t where
	decide	:: (World w, Body b) => t -> [Sensor w b] -> (Actor w b,String)
	learn	:: (World w, Body b) => t -> (Actor w b,Strength) -> t


constFeel	:: (World w, Body b) => Strength -> (w,b) -> Strength
constFeel val _ = val

idleActor	:: (World w, Body b) => Actor w b
idleActor (w,b) = (w,b,0)

data ZeroMind = ZeroMind	{}
instance Think ZeroMind where
	decide ZeroMind _ = (idleActor,"none")
	learn ZeroMind _ = ZeroMind
