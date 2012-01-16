module AI.Core
(	Body, World, Sensor, Actor,
	Think, decide, learn,
	constFeel, idle, ZeroMind(ZeroMind)
) where


class Body b
class World w

type Sensor	= (World w, Body b) => (w,b) -> Int
type Actor	= (World w, Body b) => (w,b) -> (w,b,Int)

class Think t where
	decide	:: t -> Sensor -> (Actor,String)
	learn	:: t -> (Actor,Int) -> t


constFeel val _ = val

idle :: (World w, Body b) => (w,b) -> (w,b,Int)
idle (w,b) = (w,b,0)

data ZeroMind = ZeroMind	{}
instance Think ZeroMind where
	decide ZeroMind _ = (idle,"none")
	learn ZeroMind _ = ZeroMind
