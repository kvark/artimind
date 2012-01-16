module AI.Core
(	Body, World, Sensor, Actor,
	Think, decide, learn, MindZero
) where


class Body b
class World w

type Sensor	st	= st -> Int
type Actor st	= st -> st


class Think t where
	decide	:: (World w, Body b) => t -> [Sensor (w,b)] -> Actor (w,b)
	learn	:: (World w, Body b) => t -> [Actor (w,b)] -> t


data MindZero = MindZero	{}

instance Think MindZero where
	decide _ _ = id
	learn t _ = t
