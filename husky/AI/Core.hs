module AI.Core
(	Body, World,
	Energy, Sensor, Actor,
	Think, decide, learn,
	feelConst, actIdle,
) where


class Body b
class World w

type Energy = Int
type Actor w b = (w,b) -> (w,b,Energy)
type Sensor w b = (w,b) -> Energy


class (World w, Body b) => Think w b t | t->w, t->b where
	decide	:: t -> [Sensor w b] -> (Actor w b,String)
	learn	:: t -> (Actor w b,Energy) -> t


feelConst	:: (World w, Body b) => Energy -> (w,b) -> Energy
feelConst val _ = val

actIdle	:: (World w, Body b) => Actor w b
actIdle (w,b) = (w,b,0)
