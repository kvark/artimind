namespace artimind

public class Creature:
	public final name	as string
	public final mind	= Mind()
	public def constructor(str as string):
		name = str
	public def step() as Method:
		mind.send(true)
		action = mind.decide()
		mind.learn()	if action
		return action


public class CatBasic(Creature):
	private final env	as World
	
	public def frInt(pow as int) as int:	# conscience
		return 1
	public def frSmell(pow as int) as int:	# smell of food
		return (0,1)[env.food]
	public def faWait(pow as int) as int:	# wait
		return 0
	public def faEat(pow as int) as int:	# eat
		if env.food:
			env.food = false
			return 3
		return -1
	
	public def constructor(str as string, world as World):
		super(str)
		env = world
		mind.reset( (frInt,frSmell) , (faWait,faEat) )
