namespace artimind

public class Agent:
	public final name	as string
	public final mind	= Mind()
	private time	as single	= 0f
	
	public def constructor(str as string):
		name = str
	public def step() as Method:
		mind.generateInputs()
		#mind.send(time)
		#action = mind.decide()
		#mind.learn(time)	if action
		#mind.houseKeep(time)
		time += 1f
		#return action
		return null


public class Cat(Agent):
	private final env	as World
	
	public def frInt() as int:	# conscience
		return 1
	public def frSmell() as int:	# smell of food
		return (0,1)[env.food]
	public def faWait() as int:	# wait
		return 0
	public def faEat() as int:	# eat
		if env.food:
			env.food = false
			return 3
		return -1
	
	public def constructor(str as string, world as World):
		super(str)
		env = world
		mind.resetStructure( (frInt,frSmell) , (faWait,faEat) )
