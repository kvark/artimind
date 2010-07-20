namespace artimind

public class Creature:
	public final name	as string
	public final mind	= Mind()
	public def constructor(str as string):
		name = str
	public def step() as void:
		mind.send(true)
		action = mind.decide()
		mind.learn()	if action


public class CatBasic(Creature):
	public def frInt(pow as int) as int:	# conscience
		return 1
	public def faWait(pow as int) as int:	# waiting
		return 0
	public def constructor(str as string):
		super(str)
		mind.reset( (frInt,) , (faWait,) )
