namespace artimind

import System.Collections.Generic

public struct Extern:
	public fun	as callable(int) as int
	public ron	as Neuron
	public def constructor(f as callable):
		fun = f
		ron = Neuron()


public class Mind:
	protected final neurons		= List[of Neuron]()
	protected final receptors	= List[of Extern]()
	protected final actors		= List[of Extern]()
	
	public def check() as void:
		for ex in receptors:
			assert ex.fun
			assert ex.ron and ex.ron in neurons
		for ex in actors:
			assert ex.fun
			assert ex.ron and ex.ron in neurons
		
	public def reset(lrec as (callable), lact as (callable)) as void:
		neurons.Clear()
		# fill actors
		for rec in lrec:
			actors.Add( Extern(rec) )
		# fill receptors & links
		for act in lact:
			ex = Extern(act)
			receptors.Add(ex)
			for ac in actors:
				ex.ron.linkTo( ac.ron )
		actors.ForEach() do(ref ex as Extern):
			ex.ron = Neuron()
	
	public def send(x as bool) as void:
		pass
	public def decide() as Extern:
		return Extern(null)
	public def learn() as void:
		pass
