namespace artimind

import System.Collections.Generic

public class Method:
	private final fun	as callable(int) as int
	public final ron	as Neuron
	public result		= 0
	
	public Name as string:
		get: return fun.Method.Name
	
	public def constructor(f as callable):
		fun = f
		ron = Neuron()
	public def execute() as void:
		result = fun(0)
	public def check() as void:
		assert fun
		assert ron



public class Mind:
	private static final kNeuroTax	= 0.1f
	private static final kIdLevel	= 0.02f
	private static final kAfterMath	= 0.1f
	private static final random		= System.Random()
	
	protected final neurons		= List[of Neuron]()
	protected final receptors	= List[of Method]()
	protected final actors		= List[of Method]()
	
	public def check() as void:
		for me in receptors:
			me.check()
			assert me.ron in neurons
		for me in actors:
			me.check()
			assert not me.ron.arms.Count
			assert me.ron in neurons
		
	public def reset(lrec as (callable), lact as (callable)) as void:
		neurons.Clear()
		# fill actors
		for act in lact:
			met = Method(act)
			neurons.Add( met.ron )
			actors.Add(met)
		# fill receptors & links
		for rec in lrec:
			met = Method(rec)
			neurons.Add( met.ron )
			receptors.Add(met)
			for ac in actors:
				met.ron.linkTo( ac.ron )
	

	public def send(generate as bool) as void:
		que = Queue[of Neuron]()
		for rec in receptors:
			rec.execute()	if generate
			continue	if not rec.result
			rec.ron.charge += 1f * rec.result
			que.Enqueue( rec.ron )
		while que.Count:
			ron = que.Dequeue()
			continue	if not ron.arms.Count
			ron.charge -= kNeuroTax
			if ron.charge < kIdLevel:
				ron.charge = 0f
				continue
			dsum = ron.charge / ron.sum
			for ax in ron.arms:
				assert ron != ax.dest
				# queuing only recently rised neurons
				que.Enqueue(ax.dest)	if ax.dest.charge < kIdLevel
				ax.dest.charge += dsum * ax.power
			ron.charge = 0f
		#only action neurons (the tails) are charged now


	private static def transFunc(val as single) as single:
		return val*val
	
	public def decide() as Method:
		# calculate sum
		sum = -1.0e-3
		for act in actors:
			act.result = 0
			sum += transFunc( act.ron.charge )
		return null if sum<=0f
		sum *= random.NextDouble()
		# choose method
		met	as Method = null
		for act in actors:
			oldSum = sum
			sum -= transFunc( act.ron.charge )
			if sum*oldSum < 0f:
				(met = act).execute()
				act.ron.charge = kAfterMath * act.result
			else: act.ron.charge = 0f
		return met

	
	public def learn() as void:
		que = Queue[of Neuron]()
		sta = Stack[of Neuron]()
		# put initial = receptors
		for rec in receptors:
			continue	if not rec.result
			que.Enqueue( rec.ron )
			sta.Push( rec.ron )
		# form the neuron stack
		while que.Count:
			ron = que.Dequeue()
			ron.charge = 0f
			for ax in ron.arms:
				d = ax.dest
				assert d != ron
				continue	if d.charge or not d.arms.Count
				que.Enqueue(d)
				sta.Push(d)
				d.charge = 1f
		# unfold the stack
		while sta.Count:
			ron = sta.Pop()
			continue	if ron.charge
			for ax in ron.arms:
				assert ax.dest != ron
				den = ax.dest.charge
				continue	if not den
				assert ron.charge in (0f,den)
				ron.charge = den	# careful!
				add = den * (ax.power,ron.sum)[den>0f]
				ax.power += add
				ron.sum += add
		# clean neuron charges
		for n in neurons:
			n.clean()
