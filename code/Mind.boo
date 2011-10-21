namespace artimind

import System.Collections.Generic

public class Method:
	private	final	fun		as System.Func[of int]
	public	final	ron		as Neuron
	public			result	= 0
	
	public Name as string:
		get: return fun.Method.Name
	
	public def constructor(f as System.Func[of int]):
		fun = f
		ron = Neuron()

	public def execute() as void:
		result = fun()

	public def propagate() as bool:
		if not result:
			return false
		ron.current += result * 1f
		return true

	public def check() as void:
		assert fun
		assert ron


		
public class Neuron:
	private	static	final	kNeuroTax	= 0.1f
	private	static	final	kTaxMult	= 2f

	public final axons	= List[of Neuron]()
	public current		as single	= 0f
	public totalCharge	as single	= 0f
	public totalAnti	as single	= 0f
	public tax			as single	= 0f
	
	public def clean() as void:
		current = totalCharge = totalAnti = 0f
		tax = kNeuroTax
	
	public def propagate() as single:
		if not axons.Count:
			return -1f
		totalCharge += current
		current -= tax
		if current <= 0f:
			current = 0f
		tax *= kTaxMult
		return current / axons.Count




public class Mind:
	private	static	final	random		= System.Random()
	
	protected		final	neurons		= List[of Neuron]()
	protected		final	receptors	= List[of Method]()
	protected		final	actors		= List[of Method]()
	
	
	public def checkValidity() as void:
		for me in receptors:
			me.check()
			assert me.ron in neurons
		for me in actors:
			me.check()
			assert not me.ron.axons.Count
			assert me.ron in neurons

		
	public def resetStructure(lrec as (System.Func[of int]), lact as (System.Func[of int])) as void:
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
				met.ron.axons.Add( ac.ron )
	
	
	public def generateInputs() as void:
		for rec in receptors:
			rec.execute()
	
	public def cleanCharges() as void:
		for n in neurons:
			n.clean()


	#-------------------------------------------
	#	COMPUTATIONAL TASKS
	#-------------------------------------------
	
	private	static	final	kDecidePower	= 2f
	
	public def propageateSignal() as void:
		que = Queue[of Neuron]()
		# fill queue with inputs
		for rec in receptors:
			if not rec.propagate():
				continue
			que.Enqueue( rec.ron )
		# propagate signal
		while que.Count:
			ron = que.Dequeue()
			each = ron.propagate()
			if each <= 0f:
				continue
			for ax in ron.axons:
				if not ax.current:
					que.Enqueue(ax)
				ax.current += each
			ron.current = 0f


	public def decideAction() as Method:
		# calculate sum
		sum = 0f
		for act in actors:
			act.result = 0
			n = act.ron
			n.totalCharge += n.current
			n.current = 0f
			sum += System.Math.Pow( n.totalCharge, kDecidePower )
		if sum<=0f:
			return null
		sum *= random.NextDouble()
		# choose method
		met	as Method = null
		for act in actors:
			n = act.ron
			sum -= System.Math.Pow( n.totalCharge, kDecidePower )
			if sum <= 0f:
				(met = act).execute()
				break
		return met

	
	public def learn(time as single) as void:
		# initialize state
		for n in neurons:
			n.current = 0f
		for act in actors:
			act.propagate()
		# put initial = receptors
		que = Queue[of Neuron]()
		sta = Stack[of Neuron]()
		for rec in receptors:
			if not rec.result:
				continue
			que.Enqueue( rec.ron )
			sta.Push( rec.ron )
			rec.ron.current = single.NaN
		# form the neuron stack
		while que.Count:
			ron = que.Dequeue()
			for ax in ron.axons:
				if ax.current or not ax.axons.Count:
					continue
				que.Enqueue(ax)
				sta.Push(ax)
				ax.current = single.NaN
		# unfold the stack
		while sta.Count:
			ron = sta.Pop()
			assert ron.current == single.NaN
			if not ron.axons.Count:
				continue
			kf = 1f / ron.axons.Count
			for ax in ron.axons:
				den = ax.current
				if not den:	continue
				ron.current += kf * den


	#-------------------------------------------
	#	INTERNAL ROUTINES
	#-------------------------------------------

