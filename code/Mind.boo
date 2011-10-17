namespace artimind

import System.Collections.Generic

public class Method:
	private final fun	as System.Func[of int,int]
	public final ron	as Neuron
	public result		= 0
	
	public Name as string:
		get: return fun.Method.Name
	
	public def constructor(f as System.Func[of int,int]):
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
		
	public def reset(lrec as (System.Func[of int,int]), lact as (System.Func[of int,int])) as void:
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

	
	#-------------------------------------------
	#	COMPUTATIONAL TASKS
	#-------------------------------------------

	public def send(time as single, generate as bool) as void:
		que = Queue[of Neuron]()
		for rec in receptors:
			rec.execute()	if generate
			continue	if not rec.result
			rec.ron.charge += 1f * rec.result
			que.Enqueue( rec.ron )
		while que.Count:
			ron = que.Dequeue()
			continue	if not ron.arms.Count
			ron.load.Value[time] = ron.charge
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
				act.ron.response = kAfterMath * act.result
			else: act.ron.response = 0f
		return met

	
	public def learn(time as single) as void:
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
			ron.response = 0f
			for ax in ron.arms:
				d = ax.dest
				assert d != ron
				continue	if d.response or not d.arms.Count
				que.Enqueue(d)
				sta.Push(d)
				d.response = 1f
		# unfold the stack
		while sta.Count:
			ron = sta.Pop()
			continue	if ron.response
			for ax in ron.arms:
				assert ax.dest != ron
				den = ax.dest.response
				continue	if not den
				assert ron.response in (0f,den)
				ax.response.Value[time] = den
				ax.profit.Value[time] = den * ron.charge
				ron.response = den	# careful!
				add = den * (ax.power,ron.sum)[den>0f]
				ax.power += add
				ron.sum += add
		# clean neuron charges
		for n in neurons:
			n.clean()


	#-------------------------------------------
	#	HOUSE-KEEPING TASKS
	#-------------------------------------------

	private static final AxonMinResponse	= 0f
	private static final NeuronMinOutput	= 0.1f
	private static final AxonThreshold		= 5f
	private static final AxonCovariation	= 0.01f
	
	private def choseRandom(val as single, fun as callable(Neuron) as single) as Neuron:
		val *= random.NextDouble()
		for n in neurons:
			val -= fun(n)
			return n	if val<0f
		return null
	
	private def axonDelete(time as single) as void:
		for ron in neurons:
			ron.arms.RemoveAll() do(ax as Axon):
				resp = ax.response.Value[time]
				return resp < AxonMinResponse
	
	private def axonCreate() as void:
		n2input = Dictionary[of Neuron,single]()
		totalInputs = 0f
		totalOutputs = 0f
		for ron in neurons:
			totalOutputs += ron.sum
			for ax in ron.arms:
				totalInputs += 1f
				if ron in n2input:
					n2input[ron] += 1f
				else:
					n2input[ron] = 1f
		nFrom = choseRandom(totalOutputs) do(n as Neuron):
			rez = 0f
			n2input.TryGetValue(n,rez)
			return rez
		nTo = choseRandom(totalInputs) do(n as Neuron):
			return n.sum
		if nFrom and nTo and nFrom!=nTo and\
		nTo.sum > n2input[nFrom] * AxonThreshold:
			#incorrect: loops are created
			nFrom.linkTo(nTo)
	
	private def neuronDelete() as void:
		n2delete = neurons.FindAll() do(ron as Neuron):
			return ron.sum < NeuronMinOutput
		for act in actors:
			n2delete.Remove( act.ron )
		for ron in n2delete:
			neurons.Remove(ron)
			for n in neurons:
				n.arms.RemoveAll() do(ax as Axon):
					ax.dest == ron
	
	private def neuronCreate(time as single) as void:
		n2add = List[of Neuron]()
		for ron in neurons:
			continue	if not ron.arms.Count
			a2effect = Dictionary[of Axon,single]()
			sumVal = 0f
			good = false
			for ax in ron.arms:
				val = ax.getCovariation(ron,time)
				good = true	if val > AxonCovariation
				sumVal += val
				a2effect.Add(ax,val)
			continue	if not good
			sumVal /= ron.arms.Count
			if sumVal < AxonCovariation:
				n = Neuron()
				n2add.Add(n)
				for ax in ron.arms:
					if a2effect[ax] < AxonCovariation:
						n.linkTo( ax.dest )
				for ax in n.arms:
					ron.arms.RemoveAll() do(axon as Axon):
						return axon.dest == ax.dest
				for ns in neurons:
					for ax in ns.arms:
						if ax.dest == ron:
							ns.linkTo(n)
							break
		neurons.AddRange(n2add)
	
	public def houseKeep(time as single) as void:
		axonDelete(time)
		#axonCreate()
		neuronDelete()
		neuronCreate(time)
		# re-sync
		for ron in neurons:
			ron.sync()
