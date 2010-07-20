namespace artimind

import System.Collections.Generic

public class Axon:
	public dest		as Neuron	= null
	public power	as single	= 0f


public class Neuron:
	public final arms	= List[of Axon]()
	public charge	as single
	public sum		as single
	
	public def linkTo(n as Neuron) as void:
		arms.Add( Axon(dest:n,power:1f) )
		sum += 1f
	public def cutOff() as void:
		arms.Clear()
		sum = 0f
	public def clean() as void:
		charge = 0f
		return	if sum < 1.0e10
		sum = 0f
		for ax in arms:
			ax.power *= 0.01f
			sum += ax.power
