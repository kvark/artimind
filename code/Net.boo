namespace artimind

import System.Collections.Generic

public struct Axon:
	public dest		as Neuron
	public power	as single


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
		arms.ForEach() do(ref ax as Axon):
			ax.power *= 0.01f
			sum += ax.power
