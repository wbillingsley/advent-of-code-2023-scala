// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star35/solver.scala
// (or select the "star35" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable

enum Pulse:
    case Low, High

enum ComponentType:
    case Broadcaster, FlipFlop, Conjunction

import Pulse.*

object Done extends RuntimeException("Done")

class StateRecorder(name:String) {
    val high = mutable.Buffer.empty[Int]
    
    def record(n:Int, p:Pulse) = 
        if p == High then high.append(n)

    // Where's the cycle
    def cycle:Option[Int] = 
        if 
            high.length > 3 &&
            high(2) - high(1) == high(1) - high(0)
        then Some(high(2) - high(1)) else None
}

class Circuit(triples:Seq[(String, ComponentType, Seq[String])]) {
    // I have a feeling we might need the network at some point.
    val forwardMap = (for (s, _, d) <- triples yield s -> d).toMap

    val counts = mutable.Map[Pulse, Int]((for p <- Pulse.values yield p -> 0)*)

    val q = mutable.Queue.empty[(String, Pulse, Element)]

    sealed trait Element {
        def receive(from:String, msg:Pulse):Unit

        def name:String
    }
    
    def send(from:String, msg:Pulse):Unit = 

        def process(entry:(String, Pulse, Element)) = {
            val (from, msg, to) = entry
            // println(s"$from -$msg-> ${to.name} ")
            to.receive(from, msg)            
        }


        if from == "button" then 
            counts(msg) = counts(msg) + 1
            q.enqueue(("button", msg, gates("broadcaster")))
        else     
            for d <- forwardMap(from) do
                counts(msg) = counts(msg) + 1
                q.enqueue((from, msg, gates(d)))

        while q.nonEmpty do
            process(q.dequeue())

    class Broadcaster(val name:String) extends Element {
        override def receive(from:String, pulse:Pulse) = 
            send(name, pulse)
    }

    class FlipFlop(val name:String) extends Element {
        var _state = false
        override def receive(from:String, pulse:Pulse) = pulse match 
            case High => ()
            case Low => 
                _state = !_state
                if _state then send(name, High) else send(name, Low)
    }

    class Conjunction(val name:String) extends Element {
        val inputNames = forwardMap.filter({ case (n, dest) => dest.contains(name) }).map(_._1).toSeq
        val states = mutable.Map[String, Pulse]((for n <- inputNames yield n -> Low).toSeq*)

        val stateRecorders = (for n <- inputNames yield n -> StateRecorder(n)).toMap

        def out = states.values.forall(_ == High)

        // def cycled(input:String):Option[Long] = 
        //     val times = highTimes(input)
        //     if times.length > 3 && (times(2) - times(1) == times(1) - times(0)) then 
        //         Some(times(2) - times(1))
        //     else None

        // Just get the periods
        def cycleTimes = for n <- inputNames; c <- stateRecorders(n).cycle yield c

        override def receive(from:String, pulse:Pulse) = 
            stateRecorders(from).record(buttonCount.toInt, pulse)
            
            states(from) = pulse
            if states.values.forall(_ == High) then 
                send(name, Low)
            else 
                send(name, High)
    }

    object RX extends Element {
        val name = "rx"

        val inputNames = inputsOf("rx")

        override def receive(from:String, pulse:Pulse) = 
            if pulse == Low then 
                println("DONE!")
                throw Done
            // else 
            //     val o = inputNames.map(gates(_)).map({
            //         case c: Conjunction => 
            //             c.lastInputsOrdered
            //     }).mkString(", ")
            //     println(s"Button $buttonCount gates $o")
    }

    def inputsOf(g:String):Seq[String] = forwardMap.filter({ case (n, dest) => dest.contains(g) }).map(_._1).toSeq


    // build the gates
    val gates:Map[String, Element] = (for (n, t, d) <- triples yield n -> (t match {
        case ComponentType.Broadcaster => Broadcaster(n)
        case ComponentType.Conjunction => Conjunction(n)
        case ComponentType.FlipFlop => FlipFlop(n)
    })).toMap + ("rx" -> RX)

    // There's 48 gates in there, but it looks like there's fewer feeding into an & that then feeds into rx
    // This'll get us that and gate
    lazy val lastConjunction = gates(inputsOf("rx")(0)).asInstanceOf[Conjunction]


    var buttonCount:Long = 0

    def button(i:Long, msg:Pulse) = 
        buttonCount = i
        send("button", msg)
        (counts(Low), counts(High))

    def totals = (counts(Low), counts(High))

}


// Decompose into name, type, destinations
def decompose(line:String):(String, ComponentType, Seq[String]) = 
    val s"$typ -> $dests" = line 
    val destinations = dests.split(',').map(_.trim).toSeq
    typ match {
        case "broadcaster" => ("broadcaster", ComponentType.Broadcaster, destinations)
        case s"%$name" => (name, ComponentType.FlipFlop, destinations)
        case s"&$name" => (name, ComponentType.Conjunction, destinations)            
    }



@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val triples = lines.map(decompose)

    val circuit = Circuit(triples)
    println("Parsed circuit")

    // println(circuit.lastConjunction.inputNames)


    var i = 0L
    while i < Long.MaxValue do 
        i = i + 1
        val cycleTimes = circuit.lastConjunction.cycleTimes
        if cycleTimes.length > 3 then 
            println("Cycles " + cycleTimes)
            println("Product " + cycleTimes.map(_.toLong).product)

        if i % 100000 == 14000 then 
            println("Pressed " + i)

        try {
            circuit.button(i, Low)
        } catch {
            case Done => 
                println(s"Received Done after $i presses")
                throw IllegalStateException()
        }

    

    // circuit.button(1, Low)
    
    val (l, h) = circuit.totals

    println(s"result is $l $h ==> ${l * h}")





    














    

