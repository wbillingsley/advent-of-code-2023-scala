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

class Circuit(triples:Seq[(String, ComponentType, Seq[String])]) {
    // I have a feeling we might need the network at some point.
    val forwardMap = (for (s, _, d) <- triples yield s -> d).toMap

    val counts = mutable.Map[Pulse, Int]((for p <- Pulse.values yield p -> 0)*)

    sealed trait Element {
        def receive(from:String, msg:Pulse):Unit
    }
    
    def send(from:String, msg:Pulse):Unit = 
        if from == "button" then 
            counts(msg) = counts(msg) + 1
            gates("broadcaster").receive("button", msg)
        else     
            for d <- forwardMap(from) do
                counts(msg) = counts(msg) + 1
                for g <- gates.get(d) do g.receive(from, msg)


    class Broadcaster(me:String) extends Element {
        override def receive(from:String, pulse:Pulse) = 
            send(me, pulse)
    }

    class FlipFlop(me:String) extends Element {
        var _state = false
        override def receive(from:String, pulse:Pulse) = pulse match 
            case High => ()
            case Low => 
                _state = !_state
                if _state then send(me, High) else send(me, Low)
    }

    class Conjunction(me:String) extends Element {
        val inputNames = forwardMap.filter({ case (n, dest) => dest.contains(me) }).map(_._1)
        val states = mutable.Map[String, Pulse]((for n <- inputNames yield n -> Low).toSeq*)

        override def receive(from:String, pulse:Pulse) = 
            states(from) = pulse
            if states.values.forall(_ == High) then 
                send(me, Low)
            else 
                send(me, High)
    }

    // build the gates
    val gates:Map[String, Element] = (for (n, t, d) <- triples yield n -> (t match {
        case ComponentType.Broadcaster => Broadcaster(n)
        case ComponentType.Conjunction => Conjunction(n)
        case ComponentType.FlipFlop => FlipFlop(n)
    })).toMap

    def button(msg:Pulse) = 
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

    for (i <- 0 until 1000) do circuit.button(Low)
    val (l, h) = circuit.totals

    println(s"result is $l $h ==> ${l * h}")




            

    



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

