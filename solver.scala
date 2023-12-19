// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star35/solver.scala
// (or select the "star35" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

type RatingRange = (Int, Int)


type Ratings = Map[String, Range]


enum WorkflowResult:
    case Do(name:String)
    case Reject
    case Accept

type Rule = Ratings => (Option[Ratings], WorkflowResult, Option[Ratings])

def exhausted(r:Ratings) = r.values.forall(_.isEmpty)

type Workflow = Seq[Rule]

import WorkflowResult.*

def outcome(s:String) = s match {
    case "A" => Accept
    case "R" => Reject
    case n => Do(n)
}

// Divide a range at a particular value, putting the value into the right hand side
def bisect(r:Range, n:Int):(Range, Range) = 
    (r.start until n) -> (n until r.end)

def parseWorkflow(s:String):(String, Seq[Rule]) = {
    val s"$name{$ruleText}" = s
    val rules = ruleText.split(',').toSeq.map {
        case s"$c>$num:$dest" => 
            (r:Ratings) => 
                val key = "" + c
                val old = r(key)
                val (missed, caught) = bisect(old, num.toInt + 1)

                val missedR = if missed.isEmpty then None else Some(r.updated(key, missed))
                val caughtR = if caught.isEmpty then None else Some(r.updated(key, caught))
                (caughtR, outcome(dest), missedR)
        case s"$c<$num:$dest" => 
            (r:Ratings) => 
                val key = "" + c
                val old = r(key)
                val (caught, missed) = bisect(old, num.toInt)

                val missedR = if missed.isEmpty then None else Some(r.updated(key, missed))
                val caughtR = if caught.isEmpty then None else Some(r.updated(key, caught))
                (caughtR, outcome(dest), missedR)
        case s => 
            (r) => (Some(r), outcome(s), None)
    }

    name -> rules
}

// def parseRatings(s:String):Map[String, Int] = {
//     val s"{$inner}" = s
//     val entries = inner.split(',').toSeq map { (el) =>
//         val s"$k=$v" = el
//         k -> v.toInt 
//     }
//     entries.toMap
// }

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val Seq(workflowText, ratingsText) = splitByBlanks(lines)

    val workflows = workflowText.map(parseWorkflow).toMap

    type RangeState = (Range, WorkflowResult)

    // always use exclusive ranges
    val initialRatings = Map(
        "x" -> (1 until 4001),
        "m" -> (1 until 4001),
        "a" -> (1 until 4001),
        "s" -> (1 until 4001),
    )

    val acceptBuffer = mutable.Buffer.empty[Ratings]
    val states = mutable.Queue((initialRatings -> Do("in")))

    while states.nonEmpty do 
        val (ratings, action) = states.dequeue()
        action match {
            case Accept => acceptBuffer.append(ratings)
            case Do(wfName) => 
                val wf = workflows(wfName)
                var remaining = Option(ratings)
                for rule <- wf do 
                    remaining match { 
                        case Some(r) => 
                            val (caught, nextAction, missed) = rule(r)
                            for nextR <- caught do states.enqueue(nextR -> nextAction) 
                            remaining = missed
                        case None =>
                            () // nothing left to process 
                    }
            case Reject => 
                () // nothing to do
        }


    println(s"Found ${acceptBuffer.size} acceptable situations")

    def possiblities(r:Ratings):Long = 
        r("x").size.toLong * r("m").size * r("a").size * r("s").size

    val result = acceptBuffer.toSeq.map(possiblities(_)).sum

    println(s"Resulting combinations is $result")

    



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

