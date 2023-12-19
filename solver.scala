// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star35/solver.scala
// (or select the "star35" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

// Parses a line of puzzle
def decompose1(line:String) = 
    val s"$dir $dist (#$col)" = line

    val parsedDir = dir match {
        case "R" => East
        case "U" => North
        case "D" => South
        case "L" => West
    }
    (parsedDir, dist.toInt, col)

type Ratings = Map[String, Int]


enum WorkflowResult:
    case Do(name:String)
    case Reject
    case Accept

type Rule = Ratings => Option[WorkflowResult]

type Workflow = Seq[Rule]

import WorkflowResult.*

def outcome(s:String) = s match {
    case "A" => Accept
    case "R" => Reject
    case n => Do(n)
}

def parseWorkflow(s:String):(String, Seq[Rule]) = {
    val s"$name{$ruleText}" = s
    val rules = ruleText.split(',').toSeq.map {
        case s"$c>$num:$dest" => 
            (r:Ratings) => 
                if r("" + c) > num.toInt then Some(outcome(dest)) else None
        case s"$c<$num:$dest" => 
            (r:Ratings) => 
                if r("" + c) < num.toInt then Some(outcome(dest)) else None
        case s => (_) => Some(outcome(s))
    }

    name -> rules
}

def parseRatings(s:String):Map[String, Int] = {
    val s"{$inner}" = s
    val entries = inner.split(',').toSeq map { (el) =>
        val s"$k=$v" = el
        k -> v.toInt 
    }
    entries.toMap
}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val Seq(workflowText, ratingsText) = splitByBlanks(lines)

    val workflows = workflowText.map(parseWorkflow).toMap

    val ratings = ratingsText.map(parseRatings)

    @tailrec
    def process(part:Ratings, wf:Workflow = workflows("in")): WorkflowResult = {
        val applicable = wf.find(_.apply(part).nonEmpty)
        applicable match {
            case Some(r) => r(part) match {
                case Some(Do(name)) => process(part, workflows(name))
                case Some(done) => done
            } 
        }

    }

    println(workflows.size)

    val accepted = ratings.filter { (p) => process(p) == Accept }
    val result = accepted.map((p) => p("x") + p("m") + p("a") + p("s")).sum
    println(s"Result is $result")



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

