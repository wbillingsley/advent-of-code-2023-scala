import scala.io.*

// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star3/solver.scala
// (or select the "star3" branch from GitHub)

// The colours of the balls
enum Colour:
    case Red, Green, Blue

// A Drawing is some number of balls of each colour
type Drawing = Map[Colour, Int]

// A game has an Id and some number of drawings
type Game = (Int, Seq[Drawing])

// How many balls of each colour exist
def ballCount = Map(
    Colour.Red -> 12, Colour.Green -> 13, Colour.Blue -> 14
)

// A game is a game id, followed by text
val game = raw"Game (\d*):(.*)".r

// Regexes that capture the number of balls drawn for each colour
val red = raw"(\d*) red".r
val green = raw"(\d*) green".r
val blue = raw"(\d*) blue".r

// This comes out compact and readable, though it is a mix of techniques:
// The game regex includes its colon (and captures the remainder of the string)
// For the drawings, though, we manually split on ';' and ',' before using the colour regexes
def lineToGame(s:String):Game = s match {
    case game(id, text) => id.toInt -> (
        for drawingText <- text.split(";").toSeq
        yield
            drawingText.split(",").toSeq.map(_.trim).map({
                case red(n) => Colour.Red -> n.toInt
                case green(n) => Colour.Green -> n.toInt
                case blue(n) => Colour.Blue -> n.toInt
            }).toMap
    )
}

// A game is valid if for all its drawings, for every colour the number of balls drawn is <= how many of that colour there are
def isValid(g:Game):Boolean = 
    val (_, drawings) = g
    drawings.forall { d =>
        Colour.values.forall { c =>
            d.getOrElse(c, 0) <= ballCount(c)
        }
    }

// We need the minimum number of each colour of ball that would make the game valid, multiplied together
def minimumPower(g:Game):Int =
    val (_, drawings) = g
    (for c <- Colour.values.toSeq yield drawings.map((d) => d.getOrElse(c, 0)).max).product

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines()
    val games = lines.map(lineToGame).toSeq
    val minPowers = games.map(minimumPower)
    println(minPowers)
    println(s"Sum of minimum powers is ${minPowers.sum}")

