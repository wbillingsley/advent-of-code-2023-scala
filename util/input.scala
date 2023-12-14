package util

import scala.collection.mutable

// Splits the input by blank lines
// returning each puzzle as a Seq[Seq[Char]]
def splitByBlanks(lines:Seq[String]):Seq[Seq[String]] = 
    val puzzleBuf = mutable.Buffer.empty[Seq[String]]
    var cursor = lines
    while (!cursor.isEmpty) do
        val puz = cursor.takeWhile(!_.isBlank())
        puzzleBuf.append(puz)
        cursor = cursor.drop(puz.length).dropWhile(_.isBlank())

    puzzleBuf.toSeq
