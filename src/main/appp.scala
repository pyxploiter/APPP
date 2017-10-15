package main
import scala.collection.mutable.ListBuffer;
import scala.io.Source

object Demo {
   def main(args: Array[String]) {
     val codeFile = Source.fromFile("examples/if.appp")
     val sourceCode = codeFile.mkString
     println(sourceCode)
   }
}