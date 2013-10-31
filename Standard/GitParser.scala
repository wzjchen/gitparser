import java.io._
import scala.io.Source
// import scala.math._

// Input from git log --pretty="%ci" --name-only impl

object GitParser {
   def main(args: Array[String]) {
      println("Following is the content read:" )
      val files = parseGitLog("impl-files")
      val xml = xmlMerge("result.xml",files)
      val writer = new PrintWriter(new File("result_new.xml" ))
      writer.write(xml)
      writer.close()
      // files.foreach{
      //    case (a,b) =>
      //       print(a)
      //       print(" ")
      //       print(b)
      //       print("\r\n")
      // }
   }
   def xmlMerge(file:String,files:List[(String,String)]) = {
      var new_xml = ""
      var source = Source.fromFile( file ).mkString
      // println(source)
      // var source = Source.fromFile( file ).buffered.toIterator
      while (source.contains("<name>")) {
         val start = source.indexOf("<name>")
         val end = source.indexOf("</name>")
         val toMatch = source.slice(start+7,end).replaceAll("\\\\","/")
         println(toMatch)
         val toInsert = files.find{ case(name,date) => name==toMatch }
            match {
               case Some(value) => value._2
               case None => ""
            }
         new_xml = new_xml ++ source.take(end+7)
         new_xml = new_xml ++ "<lastUpdate>" ++ toInsert ++ "</lastUpdate>"
         source = source.drop(end+7)
      }
      new_xml ++ source
   }
   def parseGitLog(file:String):List[(String,String)] = {
      var lastDate = ""
      var visited = List[String]()
      var files = List[(String,String)]()
      for (line <- Source.fromFile( file ).getLines() ) {
        if (line.length > 0 && (line.substring(0,4) forall Character.isDigit)) {
          lastDate = line
        } else {
          if (!visited.contains(line)) {
            visited ::= line
            files ::= (line,lastDate)
          }
        }
      }
      files
   }
}