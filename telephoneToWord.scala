
import scala.io.Source
object telephoneToWord {
  def main(args: Array[String]): Unit = {
    val in = Source.fromFile("C:\\scala_coursera\\lec1_Functional Programming Principles in Scala\\2\\funsets\\lec1_6_1\\src\\main\\scala\\linuxwords.txt")
//    val words = in.getLines //change to List since "words groupBy wordCode" caused error(groupBy not applicable to String)
//val words = in.getLines.toList
/*key not found error due to hyphen, _ ,etc characters, so filter out these characters */
val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))


    val mnem = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI" , '5' -> "JKL",
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
    )
    /* Invert the mnem map to give a map from chars 'A'... 'Z' to '2'...'9' */
    val charCode: Map[Char, Char] =
      for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
    /* Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, ...) */
    println("** charCode : "+ charCode)
    println("*charCode.toList.sorted : " + charCode.toList.sorted)
/* for if example   */
//    def scalaFiles =
//      for {
//        file <- filesHere
//        if file.getName.endsWith(".scala")
//      } yield file

    /* Maps a word to the digit String it can represent i.e "Java" -> "5282" */
    val testPhrase = "I love You".toUpperCase

    val phaToNum =
    for { e  <- testPhrase ; (word, digit) <- charCode.toList.sorted  ; if(e == word) }yield  (e -> digit)
    println(phaToNum)

    val phaToNum2 =
    (for { e  <- testPhrase ; (word, digit) <- charCode.toList.sorted  ; if(e == word) }yield  digit)
    println(phaToNum2)

    val testPhrase2 = "Java"
    def phaToNum2Func( phrase : String) = {
      val returnDigit = for { e  <- phrase.toUpperCase ; (word, digit) <- charCode.toList.sorted  ; if(e == word) }yield  digit
      returnDigit
    }
    println(phaToNum2Func(testPhrase2))

    // lecture version
    def wordCode(word: String): String =
    //each element in String,"Java"  map to charCode Map, (E -> 3, X -> 9 ...),
    // then map to value(corresponding digit show up)
      word.toUpperCase map charCode
    println("word map charCode : wordCode(\"Java\") : " + wordCode("Java") )

    /*A map to digit strings to the words that represent them.
        *"5282" -> List("Java", "Kata", "Lava",...]   */
//    val wordsForNum: Map[String, Seq[String]]
//    val inputDigitStr = "5282"
//    def  wordsForNum(digitStr: String) = {
//     val digitToWord = (digitStr map mnem).toSeq
//     digitToWord
//    }
//     println(" ^^ wordsForNum(inputDigitStr) : " + wordsForNum(inputDigitStr)) //vector
//    val list_wordForNum = (wordsForNum(inputDigitStr))  //seq[String]
//    println(list_wordForNum)
//    println(list_wordForNum(0))
//    list_wordForNum(0)
//    val (xs, ys, zs, ks) = list_wordForNum
////    val (xs:List[String], ys:List[String] ,zs:List[String] ,ks:List[String]) =list_wordForNum
////
//////    xs map (x => ys map (y => zs ( map (z => ks (map (k  => (x,y,z,k)))))))  //error
//    val result = for { x <- list_wordForNum(0) ; y <-list_wordForNum(1); z <-list_wordForNum(2); k <- list_wordForNum(3)} yield (x, y, z, k)
//    println(result)
//
//    val wordsForNum2 =
//       for ((digit, str) <- mnem; ltr <- str; ds <- inputDigitStr; if (digit == ds)) yield ltr
//    println("&&" + wordsForNum2)

    /* A map to digit strings to the words that represent them.
  *"5282" -> List("Java", "Kata", "Lava",...]    */
/* group the List of words, List("Java", "Kata", "Lava",...) which have same number string("5282" )
Given the wordCode and retrive the List
* Missing number shoud map to the empty set, eg. "1111" -> List() */
   val wordForNum: Map[String, Seq[String]] =  words groupBy wordCode withDefaultValue Seq() //Default shd be Seq()
    /*key not found error due to hyphen, _ ,etc characters  */
    println("**** wordForNum : ")
//println(wordForNum)
    /* "7" -> "PQRS" , there is not word "PQRS" ind words. */
    println(wordForNum("7225247386" take 1)) //since there is no word with just one number "7", shd be Seq(List())

    /*Reference    */
    //   /*print each element : sol 1  */
    //    for ((k,v) <- wordForNum )
    //       println(" * key : "+k +" * value : "+v)
    //    /*print each element : sol 2  */
    //    wordForNum foreach (x  => println (x._1 + "=> "+ x._2))
    //    /*print each element : sol 3  */
    //    wordForNum {case (k, v) => println (k + "-->" + v)}
    for(e <- wordForNum if e._1 equals("5282"))
      println(e)
    wordForNum foreach { x  =>
      if (x._1 =="5282")
        println (x._1 + "=> "+ x._2)
    }
    /*Return all ways to encode a number as a list of words   */
    def encode(number: String): Set[List[String]] =
      if(number.isEmpty) Set(List())
        /*Determine how many characters you take from this number to form first word */
      else{
        for {
          split <- 1 to number.length  //to form first word  //Error : shd be Set[List[String]] instead of index
          word <- wordForNum(number take split) //find out what the first word shoud be
          rest <- encode(number drop split) //rest of the number
        } yield word :: rest
      }.toSet  //Set[List[String]] instad of index

    println("** encode(\"7225247386\") : "+ encode("7225247386"))  //NoSuchElementException : key not found : 7
    def translate(number: String): Set[String] = {
      encode(number) map (_ mkString " ")
    }
    println("## encode(number) map (_ mkString \" \") ")
    println(translate("7225247386"))
    /* Ref take */
    val list1 = List(1,2,3,4,5,6,7,8,9)
    println("list1.take(3) : "+ list1.take(3))  //first take 3 <-> drop
    println(list1 take 3) //same as above

  }//end of main

}

