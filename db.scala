object db {
  def main(args: Array[String]): Unit = {
    case class Book(title: String, authors: List[String])

    val books: List[Book] =
      List(Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald”, ”Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard”, ”Wadler, Phil")),
    Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
        Book(title = "Effective Java2",  //if an author published three books, triple authors, so need "distinct"
          authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
/*To find the titles of books whose author’s name is “Bird”: */
    val b_title = for (b <- books; a <- b.authors if a startsWith "Bird,")
    yield b.title
    println("b_title : "+ b_title)

    /*To find all the books which have the word “Program” in the title:  */
    val b_title2 = for ( b <- books if (b.title indexOf "Program") >= 0 ) yield b.title
    println("b_title2 : "+ b_title2)

/* To find the names of all authors who have written at least two
books present in the database. */
    val b_name1 =
      for {
        b1 <- books
        b2 <- books
        if b1 != b2  //for the different books
        a1 <- b1.authors
        a2 <- b2.authors
        if a1 == a2  //if one author of each book is same as the other
      } yield a1
    println("b_name1 :" + b_name1)  //but duplicated output.
/* remove duplicated one
 * To find the names of all authors who have written at least two
books present in the database. */
    val b_name2 =
      for {
        b1 <- books
        b2 <- books
        if b1.title < b2.title //for the different books
        a1 <- b1.authors
        a2 <- b2.authors
        if a1 == a2  //if one author of each book is same as the other
      } yield a1
    println("b_name2(remove duplicted) :" + b_name2)

    /*We must remove duplicate authors who are in the results
list twice.
This is achieved using the distinct method on sequences    */
    val b_name_distinct =
      { for {
        b1 <- books
        b2 <- books
       if b1.title < b2.title
//        if b1.title != b2.title
        a1 <- b1.authors
        a2 <- b2.authors
        if a1 == a2
      } yield a1
      }.distinct
    println("b_name_distinct :"+b_name_distinct )

    /* Compute sets instead of sequences */
    println("books.toSet : "+ books.toSet)
    val bookSet = books.toSet
    for {
      b1 <- bookSet
      b2 <- bookSet
      if b1 != b2
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1
    println("bookSet     : "+ bookSet)  //same as "books.toSet"

    /* traslate for..yield to high-order functions  */
    val b_title_startWith = for (b <- books; a <- b.authors if a startsWith "Bird")
      yield b.title
    println("b_title-startWith : "+ b_title_startWith)
    /* we have two generator (b<- books; a <-b.authors) so, flatMap  */
    val b_title_st_M = books flatMap(b =>  for(a <- b.authors if a startsWith "Bird" )yield b.title)
    println("b_title_st_M : " + b_title_st_M)

    //2.
    println("************************")
    println(for(b <- books if(b.title indexOf "Program") >= 0) yield b.title)
    println("**** end of b.title ****")

    /* withFilter(still error) - intrim version: single generator -> need to translate to map */
//    val b_title_st_M_withFilter = books flatMap(b =>  for(a <- b.authors withFilters (a => a startsWith "Bird" ))yield b.title)
//    println("b_title_st_M_withFilter : " + b_title_st_M_withFilter)

//    val b_title_st_map1 = books flatMap(b => b.authors withFilter(a => (a startsWith "Bird")) map(y => y.title ))  //error
    val b_title_st_map = books flatMap(b => b.authors withFilter(a => (a startsWith "Bird")) map ( x=> x ))  //not as intended
    println("b_title_st_map : "+ b_title_st_map )
    val find_b_title = for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title+" : find"
    println(find_b_title)



    //    for ( b <- books if b.title.indexOf("Program") >= 0 ) yield b.title  //same as above
    //    for ( b <- books if b.title indexOf "Program" >= 0 ) //error
  }//end of main

}



