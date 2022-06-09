import scala.io.Source

class MovieBaseTL(val MovieListFile: String, val NumberOfMovies: Int){
  case class Movie(index: Int, title: String, year: Int, genre: scala.collection.mutable.Set[String])

  /* skipping index 0 like heap. index of movie corresponds to index in array
  Private cause you don't want users to change info in array unrestricted
  Notes to self: if they really wanted to replace movies, etc., they could just change index.
  If they want to add movies, then just do amortized O(1). better to constantly access in O(1) than O(log(n))
  */
  private val _listOfMovies = Array.ofDim[Movie](NumberOfMovies)

  //maps provide O(1) associative look up and insertions/removals since it utilizes hash
  //Movie ratings key is movie index and value is rating. Num reviews key is movie index and value is num of reviews
  private val _movieRatings = scala.collection.mutable.Map[Int, Double]()
  private val _numOfReviews = scala.collection.mutable.Map[Int, Int]()//needed to average movie ratings

  //Heap isn't reliable for maximum recommendation and idk how to code rotations on an AVL tree :) So balboa it is
  //Best time we could do rn is	O(√n) or O(n^1/2)
  private var _movieRankPerGenre = Map[String, BALBOADLL[Movie]]()

  //Never mind, we have avl. But idk how to do proper in order traversal right now
  //private var _movieRankPerGenre = Map[String, BSTAVL[Movie]]()


  //This method reads from file specified in class constructor and creates movieList
  def createMovieDataBase(): Unit ={
    val readFile = Source.fromFile(MovieListFile)
    for(line <- readFile.getLines()){
      val parts = line.split("\\|+")
      if(parts.length == 23) { //need this for irregularities
        val index: Int = parts(0).toInt
        val title: String = parts(1)
        val year: Int = parts(2).split("-")(2).toInt
        //index 4-22 is genre
        val genre = scala.collection.mutable.Set[String]()
        if (parts(5) == "1" || parts(6) == "1" || parts(20) == "1" || parts(22) == "1") {
          genre += "Action"
        }
        if (parts(10) == "1" || parts(14) == "1" || parts(15) == "1" || parts(17) == "1") {
          genre += "Noir"
        }
        if (parts(7) == "1" || parts(8) == "1" || parts(9) == "1" || parts(16) == "1") {
          genre += "Light"
        }
        if (parts(12) == "1" || parts(18) == "1") {
          genre += "Serious"
        }
        if (parts(19) == "1" || parts(13) == "1") {
          genre += "Fantasy"
        }
        if (parts(21) == "1" || parts(11) == "1") {
          genre += "History"
        }
        val movie = Movie(index, title, year, genre)

        _listOfMovies(index) = movie //adding movie object to data structure
      }

      //movie 267 and movie 1358 have issues cause of formatting
    }
  }

  //bunch of getter methods
  def listOfMovies = _listOfMovies
  def getMovieRating(movie : Movie) = _movieRatings(movie.index)
  def movieRatings = _movieRatings
  def movieRankPerGenre = _movieRankPerGenre

  //private function used for balboa
  private def compareMovieRatings(x:Movie, y: Movie):Int = if(_movieRatings(y.index) >= _movieRatings(x.index)) 1 else 0
  //private def compareMovieRatings(x:Movie, y: Movie):Int = if(_movieRatings(y.index) <= _movieRatings(x.index)) 1 else 0

  //gives movies an accurate rating and also ranks them in each genre according to rating
  def assignMovieRatings(userDataBase: UserDataAC) { //need assistance from user class. Takes total O(n)
    val userList = userDataBase.userList
    for(user <- userList){
      if(user != null){
        for((movieId, rating) <- user.userRatings){
          if(movieRatings.contains(movieId)) {
            _movieRatings(movieId) = movieRatings(movieId) + rating
            _numOfReviews(movieId) = _numOfReviews(movieId) + 1
          }
          else{
            _movieRatings += (movieId->rating)
            _numOfReviews += (movieId -> 1)
          }
        }
      }
    }
    for(movieId <- _movieRatings.keys){ //averaging the movie ratings now
      _movieRatings(movieId) = _movieRatings(movieId) / _numOfReviews(movieId)
    }

    //after assigning movie ratings, it sorts them in separate data structure according to genre

    /*
    _movieRankPerGenre = Map(
      "Action" -> new BSTAVL[Movie]((x,y) => compareMovieRatings(x,y)),
      "Fantasy" -> new BSTAVL[Movie]((x,y) => compareMovieRatings(x,y)),
      "History" -> new BSTAVL[Movie]((x,y) => compareMovieRatings(x,y)),
      "Noir" -> new BSTAVL[Movie]((x,y) => compareMovieRatings(x,y)),
      "Light" -> new BSTAVL[Movie]((x,y) => compareMovieRatings(x,y)),
      "Serious" -> new BSTAVL[Movie]((x,y) => compareMovieRatings(x,y))
    )
*/

    _movieRankPerGenre = Map(
      "Action" -> new BALBOADLL[Movie]((x,y) => compareMovieRatings(x,y)),
      "Fantasy" -> new BALBOADLL[Movie]((x,y) => compareMovieRatings(x,y)),
      "History" -> new BALBOADLL[Movie]((x,y) => compareMovieRatings(x,y)),
      "Noir" -> new BALBOADLL[Movie]((x,y) => compareMovieRatings(x,y)),
      "Light" -> new BALBOADLL[Movie]((x,y) => compareMovieRatings(x,y)),
      "Serious" -> new BALBOADLL[Movie]((x,y) => compareMovieRatings(x,y))
    )



    for(movieIndex <- _movieRatings.keys){
      if(_listOfMovies(movieIndex) != null) {
        for (genre <- _listOfMovies(movieIndex).genre) {
          _movieRankPerGenre(genre).insert(_listOfMovies(movieIndex))
        }
      }
    }
  }

  //debating if we want to make movie recommendation method here or in the user class
  //depends on what we decide is most logical
}
