import java.io.{FileWriter, PrintWriter}

object mainTLAC extends App {

  //this code here makes the movie data base and its respective data structure
  val movieBase = new MovieBaseTL("./u.item", 1683)
  movieBase.createMovieDataBase()

  //list of all movies that was successfully added to data structure. Should be missing 2 due to malformed input
  val movieList = movieBase.listOfMovies


  /*
  //Movie addition works perfectly
  for(movie<- movieList){
    if(movie!=null) { //index 0 is null and 2 are missing
      println(s"${movie.index}. ${movie.title}. ${movie.year}. ${movie.genre}")
    }
  }
   */

  //creates user data base
  val userBase = new UserDataAC()
  userBase.createUserRatings

  movieBase.assignMovieRatings(userBase)
  userBase.makeGenrePreferences(movieBase)

  /*
  val test = userBase.userRatingsAverage(1).genreRatings
  for(genre <- test){
    println(s"User 1 has genre averages for ${genre._1} of ${genre._2}")
  }
  */

  /*
  val movieRank = movieBase.movieRankPerGenre
  val actionMoviesRanked = movieRank("Action").begin
  var i = 0
  while(actionMoviesRanked.hasNext){
   println(s"${actionMoviesRanked().title} is ranked $i with a rating of ${movieBase.getMovieRating(actionMoviesRanked())}")
    i+=1
    actionMoviesRanked.next()
  }
  */

  /*
  var userPref = userBase.userPreferences(50).genrePreference
  for(genre <- userPref.keys){
      println(s"User 50 has pref value ${userPref(genre)} for genre $genre")
  }
 */

  /*
  //returns one user's recommendations but not necessarily in order of most recommended since the data structure is a set
  val user1Recs = userBase.recommendMovies(1,movieBase,30)
  println("User 1's recommendations are")
  for(movie <- user1Recs){
    println(s"${movie}")
  }
  */

  //prints all users' recommendations into an output file
  val writeFile = new PrintWriter(new FileWriter("./userRecommendations.txt"))
  for(user <- userBase.userList){
    if(user!=null){
      writeFile.println(s"User ${user.id}'s recommendations are ")
      for(movie <- userBase.recommendMovies(user.id,movieBase,10)){ //line to change amount of recommendations
        writeFile.println(s"${movie}")
      }
      writeFile.println("\n") //two empty lines between users
    }
  }

}
