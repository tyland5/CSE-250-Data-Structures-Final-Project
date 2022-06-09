import java.io.FileWriter
import java.io.PrintWriter
import scala.io.{BufferedSource, Source}
import scala.util.control.Breaks._
import scala.collection.immutable.Map
import BigDecimal._


class UserDataAC {
  class User(var id:Int, var userRatings: Map[Int,Int]) //store info about user. Map is really Map[movieId, rating]
  var userList: Array[User] = Array.ofDim(944)

  class UserGenreRating(var user: User, var genreRatings: Map[String, Double])
  var userRatingsAverage: Array[UserGenreRating] = Array.ofDim(944)

  class UserGenrePreference(var user: User, var genrePreference: Map[String, Double])
  var userPreferences: Array[UserGenrePreference] = Array.ofDim(944)

  def createUserRatings: Unit = {
    val file: BufferedSource = Source.fromFile("u.data")



    for(line <- file.getLines()){
      val splitLine: Array[String] = line.split("\\s+")
      val id: Int = splitLine(0).toInt
      if(userList(id) == null) {
        val movieRatings = Map(splitLine(1).toInt -> splitLine(2).toInt) //first setup of the map

        //ignore time stamps
        userList(id) = new User(id, movieRatings)
      }
      else{
        var currentMovieRatings = userList(id).userRatings
        currentMovieRatings = currentMovieRatings + (splitLine(1).toInt->splitLine(2).toInt) //updating map
        userList(id) = new User(id, currentMovieRatings)
      }
    }


    //compute the average rating for each genre for a user and the preference.
    //ex. user has average rating of 3.6 for Action movies
    // ex. user has average rating of 2.1 for Fantasy movies

  }
  def makeGenrePreferences(movieBase: MovieBaseTL): Unit ={
    //var userRatings: Array[UserGenreRating] = Array.ofDim(944)
    var ratings: Map[String, Double] = Map()
    var amountOfGenreRatings: Map[String, Int] = Map()


    var movieRating: Double = 0;
    var genrePref: Map[String, Double] = Map()

    for(user <- userList) {
      if (user != null) {
        for ((k, v) <- user.userRatings) {
          val movie: movieBase.Movie = movieBase.listOfMovies(k)
          if(movie!=null) {
            movieRating = movieBase.movieRatings(k)
            if(user.id == 192){
              //println(s"movie ${movie.title}, average rating: ${movieRating}, genre: ${movie.genre}")
            }
            for (genre <- movie.genre) {
              if (!ratings.contains(genre)) {
                ratings = ratings + (genre -> v) // v is the rating for this movie
                amountOfGenreRatings = amountOfGenreRatings + (genre -> 1) // adds 1 to the amount of times this genre has been rated
                genrePref = genrePref + (genre -> movieRating)
                //println(s"#genre: ${genre} user rating: ${v} movie title: ${movie.title} movie genre: ${movie.genre}")

              }
              else {
                val prevRating: Double = ratings(genre)
                ratings = ratings + (genre -> (prevRating + v)) // replaces old rating for that genre
                amountOfGenreRatings = amountOfGenreRatings + (genre -> (amountOfGenreRatings(genre) + 1))
                val prevGenreAverage: Double = genrePref(genre)
                genrePref = genrePref + (genre -> (prevGenreAverage + movieRating)) //TODO divide user genre rating average by movie genre average
                //TODO average the movie genre
                //println(s"!genre: ${genre} rating:${ratings(genre)} movie title: ${movie.title}")
              }
            }
          }
        }

        // manual calculation instead of looping through genres due to extra time added for looping
        if(ratings.contains("Action")){
          ratings = ratings + ("Action" -> (ratings("Action") / amountOfGenreRatings("Action")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(ratings.contains("History")){
          ratings = ratings + ("History" -> (ratings("History") / amountOfGenreRatings("History")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(ratings.contains("Noir")){
          ratings = ratings + ("Noir" -> (ratings("Noir") / amountOfGenreRatings("Noir")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(ratings.contains("Light")){
          ratings = ratings + ("Light" -> (ratings("Light") / amountOfGenreRatings("Light")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(ratings.contains("Serious")){
          ratings = ratings + ("Serious" -> (ratings("Serious") / amountOfGenreRatings("Serious")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(ratings.contains("Fantasy")){
          ratings = ratings + ("Fantasy" -> (ratings("Fantasy") / amountOfGenreRatings("Fantasy")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }

        /////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Genre Average

        if(genrePref.contains("Action")){
          genrePref = genrePref + ("Action" -> (genrePref("Action") / amountOfGenreRatings("Action")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("History")){
          genrePref = genrePref + ("History" -> (genrePref("History") / amountOfGenreRatings("History")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("Noir")){
          genrePref = genrePref + ("Noir" -> (genrePref("Noir") / amountOfGenreRatings("Noir")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("Light")){
          genrePref = genrePref + ("Light" -> (genrePref("Light") / amountOfGenreRatings("Light")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("Serious")){
          genrePref = genrePref + ("Serious" -> (genrePref("Serious") / amountOfGenreRatings("Serious")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("Fantasy")){
          genrePref = genrePref + ("Fantasy" -> (genrePref("Fantasy") / amountOfGenreRatings("Fantasy")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }

        ///////////////////////////////////////////////////////
        // Genre Preference

        if(genrePref.contains("Action")){
          genrePref = genrePref + ("Action" -> (ratings("Action") / genrePref("Action")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("History")){
          genrePref = genrePref + ("History" -> (ratings("History") / genrePref("History")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("Noir")){
          genrePref = genrePref + ("Noir" -> (ratings("Noir") / genrePref("Noir")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("Light")){
          genrePref = genrePref + ("Light" -> (ratings("Light") / genrePref("Light")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("Serious")){
          genrePref = genrePref + ("Serious" -> (ratings("Serious") / genrePref("Serious")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }
        if(genrePref.contains("Fantasy")){
          genrePref = genrePref + ("Fantasy" -> (ratings("Fantasy") / genrePref("Fantasy")).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
        }



        //val userClass: UserGenreRating = new UserGenreRating(user, ratings)
        userRatingsAverage(user.id) = new UserGenreRating(user, ratings)
        userPreferences(user.id) = new UserGenrePreference(user, genrePref)

        /*
        if(user.id == 196){
          println(s"genre: Action , genre ratings: ${amountOfGenreRatings("Action")}")
        }

         */

        ratings = Map()
        amountOfGenreRatings = Map()
        genrePref = Map()
      }
    }
  }

  //recommends movie based off equation in handout. that means recommendations could be movies they already watched!
  def recommendMovies(userID:Int, movieBase:MovieBaseTL, numOfRecommendations: Int):Set[String]={ //or could make it movieBase.Movie
    val genrePrefMap = userPreferences(userID).genrePreference
    val recommendations = scala.collection.mutable.Set[String]() //set type could change based on output
    val genrePreferences = scala.collection.mutable.Map(
      "Action" -> 0.0,
      "Noir" -> 0.0,
      "Light" -> 0.0,
      "Serious" -> 0.0,
      "History" -> 0.0,
      "Fantasy" -> 0.0,
    )

    for(genre <- genrePrefMap.keys){
      genrePreferences(genre) = genrePrefMap(genre)
    }

    val rankedAction = movieBase.movieRankPerGenre("Action").begin
    val rankedFantasy = movieBase.movieRankPerGenre("Fantasy").begin
    val rankedNoir = movieBase.movieRankPerGenre("Noir").begin
    val rankedHistory = movieBase.movieRankPerGenre("History").begin
    val rankedSerious = movieBase.movieRankPerGenre("Serious").begin
    val rankedLight = movieBase.movieRankPerGenre("Light").begin

    while(recommendations.size < numOfRecommendations){
      //leaving this as recommended won't break the code because it will be overwritten
      var recommendedMovie = rankedAction
      var value:Double = -1.0 //making this a negative helps because it will help pass the first setting of the variable
      var temp: Double = 0.0

      if(rankedAction.hasNext){
        value = movieBase.getMovieRating(recommendedMovie()) * genrePreferences("Action")
      }

      if(rankedFantasy.hasNext) {
        temp = movieBase.getMovieRating(rankedFantasy()) * genrePreferences("Fantasy")
        if (temp > value) {
          recommendedMovie = rankedFantasy
          value = temp
        }
      }

      if(rankedNoir.hasNext) {
        temp = movieBase.getMovieRating(rankedNoir()) * genrePreferences("Noir")
        if (temp > value) {
          recommendedMovie = rankedNoir
          value = temp
        }
      }

      if(rankedHistory.hasNext) {
        temp = movieBase.getMovieRating(rankedHistory()) * genrePreferences("History")
        if (temp > value) {
          recommendedMovie = rankedHistory
          value = temp
        }
      }

      if(rankedSerious.hasNext) {
        temp = movieBase.getMovieRating(rankedSerious()) * genrePreferences("Serious")
        if (temp > value) {
          recommendedMovie = rankedSerious
          value = temp
        }
      }

      if(rankedLight.hasNext) {
        temp = movieBase.getMovieRating(rankedLight()) * genrePreferences("Light")
        if (temp > value) {
          recommendedMovie = rankedLight
          value = temp
        }
      }

      //println(s"${recommendedMovie().title} won with value $value")
      recommendations += recommendedMovie().title
      recommendedMovie.next()
    }

    recommendations.toSet
  }
}
