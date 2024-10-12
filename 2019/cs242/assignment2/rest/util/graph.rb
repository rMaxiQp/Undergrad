# Class Graph that is used to store data
class Graph

  # constructor
  # @param String filename
  # @return [Object]
  def initialize(filename)
    # if _args.size > 1
    #   raise ArgumentError, 'ERROR: CAN ONLY TAKE 0 OR 1 ARGUMENT'
    # end
    #
    # if _args.empty?
    #   @actor = []
    #   @movie = []
    #   return
    # end

    data = JSON.parse File.open(filename).read
    @actor = data[0]
    @movie = data[1]
  end

  # getter
  def actor_list
    @actor
  end

  def movie_list
    @movie
  end

  # setter
  def update_actors(actor)
    @actor = actor
  end

  def update_movies(movie)
    @movie = movie
  end

end