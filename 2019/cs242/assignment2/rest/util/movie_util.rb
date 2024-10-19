require 'json'
require_relative 'actor_util'

# @param [Hash] params
# @param [Object] graph
# @return [Object]
def parse_movie_param(params, graph)
  names = parse_param params['name']
  bos = parse_param params['box_office']
  years = parse_param params['year']
  movies = graph.movie_list
  result = []

  movies.each_key do |movie|

    next if !names.empty? && !(names.include? movies[movie]['name'])
    next if !bos.empty? && !(bos.include? movies[movie]['box_office'].to_s)
    next if !years.empty? && !(years.include? movies[movie]['year'].to_s)

    result << movies[movie]
  end
  result
end

def update_movie(name, graph, new_value)
  movies = graph.movie_list

  movies.each_key do |movie|
    next unless movies[movie]['name'] == name

    new_value.each do |key, value|
      puts key
      puts value
      movies[movie][key] = value
    end
  end

  graph.update_movies movies
end

def add_movie(new_value, graph)
  movies = graph.movie_list
  movies[new_value['name']] = new_value
  graph.update_movies movies
end

def delete_movie(name, graph)
  movies = graph.movie_list
  movies.delete(name)
  graph.update_movies movies
end