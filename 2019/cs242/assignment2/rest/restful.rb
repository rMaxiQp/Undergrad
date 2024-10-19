require 'sinatra'
require 'sinatra/namespace'
require 'json'
require_relative 'util/graph'
require_relative 'util/actor_util'
require_relative 'util/movie_util'

# configuration would be set when the program starts
configure do
  set :port, 8000
  set :graph, Graph.new('data.json')
end

# root router
get '/' do
  'CS242 Assignment2.1!'
end

namespace('/api') do

  get '/movies/even/all' do
    content_type :json
    return JSON.pretty_generate settings.graph.movie_list
  end

  # filter out all actors that don't have provided {attr_value}
  get '/actors' do
    logger.info 'retrieving actor info'

    actors = parse_actor_param params, settings.graph

    logger.info 'retrieving actor info done...'

    if actors.empty?
      content_type :text
      return 'NO ACTOR IS FOUND WITH GIVEN INFORMATION'
    else
      content_type :json
      return JSON.pretty_generate actors
    end

  end

  # filter out all movies that don't have provided {attr_value}
  get '/movies' do
    logger.info 'retrieving movie info'

    movies = parse_movie_param params, settings.graph

    logger.info 'retrieving movie info done...'

    if movies.empty?
      content_type :text
      return 'NO MOVIE IS FOUND WITH GIVEN INFORMATION'
    else
      content_type :json
      return JSON.pretty_generate movies
    end
  end

  # return the first Actor object that has {attr_value}
  get '/actors/:name' do
    logger.info 'retrieving first actor with given attribute'

    actors = parse_actor_param params, settings.graph

    logger.info 'retrieving first actor with given attribute done...'
    if actors.empty?
      logger.warn 'NO ACTOR IS PRODUCED WITH GIVEN INFORMATION'
      content_type :text
      return 'NO ACTOR IS FOUND WITH GIVEN INFORMATION'
    else
      content_type :json
      return JSON.pretty_generate actors[0]
    end
  end

  # return the first movie object that has {attr_value}
  get '/movies/:name' do
    logger.info 'retrieving first movie with given attribute'

    puts params

    movies = parse_movie_param params, settings.graph

    logger.info 'retrieving first movie with given attribute done...'

    if movies.empty?
      logger.warn 'NO ACTOR IS PRODUCED WITH GIVEN INFORMATION'
      content_type :text
      return 'NO MOVIE IS FOUND WITH GIVEN INFORMATION'
    else
      content_type :json
      return JSON.pretty_generate movies[0]
    end

  end

  # update standing content in backend
  put '/actors/:name' do
    logger.info 'updating actor info'

    new_value = JSON.parse(request.body.read)

    update_actor params['name'], settings.graph, new_value

    logger.info 'updating actor info done...'

  end

  # update standing content in backend
  put '/movies/:name' do
    logger.info 'updating movie info'

    new_value = JSON.parse(request.body.read)

    update_movie params['name'], settings.graph, new_value

    logger.info 'updating movie info done...'
  end

  # add actor content to backend
  post '/actors' do
    logger.info 'adding new actor'

    new_value = JSON.parse(request.body.read)

    if new_value.nil? || new_value['name'].nil?
      logger.warn 'NO ACTOR NAME IS PROVIDED'
      content_type :text
      return 'AN ACTOR NAME IS REQUIRED!'
    end

    add_actor new_value, settings.graph

    logger.info 'adding new actor done...'
  end

  # add actor content to backend
  post '/movies' do
    logger.info 'adding new movie'

    new_value = JSON.parse(request.body.read)

    if new_value.nil? || new_value['name'].nil?
      logger.warn 'NO MOVIE NAME IS PROVIDED'
      content_type :text
      return 'A MOVIE NAME IS REQUIRED!'
    end

    add_movie new_value, settings.graph

    logger.info 'adding new movie done...'
  end

  # REMOVE actor content from backend
  delete '/actors/:name' do
    logger.info 'deleting actor'

    delete_actor params['name'], settings.graph

    logger.info 'deleting actor done...'
  end

  # REMOVE movie content from backend
  delete '/movies/:name' do
    logger.info 'deleting movie'

    delete_movie params['name'], settings.graph

    logger.info 'deleting movie done...'
  end
end
