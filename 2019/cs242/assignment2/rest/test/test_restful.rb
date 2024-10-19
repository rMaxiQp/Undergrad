require 'minitest/autorun'
require 'net/http'
require 'uri'
require 'json'
require_relative '../restful'


class TestRest < MiniTest::Test

  def test_root

    uri = URI.parse('http://localhost:8000/')
    response = Net::HTTP.get_response(uri)
    body = response.body
    assert_equal body, 'CS242 Assignment2.1!'

  end

  def test_get_actor
    uri = URI.parse('http://localhost:8000/api/actors/Jack%20Warden')
    response = Net::HTTP.get_response(uri).body
    assert_includes response, 'Jack Warden'
  end

  def test_get_movie
    uri = URI.parse('http://localhost:8000/api/movies?name=In%20Country')
    response = Net::HTTP.get_response(uri).body
    assert_includes response, 'In Country'
  end

  def test_put_actor

    uri = URI.parse('http://localhost:8000/api/actors/David%20Dukes')
    put_request = Net::HTTP::Put.new(uri)
    put_request.content_type = 'application/json'
    put_request.body = '{"box_office":500}'

    put_response = Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(put_request)
    end

    assert_equal put_response.code, '200'

    get_body = Net::HTTP.get_response(uri).body
    assert_includes get_body, 'box_office'
  end

  def test_put_movie
    uri = URI.parse('http://localhost:8000/api/movies/Die%20Hard')
    put_request = Net::HTTP::Put.new(uri)
    put_request.content_type = 'application/json'
    put_request.body = '{"total_gross":500}'

    put_response = Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(put_request)
    end

    assert_equal put_response.code, '200'

    get_body = Net::HTTP.get_response(uri).body
    assert_includes get_body, 'total_gross'

  end

  def test_post_actor

    uri = URI.parse('http://localhost:8000/api/actors')
    request = Net::HTTP::Post.new(uri)
    request.content_type = 'application/json'
    request.body = JSON.dump('name' => 'Billy Joe')

    response = Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(request)
    end
    assert_equal response.code, '200'

    get_uri = URI.parse('http://localhost:8000/api/actors?name=Billy%20Joe')
    get_body = Net::HTTP.get_response(get_uri).body
    assert_includes get_body, 'Billy Joe'

  end

  def test_post_movie
    uri = URI.parse('http://localhost:8000/api/movies')
    request = Net::HTTP::Post.new(uri)
    request.content_type = 'application/json'
    request.body = JSON.dump('box_office' => '1000')

    response = Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(request)
    end
    assert_equal response.body, 'A MOVIE NAME IS REQUIRED!'
    assert_equal response.code, '200'
  end

  def test_delete_actor

    uri = URI.parse('http://localhost:8000/api/actors/Bruce%20Willis')
    request = Net::HTTP::Delete.new(uri)
    response = Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(request)
    end
    assert_equal response.code, '200'
    get_uri = URI.parse('http://localhost:8000/api/actors/Bruce%20Willis')
    get_body = Net::HTTP.get_response(get_uri).body
    assert_includes get_body, 'NO ACTOR IS FOUND WITH GIVEN INFORMATION'
  end

  def test_delete_movie
    uri = URI.parse('http://localhost:8000/api/movies/Blind%20Date')
    request = Net::HTTP::Delete.new(uri)
    response = Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(request)
    end
    assert_equal response.code, '200'
    get_uri = URI.parse('http://localhost:8000/api/movies?name=Blind%20Date')
    get_body = Net::HTTP.get_response(get_uri).body
    assert_includes get_body, 'NO MOVIE IS FOUND WITH GIVEN INFORMATION'
  end
end