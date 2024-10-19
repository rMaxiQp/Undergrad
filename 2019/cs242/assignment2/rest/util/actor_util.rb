require 'json'

# general utility functions that are used to extract information from parameters
def parse_param(param)
  if param.nil?
    []
  else
    param.split(',')
  end
end

# @param [Hash] params
# @param [Object] graph
# @return [Object]
def parse_actor_param(params, graph)
  names = parse_param params['name']
  grosses = parse_param params['gross']
  ages = parse_param params['age']
  actors = graph.actor_list
  result = []

  actors.each_key do |actor|
    next if !names.empty? && !(names.include? actors[actor]['name'])
    next if !grosses.empty? && \
            !(grosses.include? actors[actor]['total_gross'].to_s)
    next if !ages.empty? && !(ages.include? actors[actor]['age'].to_s)

    result << actors[actor]
  end
  result
end

def update_actor(name, graph, new_value)
  actors = graph.actor_list

  actors.each_key do |actor|
    next unless actors[actor]['name'] == name

    new_value.each do |key, value|
      actors[actor][key] = value
    end
  end

  graph.update_actors actors
end

def add_actor(new_value, graph)
  actors = graph.actor_list
  actors[new_value['name']] = new_value
  graph.update_actors actors
end

def delete_actor(name, graph)
  actors = graph.actor_list
  actors.delete(name)
  graph.update_actors actors
end

