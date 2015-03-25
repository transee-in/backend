#!/usr/bin/env ruby
# Downloads and parse city routes to transport list
# like this `http://bus125.ru/php/getRoutes.php?city=vladivostok`
# to `transee_city_vladivostok.config`, from Kondrahin system
require 'json'
require 'net/http'

content = Net::HTTP.get(URI.parse(ARGV[0])).force_encoding("UTF-8")
# content = File.read('./file.json')
json = JSON.load(content)
transports = {}
max_id = 0

json.each_with_index do |obj, idx|
  if transports.key?(obj['name'])
    transports[obj['name']][:id] << obj['id']
  else
    transports[obj['name']] ||= {
      id: [obj['id']], type: obj['type']
    }
  end
end

Hash[transports.sort].map do |name, obj|
  id = obj[:id].map(&:to_s).join(', ')
  id = "[#{id}],"
  formatted_name = if name =~ /[^\d]/ui
    %|"#{name}"/utf8|
  else
    %|"#{name}"|
  end

  max_id = id.size if id.size > max_id

  [id, obj[:type], formatted_name]
end.each do |obj|
  puts %|, {%-#{max_id}s <<"%s"/utf8>>, <<%s>>}| % obj
end
