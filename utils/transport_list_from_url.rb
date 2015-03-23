#!/usr/bin/env ruby
# Downloads and parse city routes to transport list
# like this `http://bus125.ru/php/getRoutes.php?city=vladivostok`
# to `transee_city_vladivostok.config`, from Kondrahin system
require 'json'
require 'net/http'

content = Net::HTTP.get(URI.parse(ARGV[0])).force_encoding("UTF-8")
# content = File.read('./file.json')
json = JSON.load(content)
transport_types = {}

json.each_with_index do |obj, idx|
  # they duplicate transport for two directions
  if idx % 2 == 0
    transport_types[obj['type']] ||= []

    name = if obj['name'] =~ /[^\d]/ui
      %|"#{obj['name']}"/utf8|
    else
      %|"#{obj['name']}"|
    end

    transport_types[obj['type']] << [obj['id'], name]
  end
end

transport_types.each do |type, transports|
  puts "-- type: #{type}"
  transports.sort_by! { |id, name| id }
  transports.each_slice(3) do |bucket|
    bucket.each do |id, name|
      print %|, {"#{id}", <<#{name}>>}|
    end
    puts
  end
end
