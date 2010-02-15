#!/usr/bin/env ruby

$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))
$LOAD_PATH.unshift(File.dirname(__FILE__))
require 'ernie'

Ernie.log = Logger.new("script.log")
Ernie.log.level = Logger::DEBUG

module Test
  # Add two numbers together
  def add(a, b)
    Ernie.log.debug "ZZZ"
    a + b
  end

  # Return the given number of bytes
  def bytes(n)
    'x' * n
  end

  # Sleep for +sec+ and then return :ok
  def slow(sec)
    sleep(sec)
    :ok
  end

  # Throw an error
  def error
    raise "abandon hope!"
  end
end

module Application
  def connect(*args)
    puts "Connect: #{args.inspect}"
    
    nil
  end
  
  def getStreamLength(movie)
    puts "Movie length: #{movie.inspect}"
    1000
  end
end

Ernie.expose(:application, Application)
Ernie.expose(:test, Test)
