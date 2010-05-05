require 'test/unit'
# require 'test_help'


class Test::Unit::TestCase
  def restart_erlyvideo
    `#{File.dirname(__FILE__)}/../contrib/erlyctl restart`
  end
end

