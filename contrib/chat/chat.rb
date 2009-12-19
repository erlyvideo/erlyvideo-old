#!/usr/bin/env ruby

require 'rubygems'
require 'sinatra'
require 'net/http'
require File.dirname(__FILE__)+"/json_session"

erlmedia_path = File.dirname(__FILE__)+"/../../ebin/erlmedia.app"
if !File.exists?(erlmedia_path)
  raise "You must initialize erlyvideo first, no ebin/erlmedia.app found"
end
erlmedia = File.read(erlmedia_path).split(/\r\n/).map {|s| s.strip}
RTMP_HOSTNAME = erlmedia.grep(/\{host,/).first[/"(.*)"/, 1]
SECRET = erlmedia.grep(/\{secret_key,/).first[/"(.*)"/, 1]
ERLY_HTTP = erlmedia.grep(/\{http_port,/).first[/(\d+)/, 1]

set :public, File.expand_path(File.dirname(__FILE__) + '/../../wwwroot')

# use Rack::Session::JsonSession, :key => 'rack.session',
#                                 :path => '/',
#                                 :expire_after => 2592000,
#                                 :secret => SECRET

get '/' do
  @hostname = RTMP_HOSTNAME
  session = {:user_id => rand(100), :channels => [10, 12]}
  @session = Rack::Session::JsonSession.pack_session(session, SECRET)
  erb :index
end

post '/say' do
  puts params[:message].inspect
  Net::HTTP.post_form(URI.parse("http://localhost:#{ERLY_HTTP}/channels/10/message"),
                                {:message => params[:message]})
end

__END__

@@ layout
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru" lang="ru">
<head>
    <title>Rails - Erlyvideo interaction.</title>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <script type="text/javascript" src="/js/jquery.js"></script>
</head>
<body>
  <%= yield %>
</body>
</html>
@@ index
<script type="text/javascript" src="/js/netconnection.js"></script>
<object width="0" height="0" type="application/x-shockwave-flash" id="push-flash" name="push-flash" data="push/push.swf">
  <param name="allowScriptAccess" value="always"/>
  <param name="allowFullScreen" value="true"/>
  <param name="wmode" value="transparent"/>
  <param name="flashvars" value="server=<%= @hostname %>&session=<%= @session %>"/>
</object>

<div class="container">
  <div class="span-24 last"><h3><a href="/">Main page</a></h3></div>
  
  <div class="span-24 last">
    <h3>Chat</h3>
    <div id="chat" style="height: 300px; overflow: scroll">
      
    </div>
    <form action="/say" method="post" onsubmit="try {var self = $(this); var postData = self.serialize(); self.find('input').attr('disabled', true); $.post(this.action, postData, function() {self.find('input[type=text]').val(''); self.find('input').attr('disabled', false);}, 'text'); } catch(e) { alert(e);}; return false">
      <input type="text" name="message">
      <input type="submit" value="Send message">
    </form>
  </div>
</div>
