module PasswordProtectedPublish
  
  def publish(session, funcall)
    amf = #RtmpFuncall(funcall)
    publish_name = amf.args[1]
    
    {_rawname, args} = Erlang.http_uri2.parse_path_query(publish_name)
    login = args["login"]
    password = args["password"]
    
    if login != "user" || password != "password"
      IO.puts "DENY RECORD"
      Erlang.rtmp_session.reject_connection(session)
    else
      'unhandled
    end
  end
end
