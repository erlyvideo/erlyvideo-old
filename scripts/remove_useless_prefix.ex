module RemoveUselessPrefix
  
  def play(session, amf)
    cleanup(session, amf)
  end
  
  def getStreamLength(session, amf)
    cleanup(session, amf)
  end
  
private  
  def cleanup(session, amf)
    funcall = #RtmpFuncall(amf)
    ['null, path | args] = funcall.args
    
    re = ~r{^(\w+):(.*)$}
    case re.run(path)
    match nil
      'unhandled
    match [_fullpath, _prefix, clean_path]
      if clean_path.starts_with("//") % it is an url
        'unhandled
      else
        IO.puts "Cut unused prefix: #{_prefix}"
        {'unhandled, session, funcall.update_record('args: ['null, clean_path |args])}
      end
    end
  end
end
