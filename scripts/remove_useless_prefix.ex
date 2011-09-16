module RemoveUselessPrefix
  def play(session, amf)
    funcall = #RtmpFuncall(amf)
    ['null, path | args] = funcall.args
    re = ~r{^\w+:(.*)$}
    case re.run(path)
    match nil
      'unhandled
    match [_fullpath, clean_path]
      {'unhandled, session, funcall.update_record('args: ['null, clean_path |args])}
    end
  end
end
