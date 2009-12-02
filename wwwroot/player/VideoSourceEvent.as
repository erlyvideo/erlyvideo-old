package
{
  import flash.events.Event;
  
  public class VideoSourceEvent extends flash.events.Event
  {
    public static const CONNECTED:String = "CONNECTED";
    public static const DISCONNECT:String = "DISCONNECT";
    public static const STREAM_READY:String = "STREAM_READY";
    public static const STREAM_FAILED:String = "STREAM_FAILED";
    public static const METADATA:String = "METADATA";
    public static const TICK:String = "TICK";
    public static const FILE_NOT_FOUND:String = "FILE_NOT_FOUND";
    public static const FINISHED:String = "FINISHED";
    
		public var payload : Object;

    public function VideoSourceEvent(type:String, obj : Object = null) {
      super(type);
			payload = obj;
    }
  }
  
}
