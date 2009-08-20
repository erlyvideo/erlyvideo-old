/* Minimal Player for RubyIZUMI */

package {
	
    import flash.display.Sprite;  
	import flash.net.NetConnection;
	import flash.events.NetStatusEvent;
	import flash.events.SecurityErrorEvent;
	import flash.events.AsyncErrorEvent;
	import flash.media.Video;
	import flash.net.NetStream;
	import flash.net.ObjectEncoding;
  import flash.external.ExternalInterface;
  import flash.text.TextField;  
	
    public class Player extends Sprite 
	{
		private var _connection : NetConnection;
		private var _stream : NetStream;
		private var _video : Video;
		private var _log : TextField;
		
    public function Player() 
		{
/*      ExternalInterface.call("alert", "Connecting");*/
      _log = new TextField();
      addChild(_log);
      _log.text = "Connecting";
			_connection = new NetConnection();
			_connection.addEventListener(NetStatusEvent.NET_STATUS, onNetStatus);
			_connection.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecurityError);
			_connection.objectEncoding = ObjectEncoding.AMF0;
			_connection.connect("rtmp://localhost/", 142, "password");
        }

		private function onNetStatus( event : NetStatusEvent ) : void
		{
			switch(event.info.code){
			case "NetConnection.Connect.Success":
/*        ExternalInterface.call("alert", "Connected");*/
        _log.text = "Connected";
				_stream = new NetStream(_connection);
				_stream.addEventListener(NetStatusEvent.NET_STATUS, onNetStatus);
				_stream.addEventListener(AsyncErrorEvent.ASYNC_ERROR, onAsyncError);

				var video : Video = new Video(320, 180);
				video.attachNetStream(_stream);
				video.deblocking = 2;
				video.smoothing = true;
				
        _stream.play("video.flv");
				addChild(video);
				video.y = 20;
				_video = video
				break;
				
			case "NetConnection.Message":
/*        ExternalInterface.call("console.log", event.info.description);*/
        _log.text = event.info.description;
        break;
			

			case "NetStream.Play.StreamNotFound":
				trace("File not found");
				break;
			}
		}

		private function onSecurityError( event : SecurityErrorEvent ) : void
		{
		}

		private function onAsyncError( event : AsyncErrorEvent ) : void
		{
		}
    }
}
