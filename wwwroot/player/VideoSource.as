package
{
	import flash.net.NetConnection;
	import flash.net.ObjectEncoding;
	import flash.events.NetStatusEvent;
	import flash.events.SecurityErrorEvent;
	import flash.events.AsyncErrorEvent;
	import flash.events.EventDispatcher;
	import flash.utils.Timer;
	import flash.events.TimerEvent;
	import flash.events.IOErrorEvent;
  import flash.events.Event;
	import VideoSourceEvent;
	import mx.core.Application;
	import flash.net.SharedObject;
	import flash.external.ExternalInterface;
  
	public class VideoSource extends EventDispatcher
	{
		private static var instance:VideoSource;
		private var _connectTimer : Timer = null;
		public var delay : int = 10000;
		public var connection:NetConnection;
		public var connected : Boolean = false;
    private var room:SharedObject;

		
		public static function get source():VideoSource
		{
			if (!instance) {
				instance = new VideoSource();
			}
			return instance;
		}
		
		public function VideoSource():void
		{
			_connectTimer = new Timer(delay, 1);
			_connectTimer.addEventListener(TimerEvent.TIMER, connect);
		}
		
		public function connect(e:Event = null) : void
		{
			connection = new NetConnection();
			connection.addEventListener(NetStatusEvent.NET_STATUS, onConnectionStatus);
			connection.addEventListener(IOErrorEvent.IO_ERROR, onError);
			connection.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecurityError);
			connection.addEventListener(AsyncErrorEvent.ASYNC_ERROR, onError);
			connection.objectEncoding = ObjectEncoding.AMF3;
			connection.connect(Application.application.parameters.server+"/test", Application.application.parameters.session, 142);
			room = SharedObject.getRemote("testRoom", connection.uri, false);
			room.connect(connection);
		}

		private function onConnectionStatus( event : NetStatusEvent ) : void
		{
			switch(event.info.code){
				case "NetConnection.Connect.Success":
					connected = true;
					dispatchEvent(new VideoSourceEvent(VideoSourceEvent.CONNECTED));
				break;

			case "NetConnection.Message":
  			try {
  				var re:RegExp = /\\/g;
  		    ExternalInterface.call("netconnection.message", event.info.description.replace(re, "\\\\"));
  			}
  			catch (e:Object) {
  			}
		    break;

		  case "NetConnection.Connect.Failed":
				connected = false;
				dispatchEvent(new VideoSourceEvent(VideoSourceEvent.DISCONNECT));
				_connectTimer.start();
		    break;

			}
		}

		private function onSecurityError( event : SecurityErrorEvent ) : void
		{
		}

		private function onError( event : Object ) : void
		{
			_connectTimer.start();
		}

	}
}