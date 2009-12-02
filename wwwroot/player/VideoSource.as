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
	
	public class VideoSource extends EventDispatcher
	{
		private static var instance:VideoSource;
		private var _connectTimer : Timer = null;
		public var delay : int = 2000;
		public var connection:NetConnection;
		
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
			connection.connect(Application.application.parameters.server+"/", Application.application.parameters.session, 142);
		}


		private function onConnectionStatus( event : NetStatusEvent ) : void
		{
			switch(event.info.code){
				case "NetConnection.Connect.Success":
					dispatchEvent(new VideoSourceEvent(VideoSourceEvent.CONNECTED));
				break;

			case "NetConnection.Message":
		    break;

		  case "NetConnection.Connect.Failed":
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