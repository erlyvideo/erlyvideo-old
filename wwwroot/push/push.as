package {
import flash.display.Sprite;  
import flash.net.NetConnection;
import flash.events.NetStatusEvent;
import flash.events.SecurityErrorEvent;
import flash.events.AsyncErrorEvent;
import flash.net.ObjectEncoding;
import flash.external.ExternalInterface;
import flash.utils.Timer;
import flash.events.TimerEvent;

public class push extends Sprite
{
	private var _connection : NetConnection;
	private var _connected : Boolean = false;
	private var _connectTimer : Timer = null;
	private var _server : String;
	private var _cookie : String;

	public function push()  : void
	{
		var delay : int = 2000;
		var repeat : int = 1;
		var parameters : Object = this.loaderInfo.parameters;
		_server = parameters.server + "/";
		_cookie = parameters.session;
		_connectTimer = new Timer(delay, repeat);
		_connectTimer.addEventListener(TimerEvent.TIMER, connect);
		
		ExternalInterface.call("netconnection.initialized", _server);
	  connect();
	}

	private function connect(a:Object = null) : void
	{
	  _connected = false;
		_connection = new NetConnection();
		_connection.addEventListener(NetStatusEvent.NET_STATUS, onConnectionStatus);
		_connection.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecurityError);
		_connection.objectEncoding = ObjectEncoding.AMF0;
		_connection.connect(_server+"/", _cookie, 142);
	}

	private function onConnectionStatus( event : NetStatusEvent ) : void
	{
		switch(event.info.code){
		case "NetConnection.Connect.Success":
			try {
		    ExternalInterface.call("netconnection.connected");
			}
			catch (e:Object) {
			}
	    _connected = true;
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
			try {
				ExternalInterface.call("netconnection.failed");
			}
			catch (e:Object) {
			}
			_connected = false;
			_connectTimer.start();
	    break;
	
		}
	}

	private function onSecurityError( event : SecurityErrorEvent ) : void
	{
	}
}
}