import flash.display.Sprite;  
import flash.net.NetConnection;
import flash.events.NetStatusEvent;
import flash.events.SecurityErrorEvent;
import flash.events.AsyncErrorEvent;
import flash.media.Video;
import flash.external.ExternalInterface;
import flash.text.TextField;
import mx.controls.Alert;
import mx.utils.UIDUtil;
import mx.events.SliderEvent;
	
	
[Bindable]
private var recordStream : NetStream;
private var playStream : NetStream;

[Bindable]
private var playButton:Boolean = false;
[Bindable]
private var offButton:Boolean = false;

public var connection:NetConnection;
private var _connectTimer : Timer = null;
private var _microphone : Microphone;


public var delay : Number = 2000;

public function init()  : void
{
	Security.allowDomain("*");
  System.useCodePage = true;
/*  recordURL = UIDUtil.createUID();*/
  connect();
  _connectTimer = new Timer(delay, 1);
	_connectTimer.addEventListener(TimerEvent.TIMER, connect);
	
}

private function shutdown() : void
{
  onShutdown();
  recordStream.close();
  recordStream = new NetStream(connection);
}

private function onShutdown() : void
{
  _log.text = "Разговор прекращён";
  recordStream.play(false);
  playButton = true;
  offButton = false;
}


private function connect(a:Object = null) : void
{
  _log.text = "Соединяюсь с "+Application.application.parameters.server;
  connection = new NetConnection();
  connection.addEventListener(NetStatusEvent.NET_STATUS, onConnectionStatus);
  connection.addEventListener(IOErrorEvent.IO_ERROR, onError);
  connection.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecurityError);
  connection.addEventListener(AsyncErrorEvent.ASYNC_ERROR, onError);
  connection.objectEncoding = ObjectEncoding.AMF3;
  connection.connect(Application.application.parameters.server);
}

private function onConnectionStatus( event : NetStatusEvent ) : void
{
	switch(event.info.code){
		case "NetConnection.Connect.Success":
			onConnect();
		break;

	case "NetConnection.Message":
		try {
			var re:RegExp = /\\/g;
	    ExternalInterface.call("netconnection.message", event.info.description.replace(re, "\\\\"));
		}
		catch (e:Object) {
		}
    break;

  case "NetConnection.Connect.Closed":
  case "NetConnection.Connect.Failed":
    onDisconnect();
    break;

  default:
    _log.text = event.info.code + ": "+event.info.description;
	}
}

protected function onPlayStatus( event : NetStatusEvent ) : void
{
	switch(event.info.code){
  	case "NetStream.Play.StreamNotFound":
  		break;

  	case "NetStream.Seek.Notify":
      break;

  	case "NetStream.Play.Complete":
  	  onShutdown();
  	  break;

	default:
  }

}


private function onSecurityError( event : SecurityErrorEvent ) : void
{
}

private function onError( event : Object ) : void
{
  onDisconnect();
}

private function onDisconnect( a: Object = null):void
{
  _log.text = "Потеря связи";
	_connectTimer.start();
	playButton = false;
}

private function onConnect(a:Object = null):void
{
  _log.text = "Есть соединение";
  playStream = new NetStream(connection);
  recordStream = new NetStream(connection);
  playButton = true;
}



public function call() : void
{
  _log.text = "Дозваниваюсь";
  playButton = false;
  if (!_microphone) {
    _microphone = Microphone.getMicrophone();
  }
  
  _microphone.codec = SoundCodec.SPEEX;
  _microphone.encodeQuality = 10;
  _microphone.rate = 8;
  _microphone.framesPerPacket = 1;
  _microphone.gain = 80;
  _microphone.setUseEchoSuppression(true);

  recordStream.attachAudio(_microphone);
  recordStream.publish(Application.application.parameters.file || "railsconf", "");
  
  _log.text = "Разговор с оператором";
}

