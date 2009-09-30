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
	
private var _connection : NetConnection;
private var _stream : NetStream;
private var _video : Video;
private var _connected : Boolean = false;
private var _playing : Boolean = false;

public function init()  : void
{
/*      ExternalInterface.call("alert", "Connecting");*/
  connect();
}

public function play() : void
{
  if (!_connected) return;
  
  if (_stream) {
    PlayButton.label = "Play";
    stop();
    return;
  }
  
	var listener : Object = new Object();
	_stream = new NetStream(_connection);
	_stream.addEventListener(NetStatusEvent.NET_STATUS, onStreamStatus);
	_stream.addEventListener(AsyncErrorEvent.ASYNC_ERROR, onAsyncError);
/*    _stream.setBufferTime(20);*/
	listener.onMetaData = onMetaData;
	_stream.client = listener;

  if (!_video) {
  	var video : Video = new Video(320, 180);
  	video.deblocking = 2;
  	video.smoothing = true;
  	video_container.addChild(video);
  	_video = video;
  }
	
	_video.attachNetStream(_stream);
  _stream.play(player_url.text);
	PlayButton.label = "Stop";
	_playing = true;
}

public function pause() : void
{
  if (!_stream) return;
  
  if (_playing) {
    _stream.pause();
    PauseButton.label = "Resume";
  } else {
    PauseButton.label = "Pause";
    _stream.resume();
  }
  _playing = !_playing;
}

public function stop() : void
{
  
}
public function set_volume(volume : Object) : void
{
  
}

private function connect() : void
{
  _connected = false;
  _log.text = "Connecting";
	_connection = new NetConnection();
	_connection.addEventListener(NetStatusEvent.NET_STATUS, onConnectionStatus);
	_connection.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecurityError);
	_connection.objectEncoding = ObjectEncoding.AMF0;
	_connection.connect("rtmp://localhost/", "password", 142);
  
}

private function onMetaData(metadata : Object) : void
{
  _video.width = metadata.width;
  _video.height = metadata.height;
  _log.text = "Metadata "+(metadata.duration);
}

private function onStreamStatus( event : NetStatusEvent ) : void
{
	switch(event.info.code){
	case "NetStream.Metadata":
	  onMetaData(event.info.description);
	  break;
	default:
	  _log.text = "Stream: "+event.info.code;
  }
  
}

private function onConnectionStatus( event : NetStatusEvent ) : void
{
	switch(event.info.code){
	case "NetConnection.Connect.Success":
/*        ExternalInterface.call("alert", "Connected");*/
    _log.text = "Connected";
    _connected = true;
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
