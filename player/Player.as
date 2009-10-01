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
import mx.events.SliderEvent;
import flash.media.Camera;
	
private var _connection : NetConnection;
private var _stream : NetStream;
private var _video : Video;
private var _connected : Boolean = false;
private var _playing : Boolean = false;
private var _pausing : Boolean = false;
private var _recording : Boolean = false;
private var _camera : Camera;
private var _statusTimer : Timer;
private var _totalTime : Number;

public function init()  : void
{
  connect();
  _statusTimer = new Timer(100); // 1 second
  _statusTimer.addEventListener(TimerEvent.TIMER, setProgressBar);
  pauseButton.enabled = false;
  playButton.enabled = false;
  recordButton.enabled = false;
  
	var video : Video = new Video(320, 240);
	video.deblocking = 2;
	video.smoothing = true;
	video_container.addChild(video);
	_video = video;
}

public function setProgressBar(event:TimerEvent) : void
{
  progressBar.value = _stream.time;
}

public function play() : void
{
  if (!_connected || !_stream) return;
  
  if (_playing) {
    stop();
    return;
  }
  
	
  _stream.play(player_url.text);
	playButton.label = "Stop";
	pauseButton.label = "Pause";
	pauseButton.enabled = true;
	_statusTimer.start();
	_playing = true;
	_pausing = true;
}

public function record() : void
{
  if (_recording) {
    _stream.publish(null);
    _recording = false;
    playButton.enabled = true;
    recordButton.label = "Record";
    _video.attachCamera(null);
    _video.clear();
    _stream.attachCamera(null);
    _recording = false;
  } else {
    if (!_camera) {
      _camera = Camera.getCamera();
    }
    if (_camera) {
      stop();
      playButton.enabled = false;
      _stream.publish("mp4:stream", "record");
      _video.attachCamera(_camera);
/*      _camera.setMode(380,240,25);*/
      _stream.attachCamera(_camera);
      recordButton.label = "Stop";
      _recording = true;
    }
  }
}

public function pause() : void
{
  if (!_stream || !_playing) return;
  
  if (_pausing) {
    _stream.pause();
  	_statusTimer.stop();
    pauseButton.label = "Resume";
  } else {
    pauseButton.label = "Pause";
    _statusTimer.start();
    _stream.resume();
  }
  _pausing = !_pausing;
}

public function seek(event:SliderEvent) : void
{
  progressBar.value = event.value;
  if (_stream) {
    _stream.seek(event.value);
  }
}

public function stop() : void
{
  playButton.label = "Play";
  pauseButton.label = "Pause";
  pauseButton.enabled = false;
  _playing = false;
  _pausing = true;
  if (_playing) {
    _stream.play(false);
  }
  _video.clear();
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
	_connection.connect("rtmp://"+Application.application.parameters.server+"/", "password", 142);
  
}

private function onMetaData(metadata : Object) : void
{
  _video.width = metadata.width;
  _video.height = metadata.height;
  _totalTime = metadata.duration;
  progressBar.maximum = metadata.duration;
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
  	var listener : Object = new Object();
  	_stream = new NetStream(_connection);
  	_stream.addEventListener(NetStatusEvent.NET_STATUS, onStreamStatus);
  	_stream.addEventListener(AsyncErrorEvent.ASYNC_ERROR, onAsyncError);
  /*    _stream.setBufferTime(20);*/
  	listener.onMetaData = onMetaData;
  	_stream.client = listener;
  	_video.attachNetStream(_stream);
  	
    playButton.enabled = true;
    recordButton.enabled = true;
    
    _connected = true;
		break;
		
	case "NetConnection.Message":
/*        ExternalInterface.call("console.log", event.info.description);*/
    _log.text = event.info.description;
    break;
    
  case "NetConnection.Connect.Failed":
    _stream = null;
    _playing = false;
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
