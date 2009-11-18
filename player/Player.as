import flash.display.Sprite;  
import flash.net.NetConnection;
import flash.events.NetStatusEvent;
import flash.events.SecurityErrorEvent;
import flash.events.AsyncErrorEvent;
import flash.media.Video;
import flash.media.SoundTransform;
import flash.net.NetStream;
import flash.net.ObjectEncoding;
import flash.external.ExternalInterface;
import flash.text.TextField;
import mx.events.SliderEvent;
import flash.media.Camera;
import flash.media.Microphone;
	
private var _connection : NetConnection;
private var _stream : NetStream;
private var _video : Video;
private var _sound : SoundTransform;
private var _connected : Boolean = false;
private var _playing : Boolean = false;
private var _pausing : Boolean = false;
private var _recording : Boolean = false;
private var _camera : Camera;
private var _microphone : Microphone;
private var _statusTimer : Timer;
private var _totalTime : Number;

public function init()  : void
{
  connect();
  _totalTime = 0;
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
	
	_sound = new SoundTransform();
	
	if (Application.application.parameters.file) {
  	player_url.text = Application.application.parameters.file;
	}
}

public function setProgressBar(event:TimerEvent) : void
{
  if (_totalTime) {
    progressBar.value = _stream.time;
  }
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
    _stream.play(false);
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
      _camera.setMode(320,240,20, false);
      _camera.setQuality(0, 90);
    }
    if (!_microphone) {
      _microphone = Microphone.getMicrophone();
      _microphone.setUseEchoSuppression(true);
    }
    if (_camera) {
      stop();
      playButton.enabled = false;
      _stream.publish(player_url.text, "record");
      _video.attachCamera(_camera);
      _stream.attachCamera(_camera);
      _stream.attachAudio(_microphone);
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

public function startSeek(event:SliderEvent) : void
{
  _statusTimer.stop();
}

public function seek(event:SliderEvent) : void
{
  var thumb:Object = progressBar.getThumbAt(0)
  progressBar.value = event.value;
  if (_stream && Math.abs(_stream.time - event.value) >= 1) {
    _statusTimer.stop();
    _stream.seek(event.value);
  }
}

public function stop() : void
{
  playButton.label = "Play";
  pauseButton.label = "Pause";
  pauseButton.enabled = false;
  if (_playing) {
    _stream.play(false);
  }
  _playing = false;
  _pausing = true;
  _video.clear();
}
public function setVolume(volume : Number) : void
{
  _sound.volume = volume;
  _stream.soundTransform = _sound;
}

private function connect() : void
{
  _connected = false;
  _log.text = "Connecting";
	_connection = new NetConnection();
	_connection.addEventListener(NetStatusEvent.NET_STATUS, onConnectionStatus);
	_connection.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecurityError);
	_connection.objectEncoding = ObjectEncoding.AMF0;
	_connection.connect(Application.application.parameters.server+"/", Application.application.parameters.session, 142);
  
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
  	case "NetStream.Play.StreamNotFound":
  		trace("File not found");
  		stop();
  		break;
  		
  	case "NetStream.Seek.Notify":
      _statusTimer.start();
      break;
  		
  	case "NetStream.Play.Complete":
  	  stop();
  	  play();
/*      stop();*/
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
    _stream.bufferTime = 1;
  	listener.onMetaData = onMetaData;
  	_stream.client = listener;
  	_video.attachNetStream(_stream);

    _sound.volume = volSlider.value;
  	_stream.soundTransform = _sound;
  	
    playButton.enabled = true;
    recordButton.enabled = true;
    
    _connected = true;
    
    if (Application.application.parameters.autostart) {
      play();
    }
		break;
		
	case "NetConnection.Message":
    ExternalInterface.call("netconnection.message", event.info.description);
/*    _log.text = event.info.description;*/
    break;
    
  case "NetConnection.Connect.Failed":
    _stream = null;
    _playing = false;
    break;
	
	}
}

private function onSecurityError( event : SecurityErrorEvent ) : void
{
}

private function onAsyncError( event : AsyncErrorEvent ) : void
{
}
