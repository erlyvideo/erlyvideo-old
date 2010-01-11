package
{
	import VideoSource;
	import VideoSourceEvent;
	import flash.events.NetStatusEvent;
	import flash.events.AsyncErrorEvent;
	import flash.media.SoundTransform;
	import flash.events.EventDispatcher;
	import flash.net.NetStream;
	import flash.utils.Timer;
	import flash.events.TimerEvent;
	import flash.media.Video;
	import flash.media.Camera;
	import flash.media.Microphone;
	import flash.media.SoundCodec;
	
	import mx.controls.Alert;
	public class VideoStream extends EventDispatcher
	{
		private var _source:VideoSource;
		public var _stream : NetStream;
		private var sound : SoundTransform;
		public var width : int = 320;
		public var height : int = 240;
		public var fps : int = 15;
		public var totalTime : int;
		private var statusTimer : Timer;
		
		public var recording : Boolean = false;
		public var paused : Boolean = false;
		
		private static var number : int = 0;
		public var id : int;
		
		public var recordQuality : int = 90;
		private var _camera : Camera;
		
		public function VideoStream(s:VideoSource):void
		{
			_source = s;
			_source.addEventListener(VideoSourceEvent.CONNECTED, onConnect);
			_source.addEventListener(VideoSourceEvent.DISCONNECT, onDisconnect);
		  statusTimer = new Timer(100); // 1 second
		  statusTimer.addEventListener(TimerEvent.TIMER, progressTick);
		  sound = new SoundTransform();
			id = ++number;
			if (_source.connected) {
				onConnect(null);
			}
		}
		
		public function onConnect(event : VideoSourceEvent) : void
		{
	  	var listener : Object = new Object();
	  	_stream = new NetStream(_source.connection);
	  	_stream.addEventListener(NetStatusEvent.NET_STATUS, onStreamStatus);
	  	_stream.addEventListener(AsyncErrorEvent.ASYNC_ERROR, onDisconnect);
	    _stream.bufferTime = 1;
	  	listener.onMetaData = onMetaData;
	  	_stream.client = listener;

			dispatchEvent(new VideoSourceEvent(VideoSourceEvent.STREAM_READY));
		}
		
		public function onDisconnect(event : Object) : void
		{
			stop();
			close();
			_stream = null;
			dispatchEvent(new VideoSourceEvent(VideoSourceEvent.STREAM_FAILED));
		}
		
		public function close() : void
		{
		  if (_stream) {
		    _stream.close();
		  }
		}
		
		public function get time():int
		{
			if (_stream) return _stream.time;
			else return -1;
		}
		
		public function get quality():int
		{
		  return recordQuality;
		}
		
		public function set quality(q:int):void
		{
		  if (!_camera) {
		    _camera = Camera.getCamera();
		  }
		  _camera.setQuality(0, q);
		  recordQuality = q;
		}
		
		public function stop() : void
		{
		  if (_stream) {
		    _stream.play(false);
				if (recording) {
					_stream.attachCamera(null);
				}
		  }
	    recording = false;
			paused = false;
			statusTimer.stop();
		}
		
		public function play(url : String, video : Video) : Boolean {
			if (!_stream) {
				return false;
			}
/*			Alert.show("Play "+id+","+_stream+" "+video );*/
			video.attachNetStream(_stream);
			statusTimer.start();
			_stream.play(url);
			return true
		}
		
		public function pause() : Boolean
		{
			if (_stream) {
				_stream.pause();
				paused = true;
				statusTimer.stop();
				return true;
			} else {
				statusTimer.stop();
				return false;
			}
		}

		public function resume() : void
		{
			if (_stream) {
				_stream.resume();
				paused = false;
				statusTimer.start();
			}
		}
		
		public function startSeek() : void
		{
			statusTimer.stop();
		}
		
		public function seek(time:int):void
		{
			if (_stream && Math.abs(_stream.time - time) >= 1) {
		    statusTimer.stop();
		    _stream.seek(time);
		  }
		}
		
		public function record(url : String, playback : Video) : Boolean
		{
			if (!_stream) {
				return false;
			}
			
		  if (!_camera) {
		    _camera = Camera.getCamera();
		  }
		  _camera.setMode(width, height, fps, false);
      _camera.setQuality(0, recordQuality);

		  var microphone : Microphone = Microphone.getMicrophone();
		  microphone.codec = SoundCodec.SPEEX;
		  microphone.encodeQuality = 10;
		  microphone.rate = 44;
		  microphone.framesPerPacket = 2;
		  microphone.gain = 80;
		  microphone.setUseEchoSuppression(true);

		  _stream.publish(url, "live");
		
			if (playback) {
				playback.attachCamera(_camera);
			}
			
			_stream.attachCamera(_camera);
			_stream.attachAudio(microphone);
			
			recording = true;
			return true;
		}
		
		private function progressTick(event:TimerEvent) : void {
			if (_stream) {
				dispatchEvent(new VideoSourceEvent(VideoSourceEvent.TICK, _stream.time));
			} else {
				statusTimer.stop();
			}
		}
		
		public function set volume(vol : Number) : void
		{
			if (_stream) {
			  sound.volume = vol;
			  _stream.soundTransform = sound;
			}
		}
		
		private function onStreamStatus( event : NetStatusEvent ) : void
		{
			switch(event.info.code){
		  	case "NetStream.Play.StreamNotFound":
					dispatchEvent(new VideoSourceEvent(VideoSourceEvent.FILE_NOT_FOUND));
		  		break;

		  	case "NetStream.Seek.Notify":
					statusTimer.start();
		      break;

		  	case "NetStream.Play.Complete":
				  dispatchEvent(new VideoSourceEvent(VideoSourceEvent.FINISHED));
		  	  break;

			default:
		  }

		}
		
		
		private function onMetaData(metadata : Object) : void
		{
		  width = metadata.width;
		  height = metadata.height;
		  totalTime = metadata.duration;
			dispatchEvent(new VideoSourceEvent(VideoSourceEvent.METADATA, metadata));
		}

		
	}
}