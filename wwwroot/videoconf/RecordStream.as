package
{
  import VideoStream;
	import flash.media.Video;
	import flash.media.SoundCodec;
	import flash.media.Microphone;
	import flash.media.Camera;
  
  public class RecordStream extends VideoStream
  {
    private var _volume : int = 80;
		private var _camera : Camera;
		private var _microphone : Microphone;
		public var fps : int = 15;
		public var recording : Boolean = false;
		public var recordQuality : int = 90;
		
		public function RecordStream(s:VideoSource = null):void
		{
/*			super.VideoStream(s);*/
		}
		
		override public function stop() : void
		{
		  if (_stream) {
		    _stream.play(false);
				_stream.attachCamera(null);
		  }
		}
		
		public function get volume() : int
		{
		  return _volume;
		}
		
		public function set volume(v:int):void
		{
		  _volume = v;
		  if (_microphone) {
		    _microphone.gain = v;
		  }
		}
		
		public function get quality():int
		{
		  return recordQuality;
		}
		
		public function set quality(q:int):void
		{
		  if (_camera) {
  		  _camera.setQuality(0, q);
		  }
		  recordQuality = q;
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

      if (!_microphone) {
        _microphone = Microphone.getMicrophone();
      }
		  
		  _microphone.codec = SoundCodec.SPEEX;
		  _microphone.encodeQuality = 10;
		  _microphone.rate = 44;
		  _microphone.framesPerPacket = 2;
		  _microphone.gain = volume;
		  _microphone.setUseEchoSuppression(true);

		  _stream.publish(url, "live");
		
			if (playback) {
				playback.attachCamera(_camera);
			}
			
			_stream.attachCamera(_camera);
			_stream.attachAudio(_microphone);
			
			recording = true;
			return true;
		}
		
  }
  
}
