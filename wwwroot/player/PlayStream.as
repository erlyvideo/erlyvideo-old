package
{
  import VideoStream;
	import flash.utils.Timer;
  import flash.events.TimerEvent;
	import flash.media.SoundTransform;
	import flash.media.Video;
	
  public class PlayStream extends VideoStream
  {
		private var statusTimer : Timer;
		private var sound : SoundTransform;
    
    public function PlayStream(s:VideoSource):void
    {
      statusTimer = new Timer(100); // 1 second
		  statusTimer.addEventListener(TimerEvent.TIMER, progressTick);
		  sound = new SoundTransform();
    }

		override public function stop() : void
		{
		  if (_stream) {
		    _stream.play(false);
		  }
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

		public function set volume(vol : Number) : void
		{
			if (_stream) {
			  sound.volume = vol;
			  _stream.soundTransform = sound;
			}
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


		private function progressTick(event:TimerEvent) : void {
			if (_stream) {
				dispatchEvent(new VideoSourceEvent(VideoSourceEvent.TICK, _stream.time));
			} else {
				statusTimer.stop();
			}
		}
    
  }
}