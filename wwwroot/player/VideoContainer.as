package 
{
	import mx.core.UIComponent;
	import flash.events.Event;
	import flash.media.Video;
	import mx.containers.Canvas;

	public class VideoContainer extends Canvas
	{
		private var _video:Video;
		private static var number : int = 0;
		public var my_id : int;

		public function VideoContainer()
		{
			super();
			my_id = ++number;
			_video = new Video(width, height);
	    _video.deblocking = 2;
	    _video.smoothing = true;
			_video.width = width;
			_video.height = height;
			rawChildren.addChild(_video);
			addEventListener(Event.RESIZE, resizeHandler);
		}
		
		public function get video() : Video
		{
			return _video;
		}
		
		private function resizeHandler(event:Event):void
		{
			if (_video != null)
			{
				_video.width = width;
				_video.height = height;
			}
		}

	}
}

