package 
{
	import mx.core.UIComponent;
	import flash.events.Event;
	import flash.media.Video;
	import mx.containers.Canvas;
  import mx.controls.Alert;

	public class VideoContainer extends Canvas
	{
		private var _video:Video;
		private static var number : int = 0;
		public var my_id : int;

		public function VideoContainer()
		{
			super();
			my_id = ++number;
			addEventListener(Event.RESIZE, resizeHandler);
		}
		
		public function clear():void
		{
			if (_video) {
				rawChildren.removeChild(_video);
				_video = null;
			}
		}
		
		public function get video() : Video
		{
			if (!_video) {
				_video = new Video(width, height);
		    _video.deblocking = 2;
		    _video.smoothing = true;
				_video.width = width;
				_video.height = height;
				rawChildren.addChild(_video);
				setStyle("paddingTop", 0);
				setStyle("paddingRight", 0);
				setStyle("paddingBottom", 0);
				setStyle("paddingLeft", 0);
        setStyle("drop-shadow-enabled", 0);
        setStyle("border-style", "none");
			}
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

