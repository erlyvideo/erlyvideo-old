package 
{
	import mx.core.UIComponent;
	import flash.events.Event;
	import flash.media.Video;
	import mx.containers.Canvas;

	public class VideoContainer extends Canvas
	{
		private var _video:Video;

		public function VideoContainer()
		{
			super();
			addEventListener(Event.RESIZE, resizeHandler);
			//setStyle("paddingLeft", "10");
			//setStyle("paddingTop", "10");
		}
		
		public function get video():Video
		{
			return _video;
		}

		public function set video(video:Video):void
		{
			if (_video != null)
			{
				rawChildren.removeChild(_video);
			}

			_video = video;

			if (_video != null)
			{
				_video.width = width;
				_video.height = height;
				rawChildren.addChild(_video);
			}
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

