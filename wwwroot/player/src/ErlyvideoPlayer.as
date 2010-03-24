package {
	
	import com.bit101.components.HBox;
	import com.bit101.components.HSlider;
	import com.bit101.components.Knob;
	import com.bit101.components.Label;
	import com.bit101.components.PushButton;
	import com.bit101.components.Text;
	import com.bit101.components.TextArea;
	
	import flash.display.Sprite;
	import flash.display.StageAlign;
	import flash.display.StageScaleMode;
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.utils.setTimeout;
	
	import org.osmf.containers.MediaContainer;
	import org.osmf.elements.VideoElement;
	import org.osmf.events.DisplayObjectEvent;
	import org.osmf.events.MediaPlayerCapabilityChangeEvent;
	import org.osmf.events.MediaPlayerStateChangeEvent;
	import org.osmf.events.PlayEvent;
	import org.osmf.events.SeekEvent;
	import org.osmf.events.TimeEvent;
	import org.osmf.layout.HorizontalAlign;
	import org.osmf.layout.LayoutMetadata;
	import org.osmf.layout.ScaleMode;
	import org.osmf.layout.VerticalAlign;
	import org.osmf.media.MediaPlayer;
	import org.osmf.media.MediaPlayerState;
	import org.osmf.media.URLResource;
	
	[SWF(width=800, height=455, backgroundColor=0x0, frameRate=25)]
	
	public class ErlyvideoPlayer extends Sprite {
		
		private const VIDEO_URL_FMS:String = "rtmp://cp67126.edgefcs.net/ondemand/mp4:mediapm/osmf/content/test/sample1_700kbps.f4v";
		private const VIDEO_URL_ERL:String = "rtmp://erlyvideo.org/rtmp/video.mp4";
		private const VIDEO_URL_TS:String = "rtmp://erlyvideo.org/rtmp/video.ts";
//			for testing
//			http://mediapm.edgesuite.net/strobe/content/test/AFaerysTale_sylviaApostol_640_500_short.flv
//			rtmp://cp67126.edgefcs.net/ondemand/mediapm/strobe/content/test/SpaceAloneHD_sounas_640_500_short
//			gta1.mp4
		
		private var vars:Object;
		
		private var c:MediaContainer;
		private var p:MediaPlayer;
		private var v:VideoElement;
		private var l:LayoutMetadata;
		private var r:URLResource;
		
		private var hbox:HBox;
		private var stop:PushButton;
		private var play:PushButton;
		private var pause:PushButton;
		private var bar:HSlider;
		private var volume:Knob;
		private var urlInput:Text;
		private var logger:TextArea;
		
		public function ErlyvideoPlayer() {
			visible = false;
			checkStage();
		}
		
		// CHECK STAGE
		private function checkStage():void {
			if (stage) {
				if (stage.stageWidth > 0) {
					init();
					return;
				}
				addEventListener(Event.ENTER_FRAME, onCheckStage);
			} else {
				addEventListener(Event.ADDED_TO_STAGE, onAddedToStage);
			}
		}
		private function onAddedToStage(e:Event):void {
			removeEventListener(Event.ADDED_TO_STAGE, onAddedToStage);
			checkStage();
		}
		private function onCheckStage():void {
			removeEventListener(Event.ENTER_FRAME, onCheckStage);
			checkStage();
		}
		
		// INIT
		private function init():void {
			stage.scaleMode = StageScaleMode.NO_SCALE;
			stage.align = StageAlign.TOP_LEFT;
			stage.showDefaultContextMenu = false;
			
			vars = loaderInfo.parameters;
			
			createPlayer();
			createControls();
			
			stage.addEventListener(Event.RESIZE, onResize);
		}
		
		// ON RESIZE STAGE
		private function onResize(e:Event = null):void {
			var w:int = stage.stageWidth;
			var h:int = stage.stageHeight;
			
			l.width = w;
			l.height = h;
			
			urlInput.width = w - 320;
			
			hbox.y = h - stop.height - 10;
			bar.width = w - stop.width - play.width - pause.width - volume.width - (hbox.numChildren-1)*hbox.spacing - 2*hbox.x;
			volume.y = stop.height - volume.height + 10;
			
			if (!visible) visible = true;
		}
		
		/**
		 * CREATE PLAYER
		 */
		private function createPlayer():void {
			l = new LayoutMetadata();
			l.scaleMode = ScaleMode.LETTERBOX;
			l.snapToPixel = true;
			l.horizontalAlign = HorizontalAlign.CENTER;
			l.verticalAlign = VerticalAlign.MIDDLE;
			
			p = new MediaPlayer();
			p.addEventListener(MediaPlayerStateChangeEvent.MEDIA_PLAYER_STATE_CHANGE, onStateChange);
			p.addEventListener(MediaPlayerCapabilityChangeEvent.CAN_PLAY_CHANGE, onCanPlayChange);
			p.addEventListener(SeekEvent.SEEKING_CHANGE, onSeekChange);
			p.addEventListener(DisplayObjectEvent.MEDIA_SIZE_CHANGE, onMediaSizeChange);
			p.addEventListener(TimeEvent.DURATION_CHANGE, onDurationChange);
			p.addEventListener(TimeEvent.CURRENT_TIME_CHANGE, onCurrentTimeChange);
			p.addEventListener(TimeEvent.COMPLETE, onTimeComplete);
			p.volume = .3;
			p.autoPlay = false;
			
			c = new MediaContainer(null, l);
			addChild(c);
		}
		private function onStateChange(e:MediaPlayerStateChangeEvent):void {
			addLog("state\t\t", e.state)
			switch (e.state) {
				case MediaPlayerState.READY:
					stop.enabled = false;
				case MediaPlayerState.PAUSED:
					play.enabled = true;
					pause.enabled = false;
					break;
				case MediaPlayerState.PLAYING:
					play.enabled = false;
					pause.enabled = true;
					stop.enabled = true;
					break;
//				case MediaPlayerState.BUFFERING:
//				case MediaPlayerState.LOADING:
//				case MediaPlayerState.PLAYBACK_ERROR:
//				case MediaPlayerState.UNINITIALIZED:
//					break;
			}
		}
		private function onCanPlayChange(e:MediaPlayerCapabilityChangeEvent):void {
			hbox.enabled = e.enabled;
		}
		private function onSeekChange(e:SeekEvent):void {
			if (e.seeking) addLog("seeking to\t", e.time);
		}
		private function onMediaSizeChange(e:DisplayObjectEvent):void {
			if (e.newWidth > 0) addLog("video_size\t", e.newWidth, e.newHeight);
		}
		private function onDurationChange(e:TimeEvent):void {
			addLog("duration\t", e.time);
		}
		private function onCurrentTimeChange(e:TimeEvent):void {
			bar.value = p.currentTime / p.duration;
		}
		private function onTimeComplete(e:TimeEvent):void {
			addLog("complete");
		}
		
		/**
		 * CREATE CONTROLS
		 */
		private function createControls():void {
			// top
			var hb:HBox = new HBox(null, 10, 10);
			var bfms:PushButton = new PushButton(hb, 0, 0, "FMS", onFMSVideo);
			var berl:PushButton = new PushButton(hb, 0, 0, "VIDEO.MP4", onERLVideo);
			var bts:PushButton = new PushButton(hb, 0, 0, "MPEG-TS", onTSVideo);
			bfms.width = berl.width = bts.width = 60;
			
			// custom url
			urlInput = new Text(hb);
			urlInput.height = bfms.height;
			urlInput.textField.multiline = false;
			urlInput.textField.wordWrap = false;
			urlInput.editable = true;
			if (vars.url) {
				urlInput.text = vars.url;
				setTimeout(connect, 100);
			}
			var connectBut:PushButton = new PushButton(hb, 0, 0, "CONNECT", onConnect);
			
			// logger
			logger = new TextArea(null, hb.x, hb.y + bfms.height + 10);
			logger.editable = false;
			
			// bottom
			hbox = new HBox();
			hbox.x = 10;
			hbox.enabled = false;
			
			stop = new PushButton(hbox, 0, 0, "STOP", onStop);
			stop.enabled = false;
			
			play = new PushButton(hbox, 0, 0, "PLAY", onPlay);
			pause = new PushButton(hbox, 0, 0, "PAUSE", onPause);
			stop.width = play.width = pause.width = 50;
			
			bar = new HSlider(hbox, 0, 0);
			bar.minimum = 0;
			bar.maximum = 1;
			bar.tick = .01;
			bar.height = stop.height;
			bar.addEventListener(MouseEvent.MOUSE_DOWN, onBarDown);
			
			volume = new Knob(hbox, 0, 0, "volume", onVolumeChange);
			volume.minimum = 0;
			volume.maximum = 100;
			volume.labelPrecision = 0;
			volume.value = p.volume * volume.maximum;
			
			addChild(hb);
			addChild(logger);
			addChild(hbox);
			
			setTimeout(onResize, 100);
		}
		private function onFMSVideo(e:MouseEvent):void {
			changeURL(VIDEO_URL_FMS);
		}
		private function onERLVideo(e:MouseEvent):void {
			changeURL(VIDEO_URL_ERL);
		}
		private function onTSVideo(e:MouseEvent):void {
			changeURL(VIDEO_URL_TS);
		}
		private function changeURL(url:String):void {
			urlInput.text = url;
			connect();
		}
		private function onConnect(e:MouseEvent):void {
			connect();
		}
		private function connect():void {
			if (urlInput.text.length == 0) return;
			if (v) c.removeMediaElement(v);
			r = new URLResource(urlInput.text);
			v = new VideoElement(r);
			v.smoothing = true;
			p.media = v;
			c.addMediaElement(v);
		}
		private function onStop(e:MouseEvent):void {
			p.stop();
			stop.enabled = false;
		}
		private function onPlay(e:MouseEvent):void {
			p.play();
		}
		private function onPause(e:MouseEvent):void {
			p.pause();
		}
		private function onBarDown(e:MouseEvent):void {
			p.removeEventListener(TimeEvent.CURRENT_TIME_CHANGE, onCurrentTimeChange);
			stage.addEventListener(MouseEvent.MOUSE_UP, onBarUp);
		}
		private function onBarUp(e:MouseEvent):void {
			p.addEventListener(TimeEvent.CURRENT_TIME_CHANGE, onCurrentTimeChange);
			stage.removeEventListener(MouseEvent.MOUSE_UP, onBarUp);
			p.seek(p.duration * bar.value);
		}
		private function onVolumeChange(e:Event):void {
			p.volume = volume.value / volume.maximum;
		}
		
		private function addLog(...args):void {
			logger.text += args.join(" ") + "\n";
			setTimeout(function():void {logger.textField.scrollV = logger.textField.maxScrollV}, 100);
		}
		
	}
}