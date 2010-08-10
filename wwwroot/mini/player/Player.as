package {
  import flash.events.Event;
  import flash.events.NetStatusEvent;
  import flash.events.TimerEvent;

	import flash.display.Sprite;
	import flash.display.StageAlign;
	import flash.display.StageScaleMode;
	
  import flash.media.Video;
  
  import flash.net.NetConnection;
  import flash.net.NetStream;
  
  import flash.external.ExternalInterface;
  import flash.utils.Timer;
  
	[SWF(width=800, height=500, backgroundColor=0xFFFFFF, frameRate=25)]

  public class Player extends Sprite {
    private var server:String;
    private var app:String;
    private var session:String;
    private var file:String;
    private var nc:NetConnection;
    private var stream:NetStream;
    
    private var connected:Boolean = false;
    private var connectTimer:Timer;
    
    private var video:Video;
    
    public function Player() {
			checkStage();
    }

		/**
		 * Check stage on exists and width > 0
		 */
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
		
		private function log(...args):void {
			var text:String;
			text = args.join(",") + "\n";
			if(ExternalInterface.available) {
  			ExternalInterface.call("erlyvideo.log", text);
			}
		}
		
		private function init():void {
			stage.scaleMode = StageScaleMode.NO_SCALE;
			stage.align = StageAlign.TOP_LEFT;
			stage.showDefaultContextMenu = false;

		  this.server = loaderInfo.parameters.server;
		  this.app = loaderInfo.parameters.app || "rtmp";
		  this.session = loaderInfo.parameters.session;
		  this.file = loaderInfo.parameters.file;
		  
		  ExternalInterface.addCallback("play", handlePlay);
		  ExternalInterface.addCallback("seek", handleSeek);
		  ExternalInterface.addCallback("stop", handleStop);
		  connectTimer = new Timer(3000, 0);
		  connectTimer.addEventListener(TimerEvent.TIMER_COMPLETE, connect);
      ExternalInterface.call("erlyvideo.init");
      
      video = new Video(stage.stageWidth, stage.stageHeight);
      addChild(video);
      connect();
		}
		
		private function connect(e:Event = null):void {
			if (!nc) {
				nc = new NetConnection();
				nc.addEventListener(NetStatusEvent.NET_STATUS, onNetStatus);
			}
			nc.connect(this.server + "/" + this.app, this.session);		// 142 is magic number
		}
		
		public function onMetaData(object:Object):void {
		  video.width = object.width;
		  video.height = object.height;
		}
		
		private function onNetStatus(e:NetStatusEvent):void {
		  switch(e.info.code) {
		    case "NetConnection.Connect.Success":
		      stream = new NetStream(nc);
		      stream.client = this;
    		  video.attachNetStream(stream);
		      this.connected = true;
		      ExternalInterface.call("erlyvideo.connected");
		      if(this.file) {
		        play();
		      }
		      break;
		    
		    case "NetConnection.Connect.Closed":
		    case "NetConnection.Connect.Failed":
		      connectTimer.repeatCount = 1;
		      connectTimer.start();
		      this.connected = false;
		      break;
		    
		    case "NetConnection.Connect.Rejected":
		  }
		  
		}
		
		private function play():void {
		  stream.play(this.file);
      ExternalInterface.call("erlyvideo.playing", this.file);
		}
		
		private function handlePlay(file:String):void {
		  if(this.file && this.stream) {
		    stream.close();
		    stream = new NetStream(nc);
		    video.attachNetStream(stream);
		    stream.play(null);
		  }
		  this.file = file;
		  play();
		}
		
		private function handleStop():void {
		  if(this.stream) {
		    stream.play(null);
		    stream.close();
		  }
		  this.stream = null;
		}
		
		private function handleSeek(offset:Number):void {
		  if(this.stream) {
		    stream.seek(offset);
		  }
		}
  }
}