import flash.display.Sprite;  
import flash.net.NetConnection;
import flash.events.NetStatusEvent;
import flash.events.SecurityErrorEvent;
import flash.events.AsyncErrorEvent;
import flash.media.Video;
import flash.external.ExternalInterface;
import flash.text.TextField;
import VideoSource;
import VideoStream;
import mx.controls.Alert;
import VideoSourceEvent;
import mx.events.SliderEvent;
	
	
[Bindable]
private var play_url :String = "railsconf";

[Bindable]
private var showPlayButton : Boolean = true;

[Bindable]
private var videoWidth:int = 720;

[Bindable]
private var videoHeight:int = 450;

[Bindable]
private var imageUrl:String = "ev-disabled.png";

[Bindable]
private var state:String = "connecting";

private var playStream : VideoStream;


private var replayTimeout : Number = 1000;

public function init()  : void
{
	Security.allowDomain("*");
  System.useCodePage = true;
	VideoSource.source.addEventListener(VideoSourceEvent.CONNECTED, onConnected);
  VideoSource.source.connect();


	
	if (Application.application.parameters.file) {
    play_url = Application.application.parameters.file;
	}

	ExternalInterface.addCallback("sendMessage", sendMessage);
	loadVideoPlayer();
}

public function sendMessage(msg:String):void
{
	VideoSource.source.sendMessage(msg);
}

public function onClick():void
{
	switch(state) {
		case "connecting": return;
		case "connected": 
			startPlay();
			return;
		case "play": 
		case "playing": 
			playStream.stop();
			videoContainer.clear();
			state = "connected";
			return;
	}
}

public function startPlay():void
{
	playStream.play(play_url, videoContainer.video);
}

public function loadVideoPlayer():void
{
	playStream = new VideoStream(VideoSource.source);
	playStream.addEventListener(VideoSourceEvent.METADATA, setMetadata);
	playStream.addEventListener(VideoSourceEvent.STREAM_READY, onReady);
	playStream.addEventListener(VideoSourceEvent.PLAY_START, onPlayStart);
	playStream.addEventListener(VideoSourceEvent.FILE_NOT_FOUND, onFinish);
	playStream.addEventListener(VideoSourceEvent.FINISHED, onFinish);
}

public function onConnected(e:Event):void {
	state = "connected";
	if (Application.application.parameters.autostart == "true") {
//		state = "playing";
		startPlay();
	}
}

public function onFinish(e:Event):void {
	state = "connected";
	if (Application.application.parameters.autostart == "true") {
//		state = "playing";
    var t:Timer = new Timer(replayTimeout, 1);
    t.addEventListener(TimerEvent.TIMER, function(e:Event):void {
      startPlay();
    });
    replayTimeout += 100;
    t.start();
	}
}

public function onPlayStart(e:Event):void {
  replayTimeout = 1000;
  state = "playing";
}

public function onReady(e:Event):void {
	
}

public function setMetadata(event:VideoSourceEvent):void {
	var metadata : Object = event.payload;
  videoWidth = metadata.width;
  videoHeight = metadata.height;
}

public function onPlay(e:Event):void {
}

