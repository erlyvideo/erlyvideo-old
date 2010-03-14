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
import mx.utils.UIDUtil;
import VideoSourceEvent;
import mx.events.SliderEvent;
	
	
[Bindable]
private var play_url :String = "video.mp4";
private var recordStream : RecordStream;
private var playStream : PlayStream;

[Bindable]
private var bufferStat:String = "empty";

[Bindable]
private var playButton:Boolean = false;
[Bindable]
private var stopButton:Boolean = false;
[Bindable]
private var enablePauseButton:Boolean = false;
[Bindable]
private var enableRecordButton:Boolean = false;
//  [Bindable]
//  private var url:String = "video.mp4";
[Bindable]
private var videoWidth:int = 320;
[Bindable]
private var videoHeight:int = 240;
[Bindable]
private var videoFps:int = 15;
[Bindable]
private var duration : Number = 0;
[Bindable]
private var currentTime : Number = 0;
[Bindable]
private var quality : Number = 90;
[Bindable]
private var recordURL : String;
private var recording : Boolean = false;

public function init()  : void
{
	Security.allowDomain("*");
  System.useCodePage = true;
	VideoSource.source.addEventListener(VideoSourceEvent.CONNECTED, onReady);
  VideoSource.source.connect();
	
	if (Application.application.parameters.file) {
    play_url = Application.application.parameters.file;
	}
	recordURL = "erly-"+UIDUtil.createUID()+".flv";
}

public function check() : void
{
/*	Alert.show("z:"+(Application.application.parameters.player1.stream._stream == Application.application.parameters.player2.stream._stream));*/
/*  Application.application.parameters.player2.videoContainer.video.visible = false;*/
}

private function onRecordUrlClick():void
{
  play_url = recordURL;
}


private function initPlaystream():void
{
  if (!playStream) {
  	playStream = new PlayStream(VideoSource.source);
  	playStream.addEventListener(VideoSourceEvent.TICK, setProgressBar);
  	playStream.addEventListener(VideoSourceEvent.METADATA, setMetadata);
/*    playStream.addEventListener(VideoSourceEvent.STREAM_READY, onReady);*/
  	playStream.addEventListener(VideoSourceEvent.FILE_NOT_FOUND, onStop);
  	playStream.addEventListener(VideoSourceEvent.FINISHED, onFinish);
  }
}

private function initRecordstream():void
{
  if (!recordStream) {
  	recordStream = new RecordStream(VideoSource.source);
  }
}

public function onHideClicked(e:Event):void {
  visible = false;
}

public function onReady(e:Event):void {
	playButton = true;
	enableRecordButton = true;
}

public function onPlay(e:Event):void {
  initPlaystream();
	playButton = false;
//		Alert.show("z:"+(stream._stream == Application.application.parameters.player1.stream._stream)+","+
//		                (stream._stream == Application.application.parameters.player2.stream._stream) + " "+
//		                videoContainer.my_id);
  playStream.bufferTime = Application.application.parameters.buffer ? int(Application.application.parameters.buffer) : 10;
	if (playStream.play(player_url.text, videoContainer.video)) {
		playButton = false;
		enablePauseButton = true;
		stopButton = true;
	} else {
		playButton = true;
	}
}

public function onStop(e:Event):void {
	playStream.stop();
	duration = 0;
	playButton = true;
	enablePauseButton = false;
	stopButton = false;
	
}

public function onPause(e:Event):void {
  if (playStream.paused) {
    playStream.resume();
    pauseButton.label = "Pause";
  } else {
    if (playStream.pause()) {
      pauseButton.label = "Resume";
    }
  }

}

public function setWidth(width: Number) : void
{
  videoWidth = width;
  recordStream.width = width;
}

public function setHeight(height: Number) : void
{
  videoHeight = height;
  recordStream.height = height;
}

public function setFps(fps: Number) : void
{
  videoFps = fps;
  recordStream.fps = fps;
}

public function setVolume(volume : Number) : void
{
	recordStream.volume = volume;
	playStream.volume = volume;
}

public function setMetadata(event : VideoSourceEvent) : void
{
	var metadata : Object = event.payload;
  videoWidth = metadata.width;
  videoHeight = metadata.height;
  duration = metadata.duration;
}

public function startSeek(event:SliderEvent) : void
{
	playStream.startSeek();
}

public function setQuality(quality : int) : void
{
  recordStream.quality = quality;
}

public function setProgressBar(event:VideoSourceEvent) : void
{
	currentTime = int(event.payload);
	bufferStat = playStream.bufferLength + "/" + playStream.bufferTime;
}

public function seek(event:SliderEvent) : void
{
  if (duration > 0) {
	  playStream.seek(event.value);
    //progressBar.value = event.value;
  }
}

public function onFinish(e:Event) : void
{
  var t:Timer = new Timer(playStream.bufferLength*1000, 1);
  t.addEventListener(TimerEvent.TIMER, function():void {
    if (stopButton) {
    	playStream.play(player_url.text, videoContainer.video);
    }
  });
  t.start();
}

public function onRecord(e:Event, recordType:String) : void
{
  if (recording) {
    videoContainer.video.attachCamera(null);
    videoContainer.video.clear();
    recordButton.label = "Record";
    recordStream.stop();
    recording = false;
  } else {
    initRecordstream();
    recordStream.width = videoWidth;
    recordStream.height = videoHeight;
    recordStream.fps = videoFps;
  	if(recordStream.record(recordURL, videoContainer.video, recordType)) {
  	  recordButton.label = "Stop";
      recording = true;
  	}
  }
}
