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

public function init()  : void
{
	Security.allowDomain("*");
  System.useCodePage = true;
  VideoSource.source.roomName = "videoChat";
  VideoSource.source.connect();
	
	if (Application.application.parameters.file) {
    play_url = Application.application.parameters.file;
	}
}


public function onHideClicked(e:Event):void {
  visible = false;
}

