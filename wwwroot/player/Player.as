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
	
public function init()  : void
{
  System.useCodePage = true;
  VideoSource.source.connect();
	
	if (Application.application.parameters.file) {
  	player1.url = Application.application.parameters.file;
	}
	player2.url = "video.flv";
	
//	Alert.show("z:"+(player1.stream == player2.stream));
}



