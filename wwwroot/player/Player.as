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
	
public function init()  : void
{
  System.useCodePage = true;
  VideoSource.source.connect();
	
  //playButton.enabled = false;
  //recordButton.enabled = false;

  
	if (Application.application.parameters.file) {
  	player1.url.text = Application.application.parameters.file;
	}
}


