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
  	player1.player_url.text = Application.application.parameters.file;
	}
	player2.player_url.text = "dragonball.mp4";
	
	Application.application.parameters.player1 = player1;
	Application.application.parameters.player2 = player2;
}

public function check() : void
{
/*	Alert.show("z:"+(Application.application.parameters.player1.stream._stream == Application.application.parameters.player2.stream._stream));*/
	Application.application.parameters.player2.videoContainer.video.visible = false;
}

