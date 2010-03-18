import flash.display.Sprite;  
import flash.net.NetConnection;
import flash.events.NetStatusEvent;
import flash.events.SecurityErrorEvent;
import flash.events.AsyncErrorEvent;
import flash.media.Video;
import flash.external.ExternalInterface;
import flash.text.TextField;
import mx.controls.Alert;
import mx.events.SliderEvent;
import mx.collections.ArrayCollection;
import flash.events.SyncEvent;

private var roomName:String = "testroom";	
private var room:SharedObject;
private var roomData:Object;
private var connection:NetConnection;
[Bindable]
private var canConnect:Boolean = true;
[Bindable]
public var peerList:Array = [];
	
public function init()  : void
{
	Security.allowDomain("*");
  System.useCodePage = true;
	
	if (Application.application.parameters.room) {
    roomName = Application.application.parameters.room;
	}
	connect();
}

public function drawPeers():void
{
  
}

private function connect():void 
{
  if (user_name.text == "") {
    return;
  }
  canConnect = false;
  connection = new NetConnection();
	connection.addEventListener(NetStatusEvent.NET_STATUS, onConnectionStatus);
	connection.objectEncoding = ObjectEncoding.AMF3;
	connection.connect(Application.application.parameters.server, "", user_name.text);
}


private function onConnectionStatus( event : NetStatusEvent ) : void
{
	switch(event.info.code){
		case "NetConnection.Connect.Success":
  	room = SharedObject.getRemote(roomName, connection.uri, false);
  	room.connect(connection);
  	room.addEventListener(SyncEvent.SYNC, function(evtObj:SyncEvent):void {
  	  Alert.show("SO sync");
      for (var key:Object in evtObj.changeList) {
        if (evtObj.changeList[key].name) {
          roomData[evtObj.changeList[key].name] = evtObj.changeList[key].code;
        }
      }
  	});

		var peers:Array = roomData.peers;
		if (!peers) {
		  peers = [];
		}
		peers.push(user_name.text);
		room.setProperty("peers", user_name.text);
		break;

	case "NetConnection.Message":
		try {
			var re:RegExp = /\\/g;
	    ExternalInterface.call("netconnection.message", event.info.description.replace(re, "\\\\"));
		}
		catch (e:Object) {
		}
    break;

  case "NetConnection.Connect.Failed":
    canConnect = true;
    break;

	}
}

